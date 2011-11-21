;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-


(in-package :de.setf.utility.implementation)

(document "This file defines an image walker for the metadata-based documentation module of
 the 'de.setf.utility' library."
 
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved"
  "'de.setf.utility' is free software: you can redistribute it and/or modify
 it under the terms of version 3 of the GNU Lesser General Public License as published by
 the Free Software Foundation.

 'de.setf.utility' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 See the GNU Lesser General Public License for more details.

 A copy of the GNU Lesser General Public License should be included with 'de.setf.utility, as `lgpl.txt`.
 If not, see the GNU [site](http://www.gnu.org/licenses/).")

 (long-description "This uses a general walking interface to implement image/source walking

 - walk-self
 - walk-successors
 - walk-predecessors

 The combines with ensure-definition to generate the definition model for a running image."))


(defmethod sg:walk-predcessors ((node function) (operator t))
  (sg:walk-predecessors (de.setf.utility.clos.graph:function-calls (function-name node))

(defgeneric document-as (subject destination form)
  (:documentation "Generate documentation for the given subject(s) in the given form(s), at the
 given destination. The subject can be an individual lisp object, a pathname, a system, or a package, or
 it can be an heterogeneous list of objects.")

  (:method (subject destination form)
    (with-definition-extent (subject)
      (ensure-definition subject)
      (encode-documentation subject destination form))))


(defgeneric definition-precedence (object)
  (:method ((object asdf:system)) 0)
  (:method ((object asdf:module)) 1)
  (:method ((object asdf:component)) 2)
  (:method ((object package)) 3)
  (:method ((object t)) 4))


(defgeneric compute-definition-extent (extent)
  (:documentation "Given a designator for a system or a package, or a list of such,
 compute a flattened list of all designated objects.")
  (:method ((extent list))
    (remove-duplicates (remove nil
                               (reduce #'append (mapcar #'compute-documentation-extent extent)))))

  (:method ((extent null)) nil)

  (:method ((object asdf:component)) (list object))

  (:method ((object function)) (list object))

  (:method ((object class)) (list object))

  (:method ((extent symbol))
    (remove nil (list* (when (fboundp extent (symbol-function extent)))
                       (find-class extent nil)
                       ;; other data?
                       (compute-definition-extent (symbol-name extent)))))

  (:method ((extent string))
    (or (remove nil (list (find-package extent) (asdf::system-registered-p extent)))
        (cerror "Ignore it." "Invalid extent '~a'."  extent))))

(defun call-with-definition-extent (op extent)
  (let ((*documentation-extent* (compute-definition-extent extent)))
    (funcall op)))

(defmacro with-definition-extent (extent &body body)
  `(flet ((documentation-op () ,@body))
     (declare (dynamic-extent #'documentation-op))
     (call-with-definition-extent #'documentation-op (list ,@extent))))




(defgeneric definition-component (designator)
  (:documentation "Given a component pathname, return the component. nb. This is ambiguous when a
 file is present in more than one system, but the functions should be in just one file (at a time) anyway.")

  (:method ((pathname logical-pathname))
    (definition-component (translate-logical-pathname pathname)))

  (:method ((pathname pathname))
    (gethash pathname *definition-components*)))

(defgeneric (setf definition-component) (component designator)
  (:documentation "Given a component pathname, return the component. nb. This is ambiguous when a
 file is present in more than one system, but the functions should be in just one file (at a time) anyway.")

  (:method (component (pathname logical-pathname))
    (setf (definition-component (translate-logical-pathname pathname)) component))

  (:method ((component asdf:source-file) (pathname pathname))
    (setf (gethash pathname *definition-components*) component)))



(document ensure-definition

  "Given an extent, traverse the dependency closure of component constituency, symbol visibility, and/or
 function calls/called-by references and construct the definition graph.

 - for asdf components, register the concrete component instance, traverse the constituents
 - for packages, construct a definition instance, traverse the symbols
 - for functions, construct a definition instance, traverse the calls/called-by reference.

 A single around method tracks updates to a given definition with a cycle in order to break
 circular references.")


(defgeneric ensure-definition (subject)
  (:documentation "Given a documentation subject - a package, system, file pathname, or defined object, compute and
 cache the definition objects:

 - package  : iterate over the symbols, as regulated by *documentation-visibility* and *documentation-categories*
   and update the respective metadata for found bindings.
 - system   : walk the system definition and update definitions for each source fie pathname.
 - pathname : read the definitions and update metadata from the source definitions
 - object   : extract the metadata from the respective function, package, etc.

 Each definition object is added to the respective file's system definition component
 and to the definition name's property list.")

  (:method ((subjects list))
    (mapc #'ensure-definition
          ;; operate 'top-down', larger extents first
          (sort (copy-list subjects) #'< :key #'definition-precedence)))

  (:method :around ((subject asdf:component))
    (unless (eq (definition-cycle subject) *definition-cycle*)
      (setf (definition-cycle subject) *definition-cycle*)
      (call-next-method)))

  (:method :around ((subject definition))
    (unless (eq (definition-cycle subject) *definition-cycle*)
      (setf (definition-cycle subject) *definition-cycle*)
      (call-next-method))))


(defmethod ensure-definition ((component asdf:module))
  "Given a system or module, register it and all constituents.
 This uses the asdf components as-is and stores any definitional fields in the component's
 property list. No local update is necessary. Just navigation."

  (setf (definition-component (asdf:component-pathname component)) component)
  (map nil #'ensure-definition (asdf:module-components component)))


(defmethod ensure-definition ((component asdf:component))   ; ignore it
  nil)


(defmethod ensure-definition ((component asdf:source-file))
  "Given a source file component, register it and any contained defintitions.
 This can appear as a consequence of an independent image-based definition or by extracting
 them from the component source file. This ensure step serves to update them."

  (setf (definition-component (asdf:component-pathname component)) component)
  (map nil #'ensure-definition (component-definitions component)))

(defmethod ensure-definition ((object generic-function))
  (let ((definition (or (find-definition component)
                        (create-definition component))))
    (map nil #'ensure-definition (generic-function-methods object))
    (update-definition definition)))



(defgeneric update-definition (definition &rest args)
  (:documentation "Use a combination of the definition's object's internal state and any passed
 arguments to update information cached in the definition instance."))


(defmethod update-definition ((definition asdf:component) &key)
    definition)


(defmethod update-definition ((definition package-definition) &key)
   "Given a package, iff package is included among the documentation dimensions,

iterate over the  symbols to extract the metadata for each.
 By default, just the external symbols are included. I

 then collect data on the package  itself as well."
  (declare (dynamic-extent args))
  (when (member 'package *documentation-categories*)
    (apply #'call-next-method object processor
           :type 'package
           :documentation (documentation object)
           args))
  (flet ((do-visibility (call-next)
           (loop (multiple-value-bind (next-p symbol) (funcall call-next)
                   (unless next-p (return))
                   (extract-definition symbol processor)))))
    (when (member :external *documentation-visibility*)
      (with-package-iterator (next object :external)
        (flet ((do-next () (next))) (do-visibility #'do-next))))
    (when (member :internal *documentation-visibility*)
      (with-package-iterator (next object :external)
        (flet ((do-next () (next))) (do-visibility #'do-next))))))
  

(defmethod update-definition ((definition generic-function-definition) &key
                              :lambda-list 
                              :method-combination (c2mop:generic-function-method-combination object)
                              :methods 
                              :pathname (definition-type-source-information object 'function)
  (map nil #'update-definition (generic-function-methods (definition-object definition)))
  definition)


(defmethod update-definition ((component method))
  (let ((definition (find-definition component)))
    (if definition
      (update-definition definition)
      (compute-definition object))))

(defmethod update-definition ((definition method-definition))
  (let ((function (c2mop:method-function (definition-object definition))))
  (map nil #'update-definition (generic-function-methods (definition-object definition)))
  definition)



  
  
(defmethod update-definition ((component function))
  
  )

(defmethod update-definition-model ((component package))
  )




;;;
(document document-as
  "This is the top-level interface and must handle three dimeninsions in the argument data

 - the object can be a single lisp object, for example, a function, or it can be a system, or even a list
 of systems. 
 - the destination can be a single stream, or it can be a directory to indicate the root of a
 file hierachy
 - the content type can be a single mime type or it can be a list of them. In the former case a single
 document is generated, which can be done to a single stream. In the latter case, given a stream
 each must be mime encoded.")


(defmethod document-as ((object t) (destination t) (as t))
  (encode-as object destination as))


(defmethod encode-as ((objects cons) (destination stream) (type mime:*/*))
  "Given a sequence of documentation entities, encode each in turn."

  (dolist (object objects)
    (encode-as object destination type)))


(defun clear-definition-model ()
  (clrhash *documentation-model*))
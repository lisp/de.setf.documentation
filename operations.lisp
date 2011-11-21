;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-


(in-package :de.setf.utility.implementation)

(:documentation "This file defines interface operators for the metadata-based documentation module of
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

 (long-description
  "The documentation interface comprises the operators

 - document-as (subject destination form)     : given a subject set, extract its metadata and generate documentation.
 - with-definition-extent (list . body) : sets the extent of cross references to some combination of objects,
   packages, and systems.
 - update-definition-model (subject)    : extracts documentation metadata from a specified source. it walks
   all objects in the scope, extracts their metadata and build a graph or related definitions.
 - encode-documentation (subject destination form) : generates documents to describe the subject.

 The subject set serves both as the root for building the definition graph and as the constraints on its scope.
 Arguments objects - functions, classes, or symbols, serve as the roots for walking the dependency graph.
 If containers are given - packages or system definition components, they constrain the extent of the walk to
 those objects named a given package and defined in some file which is present in some system.
 The form is a mime type. Three types are supported

 - text/xhtml  : generate an xhtml-encoded documentation tree. The destination can be a pathname, in which case
 the entire tree is generated. If the destination is a stream, given an extent set, only the root document is
 generated, while given an object, only the single object's documentation is generated.
 - text/vnd.graphviz : generate a `.dot` encoded dependency graph of the subject object. Given a
   - function : generate a call graph
   - source file : generate a system dpenendency graph for the single node
   - module : generate a system dependenecy graph for the module or system
   - package : generate a call graph with subgraphcs which correspond to definition source files.
 - image/svg :  generates a graphviz document and processes it to produce an svg-encoded image.

 The destination can be a pathname designator or a stream. If it is a pathname, the respective directory serves
 as the root for the generated documentation tree. If it is a stream, the document is written to the stream
 respective the mime-type"))



(defgeneric documentation-file-type (mime-type)
  (:method ((type mime:*/*))
    (dsu:mime-type-file-type type)))


(defgeneric definition-precedence (object)
  (:method ((object asdf:system)) 0)
  (:method ((object asdf:module)) 1)
  (:method ((object asdf:component)) 2)
  (:method ((object package)) 3)
  (:method ((object t)) 4))


(defgeneric compute-definition-extent (extent)
  (:documentation "Given a designator for a system or a package, or a list of such,
 compute a flattened list of all designated objects. THis is the most liberal extent, which
 attempts to resolve designators into all possible objects")
  
  (:method ((extent null)) nil)
  
  (:method ((extent list))
    (case (first extent)
      (setf (when (fboundp extent) (list (fdefinition extent))))
      (function (compute-definition-extent (second extent)))
      (t
       (remove-duplicates (remove nil (reduce #'append (mapcar #'compute-definition-extent extent)))))))
  
  (:method ((object asdf:component)) (list object))

  (:method ((object package)) (list object))
  
  (:method ((object function)) (list object))
  
  (:method ((object class)) (list object))
  
  (:method ((object pathname)) (list object))
  
  (:method ((extent symbol))
    (remove nil (list (when (fboundp extent) (symbol-function extent))
                      (find-class extent nil)
                      ;; other data?
                      (asdf:find-system extent nil)
                      (find-package extent))))
  
  (:method ((extent string))
    "The string method exercises asdf special-handling for strings: no case folding."
    (remove nil (list (find-package extent)
                      (find-system-prototype extent)
                      (asdf:find-system extent nil)
                      (let ((colon (position #\: extent))
                            (package nil)
                            (symbol nil))
                        (and colon
                             (setf package (find-package (string-upcase extent :end colon)))
                             (setf symbol (find-symbol (string-upcase extent :start (1+ colon)) package))
                             (compute-definition-extent symbol)))))))


(assert (null (set-difference (compute-definition-extent '(:de.setf.utility :cl-user))
                              (list (find-package :de.setf.utility) (asdf:find-system :de.setf.utility)
                                    (find-package :cl-user)))))


(defun compute-call-extent (extent)
  "Given a designator for a function or a package, or a list of such,
 compute a flattened list of all designated objects. This delegates to compute-definition-extent and
 filters to retain packages and functions only."

  (flet ((function-or-package-p (object)
           (or (functionp object) (packagep object))))
    (remove-if-not #'function-or-package-p (compute-definition-extent extent))))


(defun call-with-definition-extent (op extent)
  (funcall op (compute-definition-extent extent)))

(defmacro with-definition-extent ((variable &rest specifications) &body body)
  `(flet ((documentation-op (,variable) ,@body))
     (declare (dynamic-extent #'documentation-op))
     (call-with-definition-extent #'documentation-op (list ,@specifications))))


(defun document (subject destination)
  (document-as subject destination *documentation-form*))


(defgeneric document-as (subject destination form)
  (:documentation "Generate documentation for the given SUBJECT(s) in the given FORM(s), to a collection of
 files rooted at the given DESTINATION. The subject can be an individual lisp object, a pathname, a system,
 or a package, or it can be an heterogeneous list of objects.")

  (:method (subject (destination pathname) *documentation-form*)
    "use the generic image walker. Supply an operator to accept the arguments
 - (object) : the individual object
 - (object referent relation) : relation between an object and something else"
    
    (let ((packages ())
          (files ())
          (systems ())
          (*documentation-root* (merge-pathnames destination *documentation-root*)))
      (flet ((record-subject (subject &optional relation other)
               (declare (ignore relation other))
               (typecase subject
                 (package (pushnew subject packages))
                 (asdf:source-file (pushnew subject files))
                 (asdf:system (pushnew subject systems))
                 (t t)))
             (documentation-pathname (component)
               (etypecase component
                 (asdf:source-file
                  (let ((source-path (asdf:component-pathname component)))
                    (make-pathname :directory (append (pathname-directory *documentation-root*)
                                                      (rest (pathname-directory source-path)))
                                   :name (pathname-name source-path)
                                   :type (documentation-file-type *documentation-form*)
                                   :defaults destination)))
                 (asdf:system
                  (make-pathname :directory (pathname-directory *documentation-root*)
                                 :name (concatenate 'string "system_" (asdf:component-name component))
                                 :type (documentation-file-type *documentation-form*)
                                 :defaults destination))
                 (package
                  (make-pathname :directory (pathname-directory *documentation-root*)
                                 :name (concatenate 'string "package_" (package-name component))
                                 :type (documentation-file-type *documentation-form*)
                                 :defaults destination)))))
        (multiple-value-bind (*walker* extent)
                             (walk-definitions subject subject #'record-subject
                                               ;; exclude internal definitions
                                               ;; don't track the inter-function relations
                                               :excluded-qualifiers '(internal calls callers))
        (let ((*walker-cache* (make-hash-table :test 'equalp)))
          ;; build a shadow cache keyed by namestring to facilitate cross-references
          (maphash #'(lambda (k v)
                       (when (symbolp k)
                         (setf (gethash (string k) *walker-cache*) v))
                       (setf (gethash k *walker-cache*) v))
                   (walker-cache *walker*))
          (dolist (package packages)
            (setf.documentation:encode-documentation-as package (documentation-pathname package) *documentation-form*))
          (dolist (file files)
            (setf.documentation:encode-documentation-as file (documentation-pathname file) *documentation-form*))
          (print systems)
          (dolist (system systems)
            (setf.documentation:encode-documentation-as system (documentation-pathname system) *documentation-form*))
          (values *walker* subject extent *documentation-root*)))))))


(defgeneric setf.documentation:encode-documentation-as (subject destination form &rest args)
  (:documentation "Encode the subject's documentation in the given form.")
  (declare (dynamic-extent args))

  (:method ((subject t) (destination pathname) (form t) &rest args)
    (ensure-directories-exist destination)
    (let ((*documentation-base-url* (definition-documentation-uri destination)))
      ; (break "pathname and uri base: ~s ~s" destination *documentation-base-url*)
      (with-open-file (stream destination :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
        (apply #'setf.documentation:encode-documentation-as subject stream form args))
      destination)))

#+(or)
(defgeneric encode-documentation (subject destination form)
  (:method ((subject asdf:source-file) (destination (eql t)) (form t))
    (let* ((source-path (asdf:component-pathname subject)))
      (if source-path
        (setf.documentation:encode-documentation-as subject
                                                    (make-pathname :directory `(,(first (pathname-directory source-path))
                                                                                "documentation"
                                                                                ,@(rest (pathname-directory source-path)))
                                                                   :type (documentation-file-type form)
                                                                   :defaults source-path)
                                                    form)
        (warn "subject has no source-path: ~s." subject))))


  (:method ((subject asdf:source-file) (destination pathname) (form t)) (inspect subject)
    (let* ((source-path (asdf:component-pathname subject)))
      (if source-path
        (setf.documentation:encode-documentation-as subject
                            (make-pathname :directory `(,(or (first (pathname-directory source-path)) :absolute)
                                                        "documentation"
                                                        ,@(rest (pathname-directory source-path)))
                                           :type (documentation-file-type form)
                                           :name (pathname-name source-path)
                                           :defaults destination)
                            form)
        (warn "subject has no source-path: ~s." subject))))

  (:method ((subject package) (destination (eql t)) (form t))
    (let* ((source-path (dsw:object-source-information subject 'package)))
      (if source-path
        (setf.documentation:encode-documentation-as subject
                            (make-pathname :directory `(,(first (pathname-directory source-path))
                                                        "documentation"
                                                        ,@(rest (pathname-directory source-path)))
                                           :type (documentation-file-type form)
                                           :name (concatenate 'string "package_" (package-name subject))
                                           :defaults source-path)
                            form)
        (warn "subject has no source-path: ~s." subject))))

  (:method ((subject package) (destination pathname) (form t))
    (setf.documentation:encode-documentation-as subject
                        (make-pathname :directory `(,(or (first (pathname-directory destination)) :absolute)
                                                    "documentation"
                                                    ,@(rest (pathname-directory destination)))
                                       :type (documentation-file-type form)
                                       :name (concatenate 'string "package_" (package-name subject))
                                       :defaults destination)
                        form))

  (:method ((subject t) (destination pathname) (form t))
    (ensure-directories-exist destination)
    (with-open-file (stream destination :direction :output :if-exists :supersede
                            :if-does-not-exist :create)
      (setf.documentation:encode-documentation-as subject stream form))
    destination))




(:documentation ensure-definition

  "Given an extent, traverse the dependency closure of component constituency, symbol visibility, and/or
 function calls/called-by references and construct the definition graph.

 - for asdf components, register the concrete component instance, traverse the constituents
 - for packages, construct a definition instance, traverse the symbols
 - for functions, construct a definition instance, traverse the calls/called-by reference.

 A single around method tracks updates to a given definition with a cycle in order to break
 circular references.")


(defgeneric ensure-definition (subject context)
  (:method ((subject t) (context definition-cache))
     (find-definition subject context)
     (setf (find-definition subject context)
           (make-definition subject))))

(defgeneric dsw:find-definition (subject context)
  (:method ((subject t) (context definition-cache))
    (walker-node-property context subject :definition)))

(defgeneric (setf dsw:find-definition) (definition subject context)
  (:method (definition (subject t) (context definition-cache))
    ; (print (list :setf-find-definition subject definition))
    (setf (walker-node-property context subject :definition) definition)))














#|


(defgeneric definition-component (designator)
  (:documentation "Given a data object, return the definition component. nb. This allows ambiguous designators,
 for example, when a file is present in more than one system definition. In those cases, the eventual
 constituents should be unique in any case, but they might be in/excluded from an extent incorrectly due to the
 navigation path.")

  (:method ((pathname logical-pathname))
    (definition-component (translate-logical-pathname pathname)))

  (:method ((object t))
    (gethash object *definition-components*))

  (:method ((definition asdf:component))
    definition))


(defgeneric (setf definition-component) (component designator)
  (:documentation "Given a component pathname, return the component. nb. This is ambiguous when a
 file is present in more than one system, but the functions should be in just one file (at a time) anyway.")

  (:method (component (pathname logical-pathname))
    (setf (definition-component (translate-logical-pathname pathname)) component))

  (:method ((component asdf:component) (pathname pathname))
    (setf (gethash pathname *definition-components*) component))

  (:method ((definition definition) name)
    (setf (gethash name *definition-components*) definition)))

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



;;;
;;; asdf:component and specializations

(defmethod ensure-definition ((component asdf:component))
  "Given a system definitioncomponent, register it.
 This uses the asdf components as-is and stores any definitional fields in the component's
 property list. No immediate update is necessary."

  (setf (definition-component (asdf:component-pathname component)) component))


;; record relations in terms for "resoures"
(defmethod ensure-definition-relation ((component asdf:module) (requirement asdf:module) (relation (eql 'depends-on)))
   (add-triple component 'depends-on requirement))

(defmethod ensure-definition-relation ((component asdf:file) (requirement asdf:file) (relation (eql 'depends-on)))
   (add-triple component 'depends-on requirement))

(defmethod ensure-definition-relation ((component asdf:file) (requirement asdf:module) (relation (eql 'depends-on)))
  (add-triple component 'depends-on requirement))

(defmethod ensure-definition-relation ((component asdf:module) (consistituent asdf:file) (relation (eql 'depends-on)))
  (add-triple component 'depends-on constituent))

(defmethod ensure-definition-relation ((component asdf:module) (consistituent asdf:module) (relation (eql 'component)))
  (add-triple component 'component constituent))


(defmethod ensure-definition-relation ((component asdf:source-file) (definition definition) (relation (eql 'contains)))
  "Given a source file component, register its contained defintition.
 This can appear as a consequence of an independent image-based definition or by extracting
 them from the component source file."

  (add-triple component 'contains definition))


(defmethod ensure-definition ((object t))
  (or (find-definition component)
      (setf (find-definition component) (make-definition component))))


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

(defmethod build-definition-model ((component package) &optional related relation)
  
  )




;;;
(:documentation document-as
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

|#
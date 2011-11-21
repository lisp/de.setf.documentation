;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-


(in-package :de.setf.utility.implementation)

(document :file
 (description "This file defines image metadata extraction operators for the documentation module of
 the 'de.setf.utility' library.")
 
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
  "The process to extract metadata for documentation proceeds in two phases. First traverse 
 packages, examine all bindings for each symbol, extract the respective metadata and cache it.
 Once a collection exists, filter all documentation strings to transform them into term lists which
 contain cross references. The actual document generation is part of a larger process."))


(defun definition-type-source-information (object type-specification)
  (warn "use dsw:object-source-information.")
  (dsw:object-source-information object type-specification))

;;;
;;; extraction

(defmethod extract-definition ((object package) (processor t) &rest args)
  "Given a package, iterate over the  symbols to extract the metadata for each.
 By default, just the external symbols are included. Iff package is included among the documentation dimensions,
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
      (with-package-iterator (next object :internal)
        (flet ((do-next () (next))) (do-visibility #'do-next))))))


(defmethod extract-definition ((object standard-object) (processor t)
                               &rest args &key
                               (name (definition-name object))
                               (type (type-of object)))
  (apply #'extract-definition object processor
         :type type
         :name name
         args))


(defmethod extract-definition ((object class) (processor t) &rest args &key
                               (category 'class))
  (declare (dynamic-extent args))
  (apply #'call-next-method object processor
         :value object
         :superclasses (c2mop:class-direct-superclasses object)
         :subclasses (c2mop:class-direct-subclasses object)
         :slots (c2mop:class-slots object)
         :documentation (documentation object 'type)
         :pathname (definition-type-source-information object 'class)
         :category category 
         args))


(defmethod extract-definition ((object function) (processor t) &rest args &key
                               (category 'function))
  (declare (dynamic-extent args))
  (apply #'call-next-method object processor
         :value object
         :lambda-list (second (function-lambda-expression object))      ; iff definitions were saved
         :pathname (definition-type-source-information object 'function)
         :category category
         args))


(defmethod extract-definition ((object generic-function) (processor t) &rest args &key
                               (category 'function))
  (declare (dynamic-extent args))
  (apply #'call-next-method object processor
         :value object
         :category category
         args))


(defmethod extract-definition ((object method) (processor t) &rest args &key
                               (category 'method))
  (declare (dynamic-extent args))
  (apply #'call-next-method object processor
         :value object
         :lambda-list (c2mop:method-lambda-list object)
         :specializers (c2mop:method-specializers object)
         :qualifiers (method-qualifiers object)
         :category category
         args))


(defmethod extract-definition ((object symbol) (processor t) &rest args)
  "Extract documentation attributes for a symbol. this includes definitions as all of
 class, function, setf (function and otherwise), special-form, type, and variable.
 where the definition is a specialized value (as a function) continue with a new extraction
 process, otherwise pass control on to the mediation phase."

  (when (member 'class *documentation-categories*)
    (let ((class (find-class object nil)))
      (when class
        (apply #'extract-definition class processor
               :category 'class
               args))))
          
  (when (and (intersection '(function macro) *documentation-categories*) (fboundp object))
    (let ((category (function-information object)))
      (ecase category
        (:function
         (apply #'extract-definition (fdefinition object) processor
                :category 'function
                :documentation (documentation object 'function)
                args))
        ((:macro :special-form)
         (apply #'call-next-method object processor
                :category 'macro
                :documentation (documentation object 'function)
                args))))
    (let ((setf-object `(setf ,object)))
      (when (fboundp setf-object)
        (apply #'extract-definition (fdefinition setf-object) processor
                 :category 'setf
                 :documentation (documentation setf-object 'setf)
                 args))))
  
  (when (member 'method-combination *documentation-categories*)
    (let ((documentation (documentation object 'method-combination)))
      (when documentation
        (apply #'call-next-method object processor
               :category 'method-combination
               :documentation documentation
               args))))
  
  (when (and (member 'type *documentation-categories*)
             (not (find-class object nil)))
    (let ((documentation (documentation object 'type)))
      (when documentation
        (apply #'call-next-method object processor
               :category 'type
               :documentation documentation
               args))))
  
  (when (and (member 'variable *documentation-categories*)
             (boundp object))
    (let ((category (variable-information object)))
      (ecase category
        ((:lexical :special :constant)
         (apply #'call-next-method object processor
                :value (symbol-value object)
                :category 'variable
                args))
        (:symbol-macro
         (apply #'call-next-method object processor
                :category 'macro
                args))))))




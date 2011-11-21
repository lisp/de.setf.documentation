;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-

(in-package :de.setf.utility.implementation)

(:documentation  "This file defines metadata extraction utilities for the documentation module of
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
  "System analyses are based on a metadata model which is compiled through introspective
 analysis and collated with the asdf system definition. The process to extract metadata is implemented
 by the definition-walker class, which extends the basic image-walker with system-walker - for
 asdf system definitions, and source-walker - for source code files.
 The combined qualifiers effect a traversal process which extracts definition metadata from

 - system definition components : asdf:system, asdf:module, and asdf:file
 - source files
 - packages : all symbols with bindings as indicated by 
 - bound definition objects : functions, macros, and classes

 The metadata includes both that implicit in the definition - for example, the source file for a
 function definition, or its cross-references to called functions, and that described in documentation
 strings. The former is extracted by traversing packages, examining all bindings for each symbol,
 to extracte and cache the metadata from respective bound objects. Once such a collection exists, filter
 all documentation strings to identify cross-references to be used for generating system documentation."))


(let ((metadata-directory-pathname (merge-pathnames (make-pathname :directory '(:relative :up "metadata"))
                                                (truename #p"LIBRARY:")))
      (upload-directory-pathname (merge-pathnames (make-pathname :directory '(:relative :up "upload"))
                                                  (truename #p"LIBRARY:"))))
  (assert (probe-file metadata-directory-pathname) ()
          "No metadata directory found: ~s." metadata-directory-pathname)
  (assert (probe-file upload-directory-pathname) ()
          "No upload directory found: ~s." upload-directory-pathname)
  (set-relative-logical-pathname-translations "METADATA" :absolute-pathname metadata-directory-pathname)
  (set-relative-logical-pathname-translations "UPLOAD" :absolute-pathname upload-directory-pathname))


(defun compute-metadata-directory (pathname)
  "Translate a pathname from its location in the source library tree to the analogous
 location in the mestadat tree."

  (let* ((relative-directory (nthcdr (length (pathname-directory (translate-logical-pathname #p"LIBRARY:")))
                                     (pathname-directory (translate-logical-pathname pathname)))))
    (make-pathname :directory `(:absolute ,@relative-directory) :name nil :type nil :host "METADATA")))


(macrolet ((def-component-property (function-name (variable class) &optional documentation)
             `(progn (defmethod ,function-name ((,variable ,class))
                       ,@(when documentation (list documentation))
                       (asdf:component-property definition ',function-name))
                     (defmethod (setf ,function-name) (value (,variable ,class))
                       ,@(when documentation (list documentation))
                       (setf (asdf:component-property definition ',function-name) value)
                       ;; asdf:component-property remains broken
                       value))))
  
  (def-component-property definition-pathname (definition asdf:system)
    "The pathname of the system definition itself. If the system was loaded as a prototype, this is
 the pathname from which it was loaded. Otherwise, it caches to the path which asdf re-searches with
 system-definition-pathname.")

  (def-component-property definition-form (definition asdf:system))

  (def-component-property system-repository-url (definition asdf:system))

  (def-component-property system-repository-type (definition asdf:system))

  (def-component-property definition-source-directory (definition asdf:system)
    "The directory in which to place the system's source files. By default this is the same directory
 as that of the system definition itself.")

  (def-component-property definition-metadata-directory (definition asdf:system)
    "The directory in which to place graphs, run-time images, and other side-effects of analyzing the
 system. The default location mirros the source graph under the logical 'METADATA' host.")

  (def-component-property component-definitions (definition asdf:component)))

(defmethod (setf definition-pathname) ((erroneous null) (definition asdf:system))
  (error "invalid pathname for system definition: ~s." definition))

(defmethod definition-pathname :around ((definition asdf:system))
  "If there is no cached value, delegate to the asdf search process and cache the result."
  (or (call-next-method)
      (let ((asdf-version (or (asdf:system-source-file definition)
                              (asdf:system-definition-pathname (asdf:component-name definition)))))
        (when asdf-version
          (setf (definition-pathname definition) asdf-version)))))

(defmethod definition-source-directory :around ((definition asdf:system))
  (or (call-next-method)
      (let ((pathname (definition-pathname definition)))
        (when pathname (make-pathname :name nil :type nil :defaults pathname)))))

(defmethod definition-metadata-directory :around ((definition asdf:system))
  (or (call-next-method)
      (compute-metadata-directory (definition-source-directory definition))))


(defmethod (setf asdf:component-parent) (parent (component asdf:component))
  (setf (slot-value component 'asdf::parent) parent))

(defgeneric definition-name (object)
  (:documentation "return the name of a definition instance or - where possible a referenced
 object. if not possibe, return nil.")
  (:method ((object asdf:component)) (asdf:component-name object))
  (:method ((object class)) (class-name object))
  (:method ((object function)) (dsw:function-name object))
  (:method ((object generic-function)) (generic-function-name object))
  (:method ((object method)) (generic-function-name (c2mop:method-generic-function object)))
  (:method ((object package)) (package-name object))
  (:method ((object t)) nil))


(defgeneric specializer-name (specializer)
  (:method ((specializer symbol)) specializer)
  (:method ((specializer class)) (class-name specializer))
  (:method ((specializer cons)) specializer)
  (:method ((specializer c2mop:eql-specializer)) (ccl:eql-specializer-object specializer)))


(defgeneric definition-identifier (object)
  (:documentation "Return an internal id unique to the object. For a method this involves all of the
 function name, specializers, and qualifiers.")

  (:method ((object t))
    (definition-name object))

  (:method ((object method))
    `(,(definition-name object)
      ,@(c2mop:method-specializers object)
      ,@(mapcar #'specializer-name (c2mop:method-specializers object))))

  (:method ((object asdf:component))
    (asdf:component-pathname object)))



;;;

(defclass definition-component (asdf:component)
  ((object
    :initarg :object
    :reader definition-object)
   (pathname
    :initform nil :initarg :pathname
    :reader definition-pathname)
   (start
    :initform nil :initarg :start
    :reader definition-start)
   (end
    :initform nil :initarg :end
    :reader definition-end)
   (category
    :initform (error "category required") :initarg :category
    :reader definition-category)
   (string
    :initform nil :initarg :string
    :reader definition-string)
   (form
    :initform nil :initarg :form
    :accessor definition-form)
   (documentation
    :initarg :documentation
    :writer (setf definition-documentation))
   (cycle
    :initform nil
    :accessor definition-cycle
    :documentation "Binds a marker to control update navigation."))
  (:documentation
   "The definition metadata class extends the component class to collects attributes of a definition for
 runtime analysis and documentation. The abstract class binds just the name and location. Each concrete
 specialization adds the attributes required for the respective form. In addition to the
 introspective data, it also capture the source information - the pathname, the location in the file,
 and the form"))


(defMethod print-object ((object definition-component) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~@[(~a) ~]"
            (definition-name object))))

(defmethod definition-name ((definition definition-component))
  (if (slot-boundp definition 'asdf::name)
    (slot-value definition 'asdf::name)
    (setf (slot-value definition 'asdf::name)
          (definition-form-name definition (definition-form definition)))))


(defclass abstract-class-definition (definition-component)
  ())

(defclass class-definition (abstract-class-definition)
  ((category :initform 'class :allocation :class)))

(defclass condition-definition (abstract-class-definition)
  ((category :initform 'condition :allocation :class)))

(defclass built-in-class-definition (abstract-class-definition)
  ((category :initform 'built-in-class :allocation :class)))

(defclass structure-definition (class-definition)
  ((category :initform 'structure :allocation :class)))

(defclass method-combination-definition (definition-component)
  ())

(defclass operator-definition (definition-component)
  ())

(defclass abstract-function-definition (operator-definition)
  ((lambda-list
    :initform (error "lambda-list required")
    :initarg :lambda-list
    :accessor definition-lambda-list)))

(defclass function-definition (abstract-function-definition)
  ((category :initform 'function :allocation :class)))

(defclass generic-function-definition (abstract-function-definition)
  ((category :initform 'generic-function :allocation :class)))

(defclass setf-definition (operator-definition)
  ())

(defclass abstract-macro-definition (operator-definition)
  ())

(defclass macro-definition (abstract-macro-definition)
  ((category :initform 'macro :allocation :class)))

(defclass compiler-macro-definition (abstract-macro-definition)
  ((category :initform 'compiler-macro :allocation :class)))

(defclass modify-macro-definition (abstract-macro-definition)
  ((category :initform 'modify-macro :allocation :class)))

(defclass setf-expander-definition (abstract-macro-definition)
  ((category :initform 'setf-expander-macro :allocation :class)))

(defclass symbol-macro-definition (abstract-macro-definition)
  ((category :initform 'symbol-macro :allocation :class)))


(defclass method-definition (operator-definition)
  ((category :initform 'method :allocation :class)))

(defclass package-definition (definition-component)
  ((category :initform 'package :allocation :class)))

(defclass type-definition (definition-definition)
  ((category :initform 'type :allocation :class)))

(defclass abstract-variable-definition (definition-component)
  ((category :initform 'variable :allocation :class)))

(defclass parameter-definition (abstract-variable-definition)
  ())

(defclass constant-definition (abstract-variable-definition)
  ())

(defclass variable-definition (abstract-variable-definition)
  ())


(defgeneric definition-p (object)
  (:method ((object definition-component)) t)
  (:method ((objet t)) nil))

(def-class-constructors
  class-definition
  condition-definition
  built-in-class-definition
  structure-definition
  method-combination-definition
  function-definition
  generic-function-definition
  macro-definition
  compiler-macro-definition
  modify-macro-definition
  setf-definition
  setf-expander-definition
  symbol-macro-definition
  method-definition
  package-definition
  type-definition
  parameter-definition
  constant-definition
  variable-definition
  )


(defgeneric definition-class-of (instance)
  (:documentation
   "determine the definition class for a given datum or definition form. in general a symbol denotes a variable binding, except that the standard macro operators yield the registered definition class for the respective defined object. this, in turn, means that the definition class for an s-expression is that class computed for its operator.")

  (:method ((object built-in-class)) (find-class *class.built-in-class-definition*))
  (:method ((object function)) (find-class *class.function-definition*))
  (:method ((object generic-function)) (find-class *class.generic-function-definition*))
  (:method ((object method)) (find-class *class.method-definition*))
  (:method ((object method-combination)) (find-class *class.method-combination-definition*))
  (:method ((object package)) (find-class *class.package-definition*))
  (:method ((object standard-class)) (find-class *class.class-definition*))
  (:method ((object funcallable-standard-class)) (find-class *class.class-definition*))
  (:method ((object structure-class)) (find-class *class.structure-definition*))
  (:method ((operator (eql 'defclass))) (find-class *class.class-definition*))
  (:method ((operator (eql 'defconstant))) (find-class *class.constant-definition*))
  (:method ((operator (eql 'defgeneric))) (find-class *class.generic-function-definition*))
  (:method ((operator (eql 'define-compiler-macro))) (find-class *class.compiler-macro-definition*))
  (:method ((operator (eql 'define-condition))) (find-class *class.condition-definition*))
  (:method ((operator (eql 'define-method-combination))) (find-class *class.method-combination-definition*))
  (:method ((operator (eql 'define-modify-macro))) (find-class *class.modify-macro-definition*))
  (:method ((operator (eql 'define-setf-expander))) (find-class *class.setf-expander-definition*))
  (:method ((operator (eql 'define-symbol-macro))) (find-class *class.symbol-macro-definition*))
  (:method ((operator (eql 'defmacro))) (find-class *class.macro-definition*))
  (:method ((operator (eql 'defmethod))) (find-class *class.method-definition*))
  (:method ((operator (eql 'defpackage))) (find-class *class.package-definition*))
  (:method ((operator (eql 'defparameter))) (find-class *class.parameter-definition*))
  (:method ((operator (eql 'defsetf))) (find-class *class.setf-definition*))
  (:method ((operator (eql 'defstruct))) (find-class *class.structure-definition*))
  (:method ((operator (eql 'defun))) (find-class *class.function-definition*))
  (:method ((operator (eql 'defvar))) (find-class *class.variable-definition*)))


(defgeneric make-definition (object &rest initargs)
  (declare (dynamic-extent initargs))

  (:method ((object t) &rest initargs &key ((:object object-key) object))
    (declare (dynamic-extent initargs))
    (apply #'make-instance (definition-class-of object)
           :object object-key
           initargs))

  (:method ((definition asdf:component) &key)
    "Given a sysem definition component, return it as is is self-defining"
    definition))


(defGeneric make-definition-as (datum type)
  (:method ((designator symbol) (type (eql 'package)))
           (make-definition-as (string designator) type))
  (:method ((designator string) (type (eql 'package)))
           (make-definition (find-package designator)))
  (:method ((package package) (type (eql 'package)))
           (make-instance *class.package-definition* :object package :name (package-name package)
                          :documentation (documentation package 'package)))
  (:method ((name symbol) (type (eql 'class)) &aux class)
           (when (typep (find-class name nil) 'standard-class)
             (make-definition class :name name)))
  (:method ((name symbol) (type (eql 'function)))
           (when (fboundp name)
             (make-definition (symbol-function name) :name name :documentation (documentation name 'function))))
  (:method ((name symbol) (type (eql 'macro)))
           (when (macro-function name)
             (make-instance *class.macro-definition* :name name :documentation (documentation name 'macro))))
  (:method ((name symbol) (type (eql 'variable)))
           (when (boundp name)
             (make-instance *class.parameter-definition* :object (symbol-value name) :class :name name
                            :documentation (documentation name 'variable))))
  (:method ((name symbol) (type (eql 'structure)) &aux class)
           (when (typep (setf class (find-class name nil)) 'structure-class)
             (make-instance *class.structure-definition* :object class :name name
                            :documentation (documentation name 'structure))))                          
  (:method ((name symbol) (type (eql 'type)))
           (when (ignore-errors (typep nil name) t)
             (make-instance *class.type-definition* :name name))))


;;;
;;;
;;;

(defgeneric definition-package (definition)
  (:method ((definition definition-component))
    (let ((name (definition-name definition)))
      (or (typecase name
            (symbol (symbol-package name))
            (string (find-package name))
            (cons (symbol-package (second name))))
          *package*))))
      
(defgeneric definition-documentation (definition) 
  (:method ((definition definition-component))
    (if (slot-boundp definition 'documentation)
      (slot-value definition 'documentation)
      (setf (definition-documentation definition)
            (or (documentation (definition-object definition) t)
                (definition-form-documentation definition (definition-form definition)))))))

(defgeneric definition-annotated-documentation (definition)
  (:method ((definition definition-component))
    (or (let ((docstring (definition-documentation definition))
              (*package* (definition-package definition)))
          (when docstring
            (setf (asdf:component-property definition 'annotated-documentation)
                  (annotate-documentation docstring))))))

  (:method ((documentation string))
    (annotate-documentation documentation))

  (:method ((documentation null))
    ""))


(defgeneric documentation-rooted-pathname (definition)
  (:method ((location pathname))
    "Given a pathname, if it is already rooted, return it unchanged.
     otherwise, hang it onto the root."
    (let ((root-directory (pathname-directory *documentation-root*))
          (location-directory (pathname-directory location)))
      (if (and (>= (length location-directory) (length root-directory))
               (equalp root-directory (subseq location-directory 0 (length root-directory))))
        location
        (make-pathname :directory (append root-directory (cdr location-directory))
                       :name (pathname-name location)
                       :type (pathname-type location)
                       :defaults *documentation-root*))))

  (:method ((definition definition-component))
    "compute the enough-pathname for the parent file-component relative to the
     system and add an anchor"
    (let* ((parent (asdf:component-parent definition))
           (path (and parent (asdf:component-pathname parent))))
      (when path
        (documentation-rooted-pathname path)))))


(defmethod definition-documentation-uri ((definition definition-component))
  (let* ((parent (asdf:component-parent definition))
         (location (and parent (asdf:component-pathname parent))))
    (if location
      (let* ((rooted-location (documentation-rooted-pathname location))
             (physical-directory (pathname-directory (translate-logical-pathname rooted-location))))
        (format nil "~:[~;/~]~{~a/~}~a.~a#~a"
                (eq (first physical-directory) :absolute)
                #+digitool (cddr physical-directory)
                #-digitool (rest physical-directory)
                (pathname-name location)
                (documentation-file-type *documentation-form*)
                (definition-name definition)))
      (format nil "#~a" (definition-name definition)))))


(defmethod definition-documentation-uri ((location pathname))
  (let* ((rooted-location (documentation-rooted-pathname location))
         (physical-directory (pathname-directory (translate-logical-pathname rooted-location))))
    (format nil "~:[~;/~]~{~a/~}~a"
            (eq (first physical-directory) :absolute)
            #+digitool (cddr physical-directory)
            #-digitool (rest physical-directory)
            (file-namestring location))))

(defmethod definition-documentation-uri ((component asdf:source-file))
  (let ((location (asdf:component-pathname component)))
    (if location
      (let* ((rooted-location (documentation-rooted-pathname location))
             (physical-directory (pathname-directory (translate-logical-pathname rooted-location))))
        (format nil "~:[~;/~]~{~a/~}~a.~a"
                (eq (first physical-directory) :absolute)
                #+digitool (cddr physical-directory)
                #-digitool (rest physical-directory)
                (pathname-name location)
                (documentation-file-type *documentation-form*)))
      "")))

(defgeneric definition-documentation-relative-url (location &optional base)

  (:method ((location t) &optional (base *documentation-base-url*))
    (definition-documentation-relative-url (definition-documentation-uri location) base))

  (:method ((location string) &optional (base *documentation-base-url*))
    (if base
      (if (and (> (length location) (length base))
               (string= location base :end1 (length base))
               (eql (char location (length base)) #\#))
        (subseq location (length base))
        (let ((base-components (split-string base "/"))
              (location-components (split-string location "/")))
          ;; remove the shared path
          (loop (unless (and base-components location-components) (return))
                (unless (equal (first base-components) (first location-components)) (return))
                (pop base-components)
                (pop location-components))
          (cond ((and (null base-components) (null location-components))
                 "")
                ((null base-components)
                 (format nil "~{~a~^/~}" location-components))
                ((null location-components)
                 (format nil "~{~a~^/~}" (make-list (length base-components) :initial-element "..")))
                (t
                 (format nil "~{~a/~}~{~a~^/~}"
                         (make-list (1- (length base-components)) :initial-element "..")
                         location-components)))))
      location)))

;;; (definition-documentation-relative-url "/dev/test/file1.html#fragment" "/dev/test/index.html")
;;; (definition-documentation-relative-url "/dev/dir1/file1.html#fragment" "/dev/dir2/index.html")
;;; (definition-documentation-relative-url "/root2/file1.html#fragment" "/dev/test/index.html")
;;; (definition-documentation-relative-url "/dev/test/file1.html#fragment" "/dev/test/file1.html")
  


         
(defgeneric definition-form-documentation (definition form)
  (:method ((definition definition-component) (body list))
    (dolist (form body)
      (typecase form
        (string (return form))
        (cons (unless (or (eq (first form) 'declare) (keywordp (first form)))
                (return))))))

  (:method ((definition class-definition) (form list))
    (second (assoc :documentation (nthcdr 4 form))))

  (:method ((definition function-definition) (form list))
    (call-next-method definition (cdddr form)))

  (:method ((definition generic-function-definition) (form list))
    (second (assoc :documentation (cdddr form))))

  (:method ((definition abstract-variable-definition) (form list))
    (fourth form))

  (:method ((definition method-definition) (form list))
    (call-next-method definition (rest (member-if #'listp form)))))


(defgeneric definition-form-name (definition form)
  (:method ((definition definition-component) (form list))
    (second form)))
  

(defgeneric definition-setf-p (definition)
  (:method ((definition definition-component))
    nil)

  (:method ((definition abstract-function-definition))
    (let ((name (definition-name definition)))
      (and (consp name) (eq (first name) 'setf)))))


(defgeneric definition-qualifiers (definition)
  (:method ((definition definition-component)) nil)
  (:method ((definition method-definition))
    (if (definition-object definition)
      (method-qualifiers (definition-object definition))
      (ldiff (cddr (definition-form definition))
             (member-if #'listp (cddr (definition-form definition)))))))

(defgeneric definition-specializers (definition)
  (:method ((definition definition-component)) nil)
  (:method ((definition method-definition))
    (if (definition-object definition)
      (c2mop:method-specializers (definition-object definition))
      (mapcar #'(lambda (parameter)
                  (when (consp parameter) (setf parameter (second parameter)))
                  (etypecase parameter
                    (symbol (find-class parameter))
                    (cons parameter)))
              (ldiff (definition-lambda-list definition)
                     (member-if #'(lambda (parameter) (member parameter lambda-list-keywords))
                               (definition-lambda-list definition)))))))


(defGeneric definition-lambda-list (definition)
  (:method ((definition definition-component)) nil)

  (:method ((definition function-definition))
    (cond ((definition-form definition)
           (third (definition-form definition)))
          ((definition-object definition)
           (third (setf (definition-form definition)
                        (function-lambda-expression (definition-object definition)))))))
                   
  (:method ((definition method-definition))
    (cond ((definition-form definition)
           (find-if #'listp (cddr (definition-form definition))))
          ((definition-object definition)
           (c2mop:method-lambda-list (definition-object definition))))))


(defGeneric definition-method-combination (definition)
  (:method ((definition definition-component)) nil)
  (:method ((definition generic-function-definition))
           (if (slot-boundp definition 'method-combination)
             (slot-value definition 'method-combination)
             (setf (slot-value definition 'method-combination)
                   (or (second (assoc :method-combination (nthcdr (definition-form definition) 4)))
                       'standard-method-combination)))))

(defun find-method-by-type-names (function qualifiers specializer-types)
  (flet ((method-match-p (method)
           (and (equal (method-qualifiers method) qualifiers)
                (block test
                  (map nil #'(lambda (specializer type)
                               (unless (etypecase specializer
                                         (class (eq (class-name specializer) type))
                                         (cons (equalp specializer type)))
                                 (return-from test nil)))
                       (method-specializers method) specializer-types)
                  t))))
    (find-if #'method-match-p (generic-function-methods function))))


(defGeneric definition-object (definition)
  (:documentation
   "return the respective cached datum. if none is present attempt to locate it by name.")
  (:method :around ((definition definition-component))
           "check for a cached object. if present, return it. otherwise delegate to the specialized primary method. don't bind the value here: leave that to the primary method's discretion."
           (if (slot-boundp definition 'object)
             (slot-value definition 'object)
             (call-next-method)))
  (:method ((definition definition-component))
           (setf (slot-value definition 'object) nil))
  (:method ((definition class-definition))
           (let* ((class (find-class (definition-name definition) nil)))
             (when class (setf (slot-value definition 'object) class))))
  (:method ((definition function-definition))
           (let ((name (definition-name definition)))
             (let* ((designator (if (definition-setf-p definition) (list 'setf name) name))
                    (function (when (fboundp designator) (fdefinition designator))))
               (when function (setf (slot-value definition 'object) function)))))
  (:method ((definition abstract-variable-definition))
           (let ((name (definition-name definition)))
             (when (boundp name)
               (setf (slot-value definition 'object) (symbol-value name)))))
  (:method ((definition method-definition))
            (let* ((name (definition-name definition))
                   (specializers (definition-specializers definition))
                   (qualifiers (definition-qualifiers definition))
                   (designator (if (definition-setf-p definition) (list 'setf name) name))
                   (function (when (fboundp designator) (fdefinition designator)))
                   (method nil))
              (when (and (typep function 'generic-function)
                          (setf method (find-method function qualifiers specializers)))
                 (setf (slot-value definition 'object) method))))
  (:method ((definition package-definition))
           (let ((package (find-package (definition-name definition))))
             (when package (setf (slot-value definition 'object) package))))
  (:method ((definition type-definition))
           (setf (slot-value definition 'object) (definition-name definition)))
  (:method ((definition macro-definition))
           (let ((function (macro-function (definition-name definition))))
             (when function (setf (slot-value definition 'object) function))))
  (:method ((definition method-combination-definition))
           (let ((combination (find-method-combination (definition-name definition)
                                                       nil
                                                       nil)))
             (when combination
               (setf (slot-value definition 'object) combination))))
  (:method ((definition setf-definition))
           (setf (slot-value definition 'object) nil))
  (:method ((definition setf-expander-definition))
           (setf (slot-value definition 'object) nil)))
           

  
#+(or )
(defgeneric definition-signature (definition)
  (:method-combination list  :most-specific-last)
  (:method list ((definition definition-component)) (definition-name definition))
  (:method list ((definition function-definition))
           (definition-lambda-list definition)))


(defgeneric definition-url (definition)
  (:documentation "Return the resource locator for the defintion's documentation.
 This is a relative url with the location of the definition's file's system taken as the root.")
  )


;;;
;;;
;;; definition methods

(defun documentation-definition (word type)
  (let ((symbol (when-symbol *package* word)))
    (when symbol (get symbol type))))

#+(or )
(defgeneric bind-definition (definition)
  (:method ((definition t)) nil)
  (:method ((definition definition-component))
           (let ((name (definition-name definition)))
             (typecase name
               (null )
               (symbol (setf (get name (type-of definition)) definition))
               (cons (setf name (second name)
                           name (intern (concatenate 'string "SETF::" (string name)) (symbol-package name)))
                     (setf (get name (type-of definition)) definition))))))

(defmethod initialize-instance :around ((instance definition-component) &rest initargs &key form string)
  "initialize an definition based on either an s-expression form or a text string.
 where a form is present, string and buffer arguments are ignored.
 where no form is present, it is is read from the text string argument,
 which itself first be extract from an argument buffer."

  (when (and (null form) (stringp string))
    (setf form
          (handler-case (read-from-string string)
            (error (condition)
                   (destructuring-bind (&key start pathname &allow-other-keys) initargs
                     (warn "reading from text expression ~:[~s~;(~s @~d)~] signaled: ~a"
                           pathname (if pathname (list pathname start) string) condition)
                     nil)))))

  (if (and (consp form) (eq (type-of instance) 'definition))
    ;; if the instance is generic, but a form is supplied, then specialize it
    (let* ((class (definition-class-of (first form))))
      (cond ((null class)
             (cerror "Continue with abstract definition."
                     "No definition class defined: ~s." (first form))
             (apply #'call-next-method instance :form form initargs))
            (t                            ; restart the initialization for the more precise class
             (apply #'initialize-instance (change-class instance class) :form form initargs))))
    (call-next-method)))

(defmethod initialize-instance ((instance definition-component) &rest initargs
                                &key object (name (definition-name object)))
  (apply #'call-next-method instance
         :name name
         initargs))

;;;
;;; function-definition methods


(defmethod initialize-instance ((instance abstract-function-definition) &rest initargs
                                &key form object
                                (lambda-list
                                 (dsw:function-lambda-list (or object form
                                                               (error "lambda-list required.")))))
  (apply #'call-next-method instance
         :lambda-list lambda-list
         initargs))


;;;
;;; generic-function-definition methods

(defmethod definition-lambda-list ((definition generic-function-definition))
  (c2mop:generic-function-lambda-list (definition-object definition)))
(defmethod definition-method-combination ((definition generic-function-definition))
  (c2mop:generic-function-method-combination (definition-object definition)))
(defmethod definition-methods ((definition generic-function-definition))
  (c2mop:generic-function-methods (definition-object definition)))


;;;
;;;

(defun split-documentation-string (string &key (start 0) (end (length string)))
  "Transform a string into a list of distinct words and punctuation. Use both whitespace and punctuation to 
 split. Discard the former and retain the latter."

  (let ((whitespace #(#\space #\tab #\linefeed #\return))
        (punctuation ".,()[]{}:;"))
    (flet ((separator-p (c) (or (find c whitespace) (find c punctuation)))
           (punctuation-p (c) (find c punctuation))
           (whitespace-p (c) (find c whitespace)))
      (declare (dynamic-extent #'separator-p #'punctuation-p))
      
      (let* ((p0 (position-if-not #'whitespace-p string :start start :end end))
             (p1 0))
        (collect-list (collect)
          ;; skip whitespace, collect initial punctuation, a word proper, then trailing punctuation.
          (loop (unless p0 (return))
                (setf p1 (position-if-not #'separator-p string :start p0 :end end))
                (unless (eql p0 p1)
                  (collect (subseq string p0 (or p1 end)))
                  (unless p1 (return))
                  (setf p0 p1))
                (setf p1 (position-if #'separator-p string :start p0 :end end))
                (collect (subseq string p0 (or p1 end)))
                (unless p1 (return))
                (setf p0 p1)
                ;(setf p1 (position-if-not #'punctuation-p string :start p0 :end end))
                ;(unless (eql p0 p1)
                ;  (collect (subseq string p0 (or p1 end)))
                ;  (unless p1 (return))
                ;  (setf p0 p1))
                ;(setq p0 (position-if-not #'whitespace-p string :start p1 :end end))
                ))))))

(assert (equal (split-documentation-string
                "This is a (short) doc-string. It describes the classes class1, and class2.")
               '("This" " " "is" " " "a" " (" "short" ") " "doc-string" ". "
                 "It" " " "describes" " " "the" " " "classes" " " "class1" ", " "and" " " "class2" ".")))


(defGeneric annotate-documentation (documentation)
  (:documentation "Given a documentation string, split it into terms and augment it iwith cross-reference
 annotations. The susceptible symbols are words which stand in relation to some adjective or verb in
 *documentation-terms*. In each such case, if metadata has been collected for the concrete term,
 the term is replaced with a cross-reference.")
 
  (:method ((documentation string))
    "Given a string, split and annotate it."
    (annotate-documentation
     (split-documentation-string documentation)))

  (:method ((documentation list))
    (let ((documentation-type nil)
          (previous-word nil))
      (collect-list (collect-word)
        (labels ((conjunction-p (word)
                   (member word *documentation-conjunctions* :test #'string-equal))
                 (name-character-p (c)
                   (or (alphanumericp c) (find c "-_")))
                 (anchor-value (word)
                   (if (find-if (complement #'name-character-p) word)
                     (substitute-if #\_ (complement #'name-character-p)  word)
                     word))
                 (word-definition (word type)
                   ;; normalize for testable types
                   (case type ((type variable) (setf type 'symbol)))
                   (let* ((entry (gethash word *walker-cache*))
                          (value (when entry
                                   (find-if #'(lambda (v) (typep v type))
                                            (getf (walker-entry-properties entry) :values)))))
                     (when value (find-definition value *walker*))))
                 (annotate-word (word)
                   (let* ((definition (word-definition word documentation-type)))
                     (cond (definition
                            `(({}a ({}href ,(definition-documentation-relative-url definition))
                                   ({}class ,(format nil "reference ~(~a~)" documentation-type)))
                              ,word))
                           (t
                            (annotate-parameter word)))))
                 (annotate-parameter (word)
                   ;; if the word was not a link, check to see if it was a parameter.
                   (if (member word *documentation-parameters* :test #'string-equal)
                     `({xhtml}code ,word)
                     word))
                 (note-documentation-type (word)
                   (let ((type (rest (assoc word *documentation-terms* :test #'string-equal))))
                     (when type
                       (setf documentation-type type)))))                        
                     
          (dolist (word documentation)
            (cond ((or (not (stringp word)) (conjunction-p word) (notany #'alpha-char-p word))
                   (collect-word word))
                  ((note-documentation-type word)
                   (when previous-word
                     (collect-word (annotate-word previous-word))
                     (setf previous-word nil))
                   (collect-word word))
                  (documentation-type
                   (collect-word (annotate-word word)))
                  (t
                   (collect-word word)))))))))

;;;
;;; track packages

(defun documentation-packages (&optional (excluded *excluded-packages*))
  "Return the packages to include in a documentation operation.
 This is the list of current packages, minus the excluded set. This is initially
 those present at load time, but can be augmented."
  (set-difference (list-all-packages) excluded))
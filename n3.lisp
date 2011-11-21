;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation.xml; -*-


(in-package :de.setf.utility.implementation.xml)

(:documentation
  "This file defines oversimplified primitive n3 encoding fo the introspection service."
 
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

 (description
  "Extend the n3 encoding to permit arbitrary object which are identified by individual urn."))


(defparameter *lisp-urn-namespace* "US-ANSI-INCITS-226-1994-R2004"
  "This is based on the ansi-incits document identifier for common lisp.

 Within the namespace, objects are identified as [ type ';' identifier ]*.
 The type is a symbol and the identifer is type specific. The seperaor is chosen based on
 rfc-2141, which is old and just proposed, but with revision. see eg.
 [bcp66](https://www3.tools.ietf.org/html/bcp66).")


(defclass definition-urn (xqdm:urn)
  ((xqdm:namespace
    :initform *lisp-urn-namespace* :allocation :class)
   (path
    :initarg :path :initform (error "Path is required.")
    :reader urn-path)))

(defmethod xqdm:urn-string ((urn definition-urn))
  (or (call-next-method)
      (setf (slot-value urn 'string)
            ;; in order to make them unambiguous, the package prefix - but not quotes are required.
            (format nil "~{~a~^;~}" (urn-path urn)))))


(defgeneric definition-uri (object)
  (:documentation "Return a global identifier for a system component.")

  (:method ((object asdf:system))
    (make-instance 'definition-urn
      :path (list 'asdf:system (asdf:component-name object))))

  (:method ((object null))
    (make-instance 'definition-urn
      :path (list 'null)))

  (:method ((object asdf:component))
    (let ((purn (definition-uri (asdf:component-parent object))))
      (make-instance 'definition-urn
        :path `(,(type-of object) ,@(rest (urn-path purn)) ,(asdf:component-name object)))))

  (:method ((object package))
    (make-instance 'definition-urn
      :path (list 'package (package-name object))))

  (:method ((object function))
    (make-instance 'definition-urn
      :path `(,(type-of object) ,(dsw:function-name object))))

  (:method ((object class))
    (make-instance 'definition-urn
      :path `(,(type-of object) ,(class-name object)))))

;;; (definition-uri (find-package :de.setf.utility))
;;; (definition-uri #'definition-uri)




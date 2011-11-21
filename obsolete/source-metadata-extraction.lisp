;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-

(in-package :de.setf.utility.implementation)

(document :file
 (description "This file defines source code metadata extraction operators for the documentation module of
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
  "This file defines operators to extract definition metadata from source files."))



;;;
;;;
;;;




(defun instance-definition (datum &rest initargs)
  (apply #'make-instance (compute-definition-class datum) :object datum initargs))

(defGeneric instance-definition-as (datum type)
  (:method ((name string) (type t))
           (let ((symbol (ignore-errors (read-from-string name))))
             (when (and symbol (not (eq (symbol-package symbol) *common-lisp-package*)))
               (instance-definition-as symbol type))))
  (:method ((designator symbol) (type (eql 'package)))
           (instance-definition-as (string designator) type))
  (:method ((designator string) (type (eql 'package)))
           (instance-definition (find-package designator)))
  (:method ((package package) (type (eql 'package)))
           (make-instance *class.defpackage* :object package :name (package-name package)
                          :documentation (documentation package 'package)))
  (:method ((name symbol) (type (eql 'class)) &aux class)
           (when (typep (find-class name nil) 'standard-class)
             (instance-definition class :name name)))
  (:method ((name symbol) (type (eql 'function)))
           (when (fboundp name)
             (instance-definition (symbol-function name) :name name :documentation (documentation name 'function))))
  (:method ((name symbol) (type (eql 'macro)))
           (when (macro-function name)
             (make-instance *class.defmacro* :name name :documentation (documentation name 'macro))))
  (:method ((name symbol) (type (eql 'variable)))
           (when (boundp name)
             (make-instance *class.defparameter* :object (symbol-value name) :class :name name
                            :documentation (documentation name 'variable))))
  (:method ((name symbol) (type (eql 'structure)) &aux class)
           (when (typep (setf class (find-class name nil)) 'structure-class)
             (make-instance *class.defstruct* :object class :name name
                            :documentation (documentation name 'structure))))                          
  (:method ((name symbol) (type (eql 'type)))
           (when (ignore-errors (typep nil name) t)
             (make-instance *class.deftype* :name name))))


;;;
;;;
;;;

(defun find-body-documentation (body &aux form)
  (loop (typecase (setf form (pop body))
          (null (return nil))
          (string (return form))
          (cons (unless (or (eq (first form) 'declare) (keywordp (first form)))
                  (return))))))

(defGeneric definition-name (definition)
  (:method ((definition definition)) nil)
  (:method ((definition named-definition))
           (if (slot-boundp definition 'name)
             (slot-value definition 'name)
             (setf (slot-value definition 'name)
                   (funcall (definition-name-reader definition) (definition-form definition))))))

(defGeneric definition-documentation (definition)
  (:method ((definition definition)) nil)
  (:method :around ((definition definition-definition))
           "check for a cached string if present, return it. otherwise delegate to the specialized primary method. don't bind the value here: leave that to the primary method's discretion."
           (if (slot-boundp definition 'documentation)
             (slot-value definition 'documentation)
             (call-next-method)))
  (:method ((definition class-definition))
           (setf (slot-value definition 'documentation)
                 (second (assoc :documentation
                                (nthcdr 4 (definition-form definition))))))
  (:method ((definition function-definition))
           (setf (slot-value definition 'documentation)
                 (find-body-documentation (cdddr (definition-form definition)))))
  (:method ((definition generic-function-definition))
           (setf (slot-value definition 'documentation)
                 (second (assoc :documentation (cdddr (definition-form definition))))))
  (:method ((definition global-definition))
           (setf (slot-value definition 'documentation)
                 (fourth (definition-form definition))))
  (:method ((definition method-definition))
           (setf (slot-value definition 'documentation)
                 (find-body-documentation (rest (member-if #'listp (definition-form definition)))))))
  

(defGeneric definition-operator (definition)
  (:method ((definition definition)) nil)
  (:method ((definition definition-definition))
           (if (slot-boundp definition 'operator)
             (slot-value definition 'operator)
             (setf (slot-value definition 'operator)
                   (first (definition-form definition))))))

(defGeneric definition-form (definition)
  (:documentation
   "the annotated lisp expression. if extracted from source code, it is the literal source.
    if the result of translation or generation, it may be computed from other properties of
    the definition.")
  (:method ((definition definition)) nil))

(defgeneric definition-setf-p (definition)
  (:method ((instance definition)) nil)
  (:method ((instance function-definition))
           (if (slot-boundp instance 'setf)
               (slot-value instance 'setf)
               (setf (slot-value instance 'setf)
                     (let ((form-name (definition-name instance)))
                       (and (consp form-name) (eq (first form-name) 'setf)))))))

(defgeneric definition-specializers (definition)
  (:method ((instance definition)) nil)
  (:method ((instance method-definition))
           (if (slot-boundp instance 'specializers)
             (slot-value instance 'specializers)
             (let ((form (definition-form instance)))
               (setf (slot-value instance 'specializers)
                     (ldiff (cddr form) (member-if #'listp (cddr form))))))))

(defgeneric definition-qualifiers (definition)
  (:method ((instance definition)) nil)
  (:method ((instance method-definition))
           (flet ((compute-qualifiers (parameters)
                    (mapcar #'(lambda (parameter) (if (consp parameter) (second parameter) t))
                            (ldiff parameters
                                   (member-if #'(lambda (parameter)
                                                  (and (symbolp parameter)
                                                       (char= #\& (char (string parameter) 0))))
                                              parameters)))))
             (if (slot-boundp instance 'qualifiers)
               (slot-value instance 'qualifiers)
               (setf (slot-value instance 'qualifiers)
                     (compute-qualifiers (definition-parameters instance)))))))

(defGeneric definition-parameters (definition)
  (:method ((instance definition)) nil)
  (:method ((instance function-definition))
           (if (slot-boundp instance 'parameters)
             (slot-value instance 'parameters)
             (setf (slot-value instance 'parameters) (third (definition-form instance)))))
  (:method ((instance method-definition))
           (if (slot-boundp instance 'parameters)
             (slot-value instance 'parameters)
             (setf (slot-value instance 'parameters) (find-if #'consp (cddr (definition-form instance)))))))

(defGeneric definition-method-combination (definition)
  (:method ((instance definition)) nil)
  (:method ((instance generic-function-definition))
           (if (slot-boundp instance 'method-combination)
             (slot-value instance 'method-combination)
             (setf (slot-value instance 'method-combination)
                   (or (second (assoc :method-combination (nthcdr (definition-form instance) 4)))
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
  (:method :around ((instance definition))
           "check for a cached object. if present, return it. otherwise delegate to the specialized primary method. don't bind the value here: leave that to the primary method's discretion."
           (if (slot-boundp instance 'object)
             (slot-value instance 'object)
             (call-next-method)))
  (:method ((instance definition))
           (setf (slot-value instance 'object) nil))
  (:method ((instance class-definition))
           (let* ((class (find-class (definition-name instance) nil)))
             (when class (setf (slot-value instance 'object) class))))
  (:method ((instance function-definition))
           (let ((name (definition-name instance)))
             (let* ((designator (if (definition-setf-p instance) (list 'setf name) name))
                    (function (when (fboundp designator) (fdefinition designator))))
               (when function (setf (slot-value instance 'object) function)))))
  (:method ((instance global-definition))
           (let ((name (definition-name instance)))
             (when (boundp name)
               (setf (slot-value instance 'object) (symbol-value name)))))
  (:method ((instance method-definition))
            (let* ((name (definition-name instance))
                   (specializers (definition-specializers instance))
                   (qualifiers (definition-qualifiers instance))
                   (designator (if (definition-setf-p instance) (list 'setf name) name))
                   (function (when (fboundp designator) (fdefinition designator)))
                   (method nil))
              (when (and (typep function 'generic-function)
                          (setf method (find-method function qualifiers specializers)))
                 (setf (slot-value instance 'object) method))))
  (:method ((instance de.setf.utility.definitions::defpackage))
           (let ((package (find-package (definition-name instance))))
             (when package (setf (slot-value instance 'object) package))))
  (:method ((instance type-definition))
           (setf (slot-value instance 'object) (definition-name instance)))
  (:method ((instance macro-definition))
           (let ((function (macro-function (definition-name instance))))
             (when function (setf (slot-value instance 'object) function))))
  (:method ((instance de.setf.utility.definitions::define-method-combination))
           (let ((combination (find-method-combination (definition-name instance)
                                                       nil
                                                       nil)))
             (when combination
               (setf (slot-value instance 'object) combination))))
  (:method ((instance de.setf.utility.definitions::defsetf))
           (setf (slot-value instance 'object) nil))
  (:method ((instance de.setf.utility.definitions::define-setf-expander))
           (setf (slot-value instance 'object) nil)))
           

  

(defGeneric definition-signature (definition)
  (:method-combination list  :most-specific-last)
  (:method list ((definition definition)) (definition-category definition))
  (:method list ((definition named-definition)) (definition-name definition))
  (:method list ((definition function-definition))
           (definition-parameters definition)))


(defun defpackage-name-reader (form)
  (let ((name (second form)))
    (etypecase name
      (symbol name)
      (string (intern name :keyword)))))

;;;
;;;
;;;

(defgeneric bind-definition (definition)
  (:method ((definition definition)) nil)
  (:method ((definition named-definition))
           (let ((name (definition-name definition)))
             (typecase name
               (null )
               (symbol (setf (get name (type-of definition)) definition))
               (cons (setf name (second name)
                           name (intern (concatenate 'string "SETF::" (string name)) (symbol-package name)))
                     (setf (get name (type-of definition)) definition))))))

(defMethod initialize-instance ((instance named-definition) &rest initargs &key form string)
  "initialize an definition based on either an s-expression form or a text string. where a form is present, string and buffer arguments are ignored. where no form is present, it is is read from the text string argument, which itself firt be extract from an argument buffer."
  (cond (form)
        ((stringp string)
         (setf form
               (handler-case (read-from-string string)
                 (error (condition)
                        (destructuring-bind (&key start pathname &allow-other-keys) initargs
                          (warn "reading from text expression ~:[~s~;(~s @~d)~] signaled: ~a"
                                pathname (if pathname (list pathname start) string) condition)
                          nil))))))
  (cond ((and (consp form) (eq (type-of instance) 'definition))
         ;; if the instance is generic, but a form is supplied, then specialize it
         (let* ((operator (first form))
                (class (find-symbol (string operator) *definition-package*)))
           (cond ((eq class (type-of instance))
                  (apply #'call-next-method instance :form form initargs)
                  (bind-definition instance))
                 ((null class)
                  (if (eq (type-of instance) *class.named-definition*)
                    (apply #'call-next-method instance :form form initargs)
                    (error "no definition class defined: ~s." operator)))
                 (t                            ; restart the initialization for the more precise class
                  (apply #'initialize-instance (change-class instance class) :form form initargs)))))
        (t
         (call-next-method)))
    ;(apply #'initialize-instance (change-class instance *class.comment-definition*) initargs))
  )

:EOF

           

;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-

(in-package :de.setf.utility.implementation)

(:documentation "This file defines system definition walking operators for the 'de.setf.utility' library."
  
  (copyright
   "Copyright 2003, 2010 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved"
   "'de.setf.utility' is free software: you can redistribute it and/or modify
 it under the terms of version 3 of the GNU Lesser General Public License as published by
 the Free Software Foundation.

 'de.setf.utility' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 See the GNU Lesser General Public License for more details.

 A copy of the GNU Lesser General Public License should be included with 'de.setf.utility, as `lgpl.txt`.
 If not, see the GNU [site](http://www.gnu.org/licenses/).")
  
  (history)
  
  (long-description "The component walkers navigate the links in an asdf system model. THis includes the
 basic components - system, module, and file, and the links to the file definitions for functions,
 classes, etc."))


;;;
;;; walking control classes

(defclass system-walker (preorder-node-walker
                         preorder-link-walker
                         cyclic-walker)
  ((class-qualifiers
    :initform '(requirement component)
    :allocation :class)))


(defclass source-walker (preorder-node-walker
                         preorder-link-walker
                         cyclic-walker)
  ((class-qualifiers
    :initform '(component)
    :allocation :class)))


(defclass definition-cache (cyclic-walker)
  ((class-qualifiers
    :initform '(definition)
    :allocation :class)))


(defclass definition-walker (definition-cache source-walker system-walker introspective-walker)
  ()
  (:documentation "The class definition-walker combines the behaviour of all walkers
    which contribute to a system's implementation:

    - system-walker : to examine the asdf definition
    - source walker : to extract definitions from source files
    - introspective-walker  : to extract definitions from an active image
      - class-walker
      - package-walker
      - function-walker"))

(assert (null (set-difference  (walker-inherited-qualifiers (find-class 'definition-walker))
                               '(requirement component definition used-by imports uses internal external
                                 subclass superclass binds specializes callers calls method relations other
                                 function macro class self constituents predecessors successors))))

;;;
;;; traversal operations

(defmethod in-extent-p ((object asdf:component))
  "True iff the component or some parent is included in the extent"
  (or (find object *walk-extent*)
      (and (asdf:component-parent object) (in-extent-p (asdf:component-parent object)))))


(defmethod asdf:component-depends-on ((operation (eql t)) (component asdf:component))
  (asdf::component-in-order-to component))

(defparameter *system-requirement-relation-map*
  '((asdf:compile-op (asdf:compile-op . compile-compile-requirement)
                     (asdf:load-op . compile-load-requirement))
    (asdf:load-op (asdf:compile-op . load-compile-requirement)
                  (asdf:load-op . load-load-requirement))))

(defun requirement-relation (operation required-op)
  (or (rest (assoc required-op (rest (assoc operation *system-requirement-relation-map*))))
      (cons-symbol nil operation required-op)))


(defmethod walk-node-predecessors requirement
           ((walker system-walker) (component asdf:component) (op t))
  (loop for (operation . requirements) in (asdf:component-depends-on t component)
        do (loop for (required-op . required-components) in requirements
                 for relation = (requirement-relation operation required-op)
                 do (dolist (required-component required-components)
                      (flet ((do-walk (required-component)
                               (walk-link walker relation component
                                          (let* ((parent (asdf:component-parent component))
                                                 (name (string-downcase required-component))
                                                 (found 
                                                  (if parent
                                                    (asdf:find-component parent name)
                                                    ;; if looking for a system, make stubs for anything which is missing
                                                    ;; or fails to load.
                                                    (handler-case
                                                      (asdf:find-component parent name)
                                                      (asdf:missing-component (c)
                                                                              (warn "System '~a' not found: ~a." name c)
                                                                              (setf (find-system-prototype name)
                                                                                    (make-instance 'asdf:system :name name)))
                                                      (error (c)
                                                             (warn "System '~a' caused error: ~a." name c)
                                                             (setf (find-system-prototype name)
                                                                   (make-instance 'asdf:system :name name)))))))
                                            ;; strip the asdf timestamp wrapper from systems
                                            (if (consp found) (rest found) found))
                                          op)))
                        (typecase required-component
                          (cons (do-walk (second required-component)))
                          ((or string symbol) (do-walk required-component)))))))
  component)


(defmethod walk-node-successors requirement
           ((walker system-walker) (component asdf:component) (op t))
  ;; nothing to trace in this direction
  component)

(defmethod walk-node-constituents component
           ((walker system-walker) (module asdf:module) (op t))
  (dolist (component (asdf:module-components module))
    (walk-link walker 'component module component op))
  module)

(defmethod walk-node-constituents component
           ((walker system-walker) (file asdf:source-file) (op t))
  (dolist (component (component-definitions file))
    (walk-link walker 'component file component op))
  file)


(defmethod walk-node-constituents contains
           ((walker source-walker) (pathname pathname) (op t))
  ;; at the moment, this does nothing
  ;; eventually integrate the code from :de.setf.utility.annotations
  pathname)

;;;
;;; iff the walker is a definition cache, then the definition-qualified methods will run.
;;; in that case, the respective worker creates and caches the definition.

(defmethod walk-node-self definition
           ((walker class-walker) (node class) (op t))
  (let ((node-definition (ensure-definition node walker))
        (pathname (object-source-information node 'class)))
    (setf (slot-value node-definition 'pathname) pathname)
    (when pathname
      (setf pathname (namestring (translate-logical-pathname pathname)))
      (let ((file-definition (find-definition pathname walker)))
        (if file-definition
          (setf (find-definition node file-definition) node-definition)
          (warn "No source file component: ~s." node))))
    node))

(defmethod walk-node-self definition
           ((walker function-walker) (node function) (op t))
  (let ((node-definition (ensure-definition node walker))
        (pathname (object-source-information node 'function)))
    (setf (slot-value node-definition 'pathname) pathname)
    (when pathname
      (setf pathname (namestring (translate-logical-pathname pathname)))
      (let ((file-definition (find-definition pathname walker)))
        (if file-definition
          (setf (find-definition node file-definition) node-definition)
          (warn "No source file component: ~s." node))))
    node))

(defmethod walk-node-self definition
           ((walker function-walker) (node method) (op t))
  (let ((node-definition (ensure-definition node walker))
        (pathname (object-source-information node 'method)))
    (setf (slot-value node-definition 'pathname) pathname)
    (when pathname
      (setf pathname (namestring (translate-logical-pathname pathname)))
      (let ((file-definition (find-definition pathname walker)))
        (if file-definition
          (setf (find-definition node file-definition) node-definition)
          (warn "No source file component: ~s." node))))
    node))

(defmethod walk-node-self definition
           ((walker system-walker) (node asdf:component) (op t))
  (ensure-definition node walker))

(defmethod walk-node-self definition
           ((walker system-walker) (node asdf:source-file) (op t))
  "Assert the source file component's definition. In addition to registering it as for other
 components with the system definition path as key, registerit with the physical pathname namestring."
  (call-next-method)
  (let ((pathname (namestring (translate-logical-pathname (asdf:component-pathname node)))))
    (setf (walker-node-cache-entry walker pathname)
          (walker-node-cache-entry walker node))))


(defmethod walk-link ((walker definition-walker) (definition-type t) (object symbol) (definition t) (op t))
  "Track the symbol's binding types in the definition cache. This follows from the -constituents operation
 as qualified for the symbol and will continue to alk the bound value as a node in itself. This method serves
 just to note that the binding type exists in the symbol's definition proper."

  (call-next-method)
  (push definition (walker-node-property walker object :values))
  object)


(defmethod object-designator ((object asdf:system))
  `(asdf:system ,(asdf:component-name object)))

(defmethod object-designator ((object asdf:module))
  `(asdf:module ,(object-designator (asdf:component-parent object)) ,(asdf:component-name object)))

(defmethod object-designator ((object asdf:source-file))
  `(asdf:source-file ,(object-designator (asdf:component-parent object)) ,(asdf:component-name object)))


(defmethod component-definitions ((component asdf:cl-source-file))
  (asdf:component-property component :definitions))

(defmethod (setf component-definitions) (definitions (component asdf:cl-source-file))
  (setf (asdf:component-property component :definitions) definitions))

(defmethod asdf::component-file-documentation ((component asdf:cl-source-file))
  "Extract the documentation from the respective source file. These appear in forms
  (:documentation { string | symbol | (cons symbol) }
 which are collected and returned in order. If in-package forms appear, the package is adopted."

  (let ((pathname (asdf:component-pathname component))
        (*package* *package*))
    (with-open-file (stream pathname :direction :input :if-does-not-exist nil)
      (when stream
        (let ((elements ())
              (form nil))
          (loop (unless (setf form (ignore-errors (read stream nil nil))) (return))
                (when (consp form)
                  (destructuring-bind (op . rest) form
                    (case op
                      (in-package
                       (let ((package (find-package (second form))))
                         (if package
                           (setf *package* package)
                           (warn "Invalid package designator: ~s." (second form)))))
                      (:documentation
                        (dolist (elt rest)
                          (typecase elt
                            (string (push elt elements))
                            (symbol (push elt elements))
                            (cons
                             (cond ((every #'symbolp elt)
                                    (push elt elements))
                                   ((or (string-equal (first elt) "description")
                                        (string-equal (first elt) "long-description"))
                                    (setf elements (append (reverse (rest elt)) elements))))))))))))
          (nreverse elements))))))

(defmethod (setf find-definition) (definition (subject t) (component asdf:source-file))
  (let ((old (find-definition subject component)))
    (if old
      (setf (component-definitions component)
            (substitute definition old (component-definitions component)))
      (push definition (component-definitions component)))
    (setf (asdf:component-parent definition) component)
    definition))

(defmethod find-definition ((subject t) (component asdf:source-file))
  (let ((class (definition-class-of subject))
         (name (definition-name subject)))
    (find-if #'(lambda (d) (and (typep d class)
                                (equalp (definition-name d) name)))
             (component-definitions component))))


;;; encoding utilities

(labels ((path-to (component)
           (typecase component
             (asdf:system
              (let ((project-root (ignore-errors (truename (definition-source-directory component))))
                    (source-root (ignore-errors (truename (make-pathname :host "UPLOAD" :directory '(:absolute))))))
                (when (and project-root source-root)
                  (rest (pathname-directory (enough-namestring project-root source-root))))))
             (null nil)
             (t                         ; faild for complex pathname specs
              (append (path-to (asdf:component-parent component)) (list (asdf:component-name component)))))))

  (defgeneric component-url (component)
    (:documentation "Return a relative url which identifies the component.
 For a system, the name is global. For other components, it requires a path.")

    (:method ((component asdf:system))
      (format nil "javascript:parent.showProject('~a');" (asdf:component-name component)))

    (:method ((component asdf:module))
      (format nil "/lisp/source/~{~a/~}~a/"
              (path-to (asdf:component-parent component))
              (asdf:component-name component)))
    
    (:method ((component asdf:component))
      (format nil "/lisp/source/~{~a/~}~a"
              (path-to (asdf:component-parent component))
              (file-namestring (asdf:component-relative-pathname component))))))


  
;;;
;;; interface

(defgeneric walk-definitions (root extent op &key &allow-other-keys)
  (:argument-precedence-order extent root op)

  (:method ((root t) (extent t) op &rest options)
    (let ((*walk-extent* (compute-definition-extent extent))
          (root-instances (compute-definition-extent root)))
      (values (walk-model (apply #'make-instance 'definition-walker options)
                          root-instances
                          op)
              *walk-extent*))))
  
(defgeneric walk-system (root extent op &key &allow-other-keys)
  (:argument-precedence-order extent root op)

  (:method ((root t) (extent t) op &rest options)
    (let ((*walk-extent* (compute-definition-extent extent)))
      (walk-model (apply #'make-instance 'system-walker options) (find-system-prototype root) op))))


(defun walk-modules (root extent op &rest args)
  (apply #'walk-system root extent
         #'(lambda (subject &optional object relation)
             (when (and (typep object 'asdf:module) (not (typep object 'asdf:system)))
               (funcall op subject object relation))
             subject)
         args))

(defun walk-systems (root extent op &rest args)
  (apply #'walk-system root extent
         #'(lambda (subject &optional object relation)
             (when (typep object 'asdf:system)
               (funcall op subject object relation))
             subject)
         args))

(defun walk-files (root extent op &rest args)
  (apply #'walk-system root extent
         #'(lambda (subject &optional object relation)
             (when (typep object 'asdf:source-file)
               (funcall op subject object relation))
             subject)
         args))

;;; (length (collect-list (collector) (walk-definitions :de.setf.xml '(:de.setf.xml) #'(lambda (&rest args) (collector (second args))))))
;;; (length (collect-list (collector) (walk-systems :de.setf.xml '(:de.setf.xml) #'(lambda (&rest args) (collector (second args))))))
;;; (length (collect-list (collector) (walk-modules :de.setf.xml '(:de.setf.xml) #'(lambda (&rest args) (collector (second args))))))
;;; (length (collect-list (collector) (walk-files :de.setf.xml '(:de.setf.xml) #'(lambda (&rest args) (collector (second args))))))


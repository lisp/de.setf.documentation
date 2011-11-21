;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-

(in-package :de.setf.utility.implementation)


(:documentation "This file defines a graphviz-based grapher for system components."
  
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
  
  (history
   (delta 20100318 "janderson" "integrate the de.setf.utilities;asdf;graph.lisp code in the
 general documentation module."))

  (long-description "Encodes the system definition model as a strict node graph. System/module/file
 components are distinguished by shape, but no subgraph structure is applied. This is intended both
 to allow more flexible radial layouts and to permit components to appear in relation to more
 than one context."))

(defparameter *relation-colors*
  '((component . "blue")
    (requires . "red")))

(defgeneric put-component-graph-statement (component &key url other relation)
  (:method ((component asdf:system) &key url (other nil o-s) relation)
    (cond (o-s
           (let ((color (rest (assoc relation *relation-colors* :test 'string-equal))))
             (if color
               (setf.dot:put-edge component other :color color)
               (setf.dot:put-edge component other :label (string relation))))
           (format setf.dot:*context* "/* ~a -[~a]-> ~a */" component relation other))

          (t
           (setf.dot:put-node component
                              :url url
                              :tooltip (let ((text (asdf:component-description component)))
                                         (if (zerop (length text))
                                           (string (asdf:component-name component))
                                           (format nil "~a : ~a"
                                                   (asdf:component-name component)
                                                   text)))
                              :label (asdf:component-name component)
                              :shape "doublecircle")
           (format setf.dot:*context* "/* ~a */" component))))

  (:method ((component asdf:module) &key url (other nil o-s) relation)
    (cond (o-s
           (let ((color (rest (assoc relation *relation-colors* :test 'string-equal))))
             (if color
               (setf.dot:put-edge component other :color color)
               (setf.dot:put-edge component other :label (string relation))))
           (format setf.dot:*context* "/* ~a -[~a]-> ~a */" component relation other))

          (t
           (setf.dot:put-node component
                              :url url
                              :label (asdf:component-name component)
                              :shape "circle")
           (format setf.dot:*context* "/* ~a */" component))))

  (:method ((component asdf:component) &key url (other nil o-s) relation)
    (cond (o-s
           (let ((color (rest (assoc relation *relation-colors* :test 'string-equal))))
             (if color
               (setf.dot:put-edge component other :color color)
               (setf.dot:put-edge component other :label (string relation))))
           (format setf.dot:*context* "/* ~a -[~a]-> ~a */" component relation other))

          (t
           (setf.dot:put-node component
                              :url url
                              :label (asdf:component-name component)
                              :shape "box")
           (format setf.dot:*context* "/* ~a */" component))))

  (:method ((component t) &rest args)
    (warn "Unknown component: ~a ~s" component args)))



(defun graph-systems (systems &key (stream *standard-output*)
                           (root (first systems))
                           (name (asdf:component-name (asdf:find-system root)))
                           (level nil)
                           (count nil)
                           (options nil)
                           (size *graph-size*)
                           (pretty nil)
                           (rankdir *graph-rankdir*)
                           (ratio *graph-ratio*)
                           (margin *graph-margin*)
                           (relation-map nil)
                           (graph-attributes '())
                           (graph-arguments graph-attributes)
                           (url-encoder nil)
                           (node-test nil)
                           (link-test nil))
  (let ((walk-count 0)
        (*system-requirement-relation-map* (append relation-map *system-requirement-relation-map*))
        (other-cache (make-hash-table))
        (node-cache (make-hash-table))
        (link-cache (make-hash-table :test 'equal)))
    (labels ((put-component (component &optional (other nil o-s) relation)
               (when (and (or (null level) (<= *walk-depth* level))
                          (or (null count) (<= walk-count count)))
                 (cond (o-s
                        (let ((key (list component relation other)))
                          (unless (gethash key link-cache)
                            (setf  (gethash key link-cache) t)
                            (unless (gethash other other-cache)
                              (setf (gethash other other-cache) :other))
                            (when (or (null link-test) (funcall link-test component other relation))
                              (put-component-graph-statement component
                                                             :other other
                                                             :relation relation)))))
                       (t
                        (incf walk-count)
                        (when (or (null node-test) (funcall node-test component))
                          (unless (gethash component node-cache)
                            (setf (gethash component node-cache) component)
                            (setf (gethash component other-cache) :self)
                            (put-component-graph-statement component
                                                           :url (encode-url component)))))))
               component)
             (encode-url (component)
               (when url-encoder (funcall url-encoder component))))
      (destructuring-bind (&key (size size) (rankdir rankdir) (margin margin) (ratio ratio)
                           &allow-other-keys)
                          graph-arguments
        (apply #'setf.dot:context-put-graph stream name
               #'(lambda ()
                   ;; walk the system definition from from the first given system
                   ;; allowing the others to bound the extent
                   (if (consp root)
                     (dolist (root root)
                       (apply #'walk-system root systems #'put-component options))
                     (apply #'walk-system root systems #'put-component options))
                   ;; handle boundry nodes which will not have appeared yet with the
                   ;; correct attributes
                   (maphash #'(lambda (component status)
                                (when (eq status :other)
                                  (when (or (null node-test) (funcall node-test component))
                                    (put-component-graph-statement component
                                                                   :url (encode-url component)))))
                            other-cache))
               :pretty pretty
               :size size
               :ratio ratio
               :rankdir rankdir
               :margin margin
               graph-arguments)))
    walk-count))


(defun print-systems (systems &key (stream *standard-output*) (root (first systems))
                              (relation-map nil))
  (let ((walk-count 0)
        (*system-requirement-relation-map* (append relation-map *system-requirement-relation-map*))
        (link-cache (make-hash-table :test 'equal)))
    (flet ((print-component-node (component &optional (other nil o-s) relation)
             (cond (o-s
                    (let ((key (list component relation other)))
                      (unless (gethash key link-cache)
                        (setf  (gethash key link-cache) t)
                        (terpri stream)
                        (dotimes (x (+ 3 (* 5 (1- *walk-depth*)))) (write-char #\space stream))
                        (format stream "~a: ~a" relation (asdf:component-name other)))))
                   (t
                    (terpri stream)
                    (dotimes (x (* 5 (1- *walk-depth*))) (write-char #\space stream))
                    (format stream "~a: ~a~@[ ~a~]"
                            (type-of component)
                            (asdf:component-name component)
                            (when (typep component 'asdf:system)
                              (asdf:system-nicknames component)))))
             (incf walk-count)
             component))
      (walk-systems root systems #'print-component-node))
    walk-count))



(defgeneric encode-graph (type systems projection &rest args)

  (:method ((relation (eql :systems)) extent (projection (eql :dot)) &rest args
            &key root &allow-other-keys)
    (setf root (or (asdf:find-system root)
                   (error "Invalid root system: ~s." root)))
    (setf extent (cons root (compute-definition-extent extent)))
    (apply #'graph-systems extent
           :root root
           :node-test #'(lambda (component) (typep component 'asdf:system))
           args))

  (:method ((relation (eql :modules)) extent (projection (eql :dot)) &rest args
            &key root &allow-other-keys)
    (setf root (or (asdf:find-system root)
                   (error "Invalid root system: ~s." root)))
    (setf extent (cons root (compute-definition-extent extent)))
    (apply #'graph-systems extent
           :root root
           :node-test #'(lambda (component) (typep component 'asdf:module))
           args))

  (:method ((relation (eql :files)) extent (projection (eql :dot))  &rest args
            &key root &allow-other-keys)
    (print (list-all-packages ))
    (setf root (or (asdf:find-system root)
                   (error "Invalid root system: ~s." root)))
    (setf extent (cons root (compute-definition-extent extent)))
    (apply #'graph-systems extent
           :root root
           args))


  (:method :around ((relation (eql :calls)) root-set (projection (eql :dot)) &rest args
                    &key (packages (documentation-packages))
                    &allow-other-keys)
           ;; resolve the traversal parameters and continue with the base method
           (setf packages (compute-call-extent packages))
           (setf root-set (or (compute-call-extent root-set) packages))
           (apply #'call-next-method relation root-set projection
                  :packages packages
                  args))

  (:method ((relation (eql :calls)) root-set (projection (eql :dot)) &rest args
            &key packages &allow-other-keys)
    "Generate a call graph:
 RELATION : :calls
 ROOT-SET : (designator (or function package))*
 PROJECTION : :dot
 :PACKAGES : (designator package)*

 Given a set of functions and/or packages as the extent, generate the call graph.
 First derive a concrete call root set from the argument, and resolve the package extent.
 If no packages are supplied, use those which have appeared since the *excluded-packages*
 were set. If no call roots were given, use the package extent as the root set also."
    (let ((root-set-names (remove nil (mapcar #'(lambda (x) (when (functionp x) (dsw:function-name x))) root-set))))
      (flet ((url-encoder (function)
               ;; return a link to encode a grap which extends the current one to include the
               ;; given function.
               (format nil "calls-2pi.svg&extent=~a,~{~a~^,~}"
                       (dsw:function-name function)
                       root-set-names)))
        (apply #'graph-functions packages :root root-set
               :url-encoder #'url-encoder
               args))))

  (:method ((relation (eql :calls)) root-set (projection (eql :rdf))
            &key root stream (packages (documentation-packages))
            &allow-other-keys)
    (setf packages (compute-call-extent packages))
    (setf root-set (or (compute-call-extent root-set) packages))
    (with-open-file (stream stream :direction :output :if-exists :supersede :if-does-not-exist :create)
        (format *trace-output* "output to: ~s" stream)
        (format stream "# ~a . ~a : ~/date:format-iso-time/"
                root relation (get-universal-time))
        (flet ((encode-relations (&rest args)
                 (when (and (typep (first args) 'function) (rest args))
                   (rotatef (second args) (third args))
                   (format stream "~&~/n3:format/ ." args))))
          (walk-image root-set packages #'encode-relations
                 :excluded-qualifiers '(callers))
          (terpri stream)))))



#+(or )
;; see also system-operations.lisp for the ecosystem graph
(progn
  (print-systems (list :de.setf.documentation :de.setf.utility :org.cl-http
                       :de.setf.xml :net.common-lisp.closer-mop :de.setf.utility.walker)
                 :relation-map '((asdf:compile-op (asdf:compile-op . requires)
                                                  (asdf:load-op . requires))
                                 (asdf:load-op (asdf:compile-op . requires)
                                               (asdf:load-op . requires))))
  
  
  (graph-systems (list :de.setf.documentation :de.setf.utility :org.cl-http
                       :de.setf.xml :net.common-lisp.closer-mop :de.setf.utility.walker)
                 :stream "METADATA:system.dot"
                 :pretty t
                 :url-encoder #'component-url
                 :relation-map '((asdf:compile-op (asdf:compile-op . requires)
                                                  (asdf:load-op . requires))
                                 (asdf:load-op (asdf:compile-op . requires)
                                               (asdf:load-op . requires))))

  (graph-systems (list :de.setf.documentation :de.setf.utility :org.cl-http
                       :de.setf.xml :net.common-lisp.closer-mop :de.setf.utility.walker)
                 :stream "METADATA:system-modules.dot"
                 :pretty t
                 :url-encoder #'component-url
                 :size "5,5"
                 :node-test #'(lambda (component) (typep component 'asdf:module))
                 :link-test #'(lambda (component other relation)
                                (declare (ignore relation))
                                (and (typep component 'asdf:module)
                                     (typep other 'asdf:module)))
                 :relation-map '((asdf:compile-op (asdf:compile-op . requires)
                                                  (asdf:load-op . requires))
                                 (asdf:load-op (asdf:compile-op . requires)
                                               (asdf:load-op . requires))))
  ;; dot
  (asdf:run-shell-command "/opt/local/bin/dot -Tsvg -o~a ~a"
                          #p"DATA:system.svg" #p"DATA:system.dot")
  )

:de.setf.utility.documentation


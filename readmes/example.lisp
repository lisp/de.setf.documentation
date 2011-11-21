;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-


(in-package :de.setf.utility.implementation)

(:documentation "This file is part of the 'de.setf.utility' Common Lisp library.
  It  contains a simple documentation example."

  (copyright
   "Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved"
   "'de.setf.amqp' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'setf.amqp' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.amqp' as `AMQP:agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/)."))



(walker-effective-qualifiers (make-instance 'definition-walker))

(asdf:load-system :test-system)

;; root at the system
(flet ((print-step (node &optional to relation)
         (if to
           (format *trace-output* "~&[~a ~a ~a]" node relation to)
           (format *trace-output* "~&~a" node))
         t))
  (let ((extent (compute-definition-extent :test-system)))
    (walk-definitions (first extent) extent #'print-step)))

(let ((extent (compute-definition-extent :de.setf.xml))) ; :test-system)))
  (walk-definitions (first extent) extent #'(lambda (&rest args) (print args))))

(let ((extent (compute-definition-extent :de.setf.xml))) ; :test-system)))
  (walk-systems (first extent) extent #'(lambda (&rest args) (print args))))

;;; root at the package
(flet ((print-step (node &optional to relation)
         (if to
           (format *trace-output* "~&[~a ~a ~a]" node relation to)
           (format *trace-output* "~&~a" node))
         t))
  (let ((extent (compute-definition-extent :test-system)))
    (walk-definitions (second extent) extent #'print-step :excluded-qualifiers '(callers))))

;;; both
(defparameter *w* nil)
(flet ((print-step (node &optional to relation)
         (if to
           (format *trace-output* "~&[~a ~a ~a]" node relation to)
           (format *trace-output* "~&~a" node))
         t))
  (let* ((extent (compute-definition-extent :test-system))
         (walker
          (walk-definitions extent extent #'print-step :excluded-qualifiers '(internal callers))))
    (setq *w* walker)
    (maphash #'(lambda (k e)
                 (print (list :* k (or (getf (walker-entry-properties e) :values)
                                       (getf (walker-entry-properties e) :definition)))))
             (walker-cache walker))))


(flet ((print-step (node &optional to relation) (break)
         (if to
           (format *trace-output* "~&[~a ~a ~a]" node relation to)
           (format *trace-output* "~&~a" node))
         t))
  (let* ((extent (compute-definition-extent :de.setf.graphics))
         (walker
          (walk-definitions extent extent #'print-step :excluded-qualifiers '(internal calls callers))))
    (setq *w* walker)
    (maphash #'(lambda (k e)
                 (print (list :* k (or (getf (walker-entry-properties e) :values)
                                       (getf (walker-entry-properties e) :definition)))))
             (walker-cache walker))))


(untrace walk-node walk-link in-extent-p)
(let ((packages (mapcar #'find-package '(:test-system)))
      (limit 100) (count 0))
  (block :walk
    (walk-image packages packages
                #'(lambda (arg &optional rel arg2)
                    (print (list arg rel arg2))
                    (when (> (incf count) limit) (return-from :walk count)))
                :excluded-qualifiers '(callers))))


(let ((*walker* *w*)
      (*walker-cache* (make-hash-table :test 'equalp)))
  (maphash #'(lambda (k v)
               (when (symbolp k)
                 (setf (gethash (string k) *walker-cache*) v))
               (setf (gethash k *walker-cache*) v))
           (walker-cache *walker*))
  (encode-documentation (find-package :test-system) t mime:text/xhtml)
  (encode-documentation (first (asdf:module-components (asdf:find-system :test-system)))
                        t mime:text/xhtml)
  (encode-documentation (first (asdf:module-components (second (asdf:module-components (asdf:find-system :test-system)))))
                        t mime:text/xhtml))

(asdf:load-system :de.setf.graphics)
(dsw:object-source-information (find-package :de.setf.graphics) 'package)
(dsw:object-source-information #'DE.SETF.GRAPHICS:CARTESIAN->SPHERICAL 'function)
(document-as :de.setf.graphics #P"DSG:" mime:text/xhtml)

(asdf:load-system :de.setf.amqp)
(compute-definition-extent  :de.setf.amqp)

(dsw:object-source-information 'amqp.i::device-open 'function)
(document-as :de.setf.amqp #P"AMQP:" mime:text/xhtml)


;; (ccl::get-source-files-with-types&classes 'test-system:function1)
;; (dsw:object-source-information #'test-system:function1 'function)
;; (dsw:object-source-information 'test-system:function1 'function)



(document-as '(:de.setf.amqp
               send-method
               call-with-encoded-arguments
               call-with-decoded-arguments
               put-encoded-frame
               get-read-frame
               process-frame
               
               
               with-open-stream
               read
               write
               stream-read- byte char string sequence vector
               stream-write- byte char string sequence vector
               finish-output
               force-output
               stream-listen
               stream-peek-char
               stream-clear-output
               
               device-open (stream #-sbcl slots initargs)
               device-close (stream abort)
               device-read (stream buffer start end blocking)
               device-clear-input (stream buffer-only)
               device-write (stream buffer start end blocking)
               device-clear-output (stream)
               device-flush (device)
               device-read-content (device &rest content-arguments)
               device-write-content (device body &rest content-arguments)
               
               )
             #p"LIBRARY:documentation"
             mime:text/xhtml)

;;;
;;; this was used to convert from the static ':denominative' methods to
;;; the dynamic, ':qualifying' mechanism.

(defun remove-denominative-methods (op)
  (dolist (m (generic-function-methods op))
    (when (intersection (method-qualifiers m) '(:denominative :qualifiers))
      (print m)
      (remove-method op m))))

(with-package-iterator (next :de.setf.utility.walker :internal :external) 
  (loop (multiple-value-bind (next-p sym) (next)
          (unless next-p (return))
          (when (fboundp sym) 
            (let ((fun (fdefinition sym)))
              (when (typep fun 'generic-function)
                (print sym)
                (remove-denominative-methods fun)))))))


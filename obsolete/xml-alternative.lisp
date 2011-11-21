;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-


(in-package :de.setf.utility.implementation)

(document  "This file defines an xml encoder for the documentation module of the
 'de.setf.utility' library."
 
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
  "Define a symbol reader and an xml-encoder for the event htat this should run without the
 fill xml module.
 - The symbol reader reader {namespace}local-part q-names as symbols. 
 - the xml macro encodes to an output stream with minimal namespace support."))


;;;
;;; xml-compatible symbol reader

(defun |{-symbol-reader|
       (stream char
               &aux
               (buffer (make-array 32 :adjustable t :fill-pointer 0 :element-type 'character))
               (package nil))
  (loop (setf char (read-char stream))
        (when (char= char #\})
          (setf package (or (find-package buffer)
                            (error "Package not found: ~s." buffer)))
          (return))
        (vector-push-extend char buffer))
  (setf (fill-pointer buffer) 0)
  (cond ((eql #\| (peek-char t stream))
         ;; where explicitly escaped allow non-name characters
         (read-char stream)
         (loop (setf char (read-char stream nil nil))
               (unless char (return))
               (when (eql #\| char)
                 (return))
               (vector-push-extend char buffer)))
        (t
         (loop (setf char (read-char stream nil nil))
               (unless char (return))
               (unless (or (alphanumericp char) (find char ":_-."))
                 (unread-char char stream)
                 (return))
               (vector-push-extend char buffer))))
  
  (or (find-symbol buffer package)
      (intern (subseq buffer 0) package)))

(defun install-|{-reader| ()
  (set-macro-character #\{ '|{-symbol-reader| t))

(unless (get-macro-character #\{)
  (install-|{-reader|))

;;;
;;; xml encoder



(defparameter *xml-declaration* "<?xml version='1.0' charset='iso-8859-1'?>")

(defparameter *xhtml-dtd*
  "<!DOCTYPE svg PUBLIC '-//W3C//DTD XHTML 1.0 Transitional//EN' 'http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd'>")

(defparameter *xml-output* nil
  "the destination stream for xml/xhtml/svg encoding")

(defparameter *xml-default-namespace* nil)

(defparameter *document-topic* "untitled")
(defparameter *document-date* "untitled")
(defparameter *document-timestamp* "untitled")


(defun encode-character-data (string)
  (dotimes (i (length string))
    (let ((c (char string i)))
      (case c
        (#\< (write-string "&lt;" *xml-output*))
        (#\> (write-string "&gt;" *xml-output*))
        (#\" (write-string "&quote;" *xml-output*))
        (#\' (write-string "&apos;" *xml-output*))
        (#\& (write-string "&amp;" *xml-output*))
        ;; no, confuses adobe svg viewer (#\# (write-string "&#163;" *xml-output*))
        (t (write-char c *xml-output*))))))

(defun encode-string (object)
  (typecase object
    (string (write-string object *xml-output*))
    (t (format *xml-output* "~a" object))))

(defgeneric encode-parsed-character-data (model)
  (:method ((symbol null))
    )
  (:method ((symbol symbol))
    (encode-string symbol))
  (:method ((string string))
    "Given a string, encode it escaping markup."
    (encode-character-data string))
  (:method ((data cons))
    "Given a list of parsed, encode it as markup."
    (dolist (element data)
      (etypecase element
        (string (encode-character-data element))
        (cons (encode-element element))))))
      

(defgeneric encode-number (datum)
  (:method ((datum integer))
    (format *xml-output* "~d" datum))
  (:method ((datum float))
    (format *xml-output* "~6,4,,,,,'eE" datum))
  (:method ((datum number))
    (encode-number (float datum 1.0))))


(defgeneric encode-attribute (name value)
  (:method ((name t) (value null))
    nil)
  (:method ((name t) (value string))
    (write-char #\space *xml-output*)
    (encode-name name)
    (write-char #\= *xml-output*)
    (write-char #\' *xml-output*)
    (encode-character-data value)
    (write-char #\' *xml-output*)
    (when (eql name '{xmlns}||)
      (setq *xml-default-namespace* #+de.setf.xml (xqdm:find-namespace value) #-de.setf.xml (find-package value))))
  (:method ((name t) (value number))
    (write-char #\space *xml-output*)
    (encode-name name)
    (write-char #\= *xml-output*)
    (write-char #\' *xml-output*)
    (encode-number value)
    (write-char #\' *xml-output*))
  (:method ((name t) (value function))
    (write-char #\space *xml-output*)
    (encode-name name)
    (write-char #\= *xml-output*)
    (write-char #\' *xml-output*)
    (funcall value)
    (write-char #\' *xml-output*)))

(defun encode-element (element)
  (destructuring-bind (tag &rest content) element
    (declare (dynamic-extent content))
    (let ((gi (if (consp tag) (first tag) tag))
          (attributes (when (consp tag) (rest tag))))
      (write-char #\< *xml-output*)
      (encode-name gi)
      (write-char #\space *xml-output*)
      (loop for (name . value) in attributes
            do (encode-attribute name value))
      (write-char #\> *xml-output*)
      (encode-parsed-character-data content)
      (write-string "</" *xml-output*)
      (encode-name gi)
      (write-char #\> *xml-output*)
      (when *print-pretty* (encode-eol)))))

(defun encode-eol ()
  (write-string #.(make-array 2 :element-type 'character :initial-contents '(#\return #\linefeed))
                *xml-output*))

(defgeneric encode-name (name)
  (:method ((name symbol))
    (let* ((ns (symbol-package name))
           (ns-name (package-name ns))
           (local-part (symbol-name name)))
      (if (equalp local-part "")
        (progn (unless (equal ns-name "xmlns")
                 (error "invalid name: ~s." name))
               (write-string "xmlns" *xml-output*))
        (if (or (equal ns-name "") (eql *xml-default-namespace* ns))
          (format *xml-output* "~a" local-part)
          (format *xml-output* "~a:~a" ns-name local-part)))))
  #+de.setf.xml
  (:method ((name xqdm:uname))
    (let* ((ns (xmlp::namespace name))
           (ns-name (xmlp::namespace-name ns))
           (local-part (xmlp::local-part name)))
      (if (equal local-part "")
        (progn (unless (equal ns-name "xmlns")
                 (error "invalid name: ~s." name))
               (write-string "xmlns" *xml-output*))
        (if (or (equal ns-name "") (eql *xml-default-namespace* ns))
          (format *xml-output* "~a" local-part)
          (format *xml-output* "~a:~a" ns-name local-part))))))

(defun encode-format (control &rest args)
  (declare (dynamic-extent args))
  (encode-character-data (apply #'format nil control args)))
    

(defmacro xml (tag &rest body)
  (unless (listp tag) (setf tag (list tag)))
  (destructuring-bind (gi &rest attributes) tag
    `(let ((*xml-default-namespace* *xml-default-namespace*))
       (write-char #\< *xml-output*)
       (encode-name ',gi)
       (write-char #\space *xml-output*)
       ,@(mapcar #'(lambda (attribute)
                         (destructuring-bind (name  value) attribute
                           `(encode-attribute ',name ,value)))
                 attributes)
       ,(if body
          `(progn (write-char #\> *xml-output*)
                  ,@body
                  (write-string "</" *xml-output*)
                  (encode-name ',gi)
                  (write-char #\> *xml-output*))
          `(write-string "/>" *xml-output*))
       (when *print-pretty* (encode-eol)))))

;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: cl-user; -*-

;;; asdf system for simple introspective documentation library
;;;
;;; copyright 2010 james anderson

(in-package :cl-user)


(unless (find :cl-http *features*)
  (cerror "Continue anyway." "CL-HTTP must be present in order to configure the xml interface."))

(asdf:defsystem :de.setf.documentation.service
  :version "0.1"
  :depends-on (:de.setf.documentation)
  :serial t
  :description "A documentation module extension as an http service."
  :components ((:file "url")
               (:file "parameters")
               (:file "xhtml-encoding")
               (:file "xhtml-interface")
               (:file "exports"))

  :long-description "This system extends :de.setf.documentation to provide an http-based
 interface to system operations through a single web page.")



#+(or)
(progn
  (asdf:load-system :org.cl-http)
  (asdf:load-system :de.setf.documentation.server)
  ;; start
  (de.setf.utility.implementation.xml::export-documentation-urls)
  )

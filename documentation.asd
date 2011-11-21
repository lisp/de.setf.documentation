;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: cl-user; -*-

;;; asdf system for simple introspective documentation
;;;
;;; copyright 2010 james anderson

(in-package :cl-user)

(asdf:defsystem :de.setf.documentation
  :version "0.1"
  :depends-on (:net.common-lisp.closer-mop
               :de.setf.utility.walker
               :de.setf.xml)
  :serial t
  :description "An introspective ASDF-aware documentation tool."
  :components ((:file "package")
               (:file "parameters")
               (:file "metadata")
               (:file "system-walker")
               (:file "operations")
               (:file "xhtml-encoding")
               (:file "system-graph")
               (:file "system-prototype")
               (:file "system-operations")
               (:file "n3")
               ;; (:file "dot-encoding")
               ;; (:file "md-encoding")
               )

  :long-description "This system combines the definition facilities in ASDF with the
 introspection mechanisms inherent in common lisp and clos to construct structural models of
 running systems and present them in textual and graphical form.")





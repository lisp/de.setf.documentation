;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation.xml; -*-


(in-package :de.setf.utility.implementation.xml)

(:documentation  "This file implements an xhtml encoding for the documentation module of the
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
  "Implements encode-documentation-as methods"))


(:documentation encode-documentation-as
  "File-level encoding accepts two principal arguments classes

 - asdf:file-component : emits a document template as per the component's definition and
 then encodes one passage each for the constituent definitions in sort order.
 - package : emits a document template which includes the package's documentation string
 and then emits a reference for each symbol, sorted and divided by binding type.

 everything else is intended for {xhtml}div encoding only. if passed as complete document
 content, a minimal document with just the pertinent division is generated.")


(defmethod encode-documentation-as ((object t) (stream stream) (as mime:text/xhtml) &key)
  (with-xml-writer (stream)
    (encode-xml-declaration)
    (encode-document-type "xhtml" :public xqdm:+xhtml-public-identifier+
                          :system xqdm:+xhtml-system-identifier+)
    (let ((*print-case* :downcase)
          (css-uri (definition-documentation-relative-url #p"documentation.css")))
      (xml ({xhtml}html ({xmlns}||  "http://www.w3.org/1999/xhtml"))
           (let ((*print-pretty* t))
             (xml {xhtml}head
                  (({}meta ({}name "date") ({}content  (iso-time))))
                  (({}meta ({}http-equiv "Content-Type")
                           ({xhtml}content "text/xhtml;charset=iso-8859-1")))
                  (({}link ({}href css-uri) ({}rel "stylesheet") ({}type "text/css")))
                  (encode-instance-as object stream '{xhtml}title)))
           ({xhtml}body
            (encode-instance-as object stream '{xhtml}div)
            ({xhtml}hr)
            (({xhtml}div ({}class "footer"))
             (({xhtml}div ({}style "float: left;"))
              (({xhtml}img ({}src "http://www.digitool.com/img/mcl-made-1.gif"))))
             (({xhtml}div ({}style "float: right;"))
              (encode-format " Copyright ~a setf.de" (date:year)))))))))
       


;;;
;;; title elements

(defmethod encode-instance-as ((object asdf:source-file) (stream stream) (as (eql '{xhtml}title)) &key)
   (xml ({xhtml}title )
       (encode-format "FILE: ~a" (asdf:component-name object))))

(defmethod encode-instance-as ((object definition-component) (stream stream) (as (eql '{xhtml}title)) &key)
  (encode-instance-as (definition-object object) stream as))

(defmethod encode-instance-as ((object package) (stream stream) (as (eql '{xhtml}title)) &key)
  (xml ({xhtml}title )
       (encode-format "PACKAGE: ~a" (package-name object))))

(defmethod encode-instance-as ((object package) (stream stream) (as (eql '{xhtml}title)) &key)
  (xml ({xhtml}title )
       (encode-format "PACKAGE: ~a" (package-name object))))

(defmethod encode-instance-as ((object asdf:system) (stream stream) (as (eql '{xhtml}title)) &key)
  (xml ({xhtml}title )
       (encode-format "SYSTEM: ~a" (asdf:component-name object))))

(defmethod encode-instance-as ((object t) (stream stream) (as (eql '{xhtml}title)) &key)
  (xml ({xhtml}title )
       (encode-escaped-format "~a" object)))


;;;
;;; div elements

(defmethod encode-instance-as ((object c2mop:direct-slot-definition) (stream stream) (as (eql '{xhtml}div)) &key)
  (let ((name (c2mop:slot-definition-name object))
        (type (c2mop:slot-definition-type object))
        (doc (documentation object)))
    (xml ({xhtml}div ({}class "slot"))
         (({xhtml}div ({}class "name"))
          (xml ({xhtml}a ({}name (format nil "#~a" name))) (encode-format "~(~a~)" name)))
         (when type
           (encode-format "[~a]" type))
         (when doc
           (xml ({xhtml}div ({}class "documentation"))
                (encode-node (definition-annotated-documentation doc))))))
  (encode-newline))


(defmethod encode-instance-as ((object class-definition) (stream stream) (as (eql '{xhtml}div)) &key)
  (let* ((class (definition-object object))
         (supers (when class (c2mop:class-direct-superclasses class)))
         (name (princ-to-string (definition-name object))))
    (xml ({xhtml}div ({}class "class"))
         (({xhtml}a ({}name name))
          (({xhtml}div ({}class "type")) (encode-string "Class"))
          (({xhtml}div ({}class "name")) (encode-string name))
          (({xhtml}div ({}class "superclasses"))
           (encode-format "(~@[ ~{~a~^  ~} ~])" (mapcar #'class-name supers))))
         (({xhtml}div ({}class "documentation"))
          (encode-newline)
          (encode-node (definition-annotated-documentation object)))
         (encode-newline)
         (({xhtml}div ({}class "slots"))
          (when class 
            (dolist (sd (c2mop:class-direct-slots class))
              (encode-instance-as sd stream '{xhtml}div))))))
  (encode-newline))


(defmethod encode-instance-as ((object function-definition) (stream stream) (as (eql '{xhtml}div)) &key)
  (let ((name (princ-to-string (definition-name object))))
    (xml ({xhtml}div ({}class "function"))
         (({xhtml}a ({}name name))
          (({xhtml}div ({}class "type")) (encode-string "Function"))
          (({xhtml}div ({}class "name")) (encode-string name))
          (({xhtml}div ({}class "parameters")) (encode-escaped-format "~@[ ~{~a~^  ~} ~]" (definition-lambda-list object))))
         (({xhtml}div ({}class "documentation"))
          (encode-newline)
          (encode-node (definition-annotated-documentation object)))))
  (encode-newline))


(defmethod encode-instance-as ((object method-definition) (stream stream) (as (eql '{xhtml}div)) &key)
  (let* ((name (princ-to-string (definition-name object)))
         (method (definition-object object))
         (qualifiers (method-qualifiers method))
         (specializers (c2mop:method-specializers method))
         (lambda-list (definition-lambda-list object))
         (non-required (member-if #'(lambda (parameter) (member parameter lambda-list-keywords))
                                  lambda-list))
         (required (ldiff lambda-list non-required)))  
    (when (string-equal name "(setf prototypal-property-value)") (print method))
    (xml ({xhtml}div ({}class "method") ({}name name) ({}ID name))
         (({xhtml}div ({}class "type")) (encode-string "Method"))
         (({xhtml}div ({}class "name"))
          (let* ((function (c2mop:method-generic-function method))
                 (definition (when function (dsw:find-definition function dsw:*walker*))))
            (if definition
              (xml ({xhtml}a ({}href (definition-documentation-relative-url definition))) (encode-string name))
              (encode-string name)))
          (encode-format "~(  ~{~s~^  ~} ~)" qualifiers))
         (({xhtml}div ({}class "parameters"))
          (mapc #'(lambda (parameter specializer)
                    (encode-format " (~a ~a)"
                                   parameter (de.setf.utility.implementation::specializer-name specializer)))
                required specializers)
          (encode-escaped-format "~{ ~a~}" non-required))
         (({xhtml}div ({}class "documentation"))
          (encode-newline)
          (encode-node (definition-annotated-documentation object)))))
  (encode-newline))


(defmethod encode-instance-as ((object generic-function-definition) (stream stream) (as (eql '{xhtml}div)) &key)
  (let ((name (princ-to-string (definition-name object))))
    (xml ({xhtml}div ({}class "function"))
         (({xhtml}a ({}name name))
          (({xhtml}div ({}class "type")) (encode-string "Generic Function"))
          (({xhtml}div ({}class "name")) (encode-string name))
          (({xhtml}div ({}class "parameters")) (encode-escaped-format "~@[ ~{~a~^  ~} ~]" (definition-lambda-list object))))
         (({xhtml}div ({}class "documentation"))
          (encode-newline)
          (encode-node (definition-annotated-documentation object)))))
  (encode-newline))


(defmethod encode-instance-as ((object asdf:source-file) (stream stream) (as (eql '{xhtml}div)) &key)
  (when *load-verbose*
    (format *trace-output* "~&;;; encode-instance-as (~a ~a ~a)"
            object stream as))
  (xml ({xhtml}div ({}class "file"))
       (({xhtml}div ({}class "type")) (encode-string "File"))
       (({xhtml}div ({}class "name")) (encode-format "~a" (file-namestring (asdf:component-pathname object))))
       ({xhtml}hr)
       (let ((index ()))
         (labels ((definition-key (definition)
                    (let ((name (definition-name definition)))
                      (etypecase name
                        (symbol (symbol-name name))
                        (cons (symbol-name (second name))))))
                  (add-entry (definition)
                    (let* ((initial (char (definition-key definition) 0))
                           (entry (assoc initial index)))
                      (if entry
                        (pushnew definition (rest entry))
                        (push (list initial definition) index)))))

           ;; collate the file's the definitional
           (dolist (definition (de.setf.utility.implementation::component-definitions object))
             (add-entry definition))

           (labels ((encode-documentation (element)
                      (typecase element
                        (null )
                        (symbol (encode-reference element))
                        (string (encode-docstring element))
                        (cons (encode-code element))))
                    (encode-docstring (element)
                      (xml ({xhtml}div ({}class "documentation"))
                           (encode-node (definition-annotated-documentation element)))
                      (encode-newline))
                    (encode-code (form)
                      (xml ({xhtml}div ({}class "documentation"))
                           ({xhtml}hr)
                           (({xhtml}div ({}class "code"))
                            (encode-character-data
                             (let ((*print-case* :downcase))
                               (write-to-string form :pretty t))))))
                    (encode-reference (name)
                      (xml ({xhtml}div ({}class "documentation"))
                           (({xhtml}div ({}class "name"))
                            (xml ({xhtml}a ({}href (format nil "#~a" name)) ({}style "margin-right: 0.5em"))
                                 (encode-format "~a " name))))))

             (let ((description (asdf:component-long-description object)))
               (when description
                 (encode-documentation description)
                 (xml {xhtml}hr)))
             
             ;; sort and encode the index
             (setf index (sort index #'char-lessp :key #'first))
             (xml ({xhtml}div ({}class "indexHeading"))
                (loop for (initial . nil) in index
                      do (xml {xhtml}span
                              (xml ({xhtml}a ({}href (format nil "#index_~c" initial)))
                                   (encode-string (string initial)))
                               (encode-string " "))))
             (xml {xhtml}hr)
             (encode-newline)

             ;; add the documentation entries from the source
             (let ((documentation (asdf::component-file-documentation object)))
               (when documentation
                 (mapc #'encode-documentation documentation)
                 (xml {xhtml}hr)
                 (encode-newline))))
           
           ;; encode the individual definitions
           (loop for (initial . definitions) in index
                 do (progn
                      (dotimes (x 2) (encode-newline))
                      (xml ({xhtml}div ({}class "indexLetter"))
                           (({xhtml}a ({}name (format nil "index_~c" initial)))
                            (encode-string (string initial))))
                      (dolist (definition (sort definitions #'string-lessp :key #'definition-key))
                        (dotimes (x 2) (encode-newline))
                        (encode-instance-as definition stream '{xhtml}div)
                        (xml ({xhtml}hr ))))))))
  (encode-newline))

(defmethod encode-instance-as ((object asdf:system) (stream stream) (as (eql '{xhtml}div)) &key)
  (when *load-verbose*
    (format *trace-output* "~&;;; encode-instance-as (~a ~a ~a)"
            object stream as))
  (xml ({xhtml}div ({}class "file"))
       (({xhtml}div ({}class "type")) (encode-string "System"))
       (({xhtml}div ({}class "name")) (encode-format "~a" (asdf:component-name object)))
       ({xhtml}hr)
       (({xhtml}div ({}class "documentation"))
            (encode-node (definition-annotated-documentation (asdf:system-long-description object))))
       ({xhtml}hr)

       (labels ((encode-component (component)
                  (typecase component
                    (asdf:source-file (encode-file component))
                    (asdf:module (encode-module component))))
                (encode-file (component)
                  (let* ((uri (definition-documentation-uri component))
                         (name (asdf:component-name component))) (encode-newline)  (encode-newline)
                        (encode-newline)
                        (xml ({xhtml}div ({}style "display: table-row;"))
                             (encode-newline)
                             (({xhtml}div ({}class "name") ({}style "width: 2in; display: table-cell;  padding-bottom: 1ex; "))
                              (({xhtml}a ({}href uri)) (encode-string name)))
                             (({xhtml}div ({}style "width: 5.5in; display: table-cell; padding-bottom: 1ex; border-bottom: 1px grey solid;"))
                              (loop for (name . definition)
                                    in (sort (mapcar #'(lambda (def) (cons (princ-to-string (definition-name def)) def))
                                                     (de.setf.utility.implementation::component-definitions component))
                                             #'string-lessp :key #'first)
                                    do (encode-newline)
                                    do (xml ({xhtml}div ({}style "display: inline-block; margin-right: .5em;"))
                                            (({xhtml}a ({}href (definition-documentation-relative-url definition)))
                                             (encode-format "~a" name))))))))
                (encode-module (component)
                  (let* ((name (asdf:component-name component)))
                    (xml ({xhtml}div ({}style "display: table-row;"))
                         (({xhtml}div ({}class "name") ({}style "width: 2in; display: table-cell;  padding-bottom: 1ex; "))
                          (encode-string name))
                         (({xhtml}div ({}style "width: 5.5in; display: table-cell; padding-bottom: 1ex; border-bottom: 1px grey solid;"))
                          (mapc #'encode-component (asdf:module-components component)))))))
         (xml ({xhtml}div ({}style "display: table; padding-left: .5in; padding-right: .5in; "))
              (mapc #'encode-component (sort (copy-list (asdf:module-components object))
                                             #'string-lessp
                                             :key #'asdf:component-name))))
       ({xhtml}hr))
  (encode-newline))


(defmethod encode-instance-as ((object package-definition) (stream stream) (as (eql '{xhtml}div)) &key)
  (encode-instance-as (definition-object object) stream as))

(defmethod encode-instance-as ((object package) (stream stream) (as (eql '{xhtml}div)) &key)
  (xml ({xhtml}div ({}class "documentation package"))
       (({xhtml}div ({}class "type")) (encode-string "Package"))
       (({xhtml}div ({}class "name")) (encode-format "~a" (package-name object)))
       ({xhtml}hr)
       (({xhtml}div ({}class "documentation"))
            (encode-node (definition-annotated-documentation (documentation object 'package))))
       ({xhtml}hr)
       (let ((index ()))
         (flet ((add-entry (symbol)
                  (let* ((initial (char (symbol-name symbol) 0))
                         (entry (assoc initial index)))
                    (if entry
                      (pushnew symbol (rest entry))
                      (push (list initial symbol) index)))))

           ;; collect al symbols with bindings
           (with-package-iterator (next object :internal :external)
             (loop (multiple-value-bind (next-p symbol) (next)
                     ;; if the symbol is associated with any metadata include it.
                     (unless next-p (return))
                     (when (dsw:walker-node-property dsw:*walker* symbol :values)
                       (add-entry symbol)))))

           ;; sort the index
           (setf index (sort index #'char-lessp :key #'first))

           ;; encode an header index
           (xml ({xhtml}div ({}class "indexHeading"))
                (loop for (initial . nil) in index
                      do (xml {xhtml}span
                              (({xhtml}a ({}href (format nil "#index_~c" initial))))
                              (encode-string (string initial)))
                      (encode-string " "))))

           ;; encode the known symbols sectiond by initial
           (loop for (initial . symbols) in index
                 do (progn
                      (encode-newline)
                      (xml {xhtml}hr)
                      (xml ({xhtml}a ({}name (format nil "index_~c" initial))
                                     ({}class "indexLetter"))
                           (encode-string (string initial)))
                      (dolist (symbol (sort symbols #'string-lessp))
                        (xml {xhtml}div
                             (encode-newline)
                             (encode-format "~a : " symbol)
                             (dolist (value (sort (copy-list (dsw:walker-node-property dsw:*walker* symbol :values))
                                                   #'string-lessp :key #'type-of))
                               (flet ((encode-definition (definition)
                                        (when definition
                                          (xml {xhtml}span
                                               (let* ((uri (definition-documentation-relative-url definition))
                                                      (category (definition-category definition)))
                                                 (encode-string (string #\space))
                                                 (xml ({xhtml}a ({}href uri))
                                                      (encode-string category) ))))))
                                 ;; (encode-newline)
                                 " "
                                 (encode-definition (dsw:find-definition value dsw:*walker*))
                                 ;; (encode-newline)
                                 " "
                                 (encode-definition (dsw:find-definition `(setf value) dsw:*walker*))))))))))
  (encode-newline))


;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-


(in-package :de.setf.utility.implementation)




(defParameter *xml-declaration* '((:|?xml| (:|version| . "1.0") (:|encoding| . "iso-8859-1"))))
(defParameter *stylesheet-declaration* '((:|?xml-stylesheet| (:|href| . "/Library/CSS/documentation.css") (:|type| . "text/css"))))
(defParameter *doctype-declaration* '((:|!DOCTYPE| :|xhtml|
                                                   (:PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "../DTD/xhtml1-strict.dtd"))))
(defParameter *document-title* "Untitled")
(defParameter *document-topic* "Untitled")
(defParameter *document-date* "Untitled")
(defParameter *document-timestamp* "Untitled")

(defParameter *document-head* '(:|head|
                                ((:|meta| (:|http-equiv| . "Content-Type") (:|content| . "text/xml;charset=iso-8859-1")))
                                ((:|title| *document-title*))))

(defParameter *page-head* '(:|pageHead| (:|logo| "cl-xml") (:|topic| *document-topic*) (:|date| *document-date*)
                            (:|contact| ((:|a| (:|href| . "mailto:cl-xml@setf.de") "setf.de")))))

(defParameter *page-navigation* '(:|navigation|
                                  " [" ((:|a| (:|href| . "#introduction")) "introduction") 
                                  "] [" ((:|a| (:|href| . "#types")) "types")
                                  "] [" ((:|a| (:|href| . "#variables")) "variables")
                                  "] [" ((:|a| (:|href| . "#functions")) "functions")
                                  "] [" ((:|a| (:|href| . "#examples")) "examples") "] "))






;;;
;;; manifestation


(defmethod manifest ((object package) (expression  (eql :documentation)) (processor html-generator)
                     &key operators macros special-forms variables method-combinations types)
  "the html manifestions for a package is a wrapper for the documentation for all definitions in the package.
 it takes the form of a cover-page with a segmented index in one pane and a category-specific page
 in the other pane.
 OBJECT : PACKAGE 
 EXPRESSION : (eql :documentation)
 PROCESSOR : html-generator
 :DOCUMENTATION : STRING : the package documentation string
 :OPERATORS : list of operator names
 :macros : list of macro names
 :special-forms : list of special form names
 :variables : list of variable names
 :method-combinations
 :types : list of defined type names"
 presentation in the other.
  (declare (dynamic-extent args))

  (mxml:with-generator-stream (stream processor)
    (mxml:with-document-encoding (:stream stream)
      (mxml:with-element-encoding (|xhtml|:|frameset| :stream stream)
        (mxml:with-element-encoding (|xhtml|:|frame| :stream stream)
          ;; the index frame
          )
        (mxml:with-element-encoding (|xhtml|:|frameset| :stream stream)
          ;; the content frame
          )))))

(defGeneric encode-annotations-as-xml (annotations stream)
  (let ((collated (collate #'annotation-nature annotations :test #'equal)))
    (encode-parsed-data *xml-declaration* stream)
    (encode-parsed-data *sytlesheet-instruction* stream)
    (encode-start-tag :|manual| '((:|xmlns| . "http://www.w3.org/XML/1998/namespace")
                                  (:|xmlns|:|html| . "http://www.w3.org/1999/xhtml"))
                      stream)
    (encode-parsed-data *head-element* stream)
    (encode-parsed-data *page-header* stream)
    (encode-parsed-data *page-navigation* stream)
    (dolist (annotations-per-nature collated)
      (destructuring-bind (*header-text* . annotations) annotations-per-nature
        (encode-parsed-data *h3-element* stream)
        (encode-start-tag :|definitions| nil stream)
        (dolist (annotation (sort annotations #'string-lessp :key #'annotation-name))
          (encode-annotation annotation stream))
        (encode-end-tag :|definitions| stream)))
    (encode-end-tag :|manual| stream)))




#|
(defGeneric generate-documentation (context destination)
  (:method ((source pathname) (destination t))
           (generate-documentation (collect-list (accumulate-annotation) (call-with-annotations #'collect-list source))
                                   destination))
  (:method ((source package) (destination t))
           (generate-documentation (collect-list (accumulate-annotation) (call-with-annotations #'collect-list source))
                                   destination))
  (:method ((annotations list) (destination pathname))
           "if the destination is a directory, then partition the descriptions by package, generate a complete pathname, and recurse. otherwise segment the annotations into categories, sort them within category and generate the description. the documentation form depends on the specified mime type. the types are text/xml and text/dot are recognized."
           (let ((mime-type (pathname-mime-type destination)))
             (unless mime-type
               (warn "no mime type specified: ~s." destination)
               (setf mime-type mime:text/xml))
             (case (pathname-name destination)
               ((nil :unspecific)       ; partition the descriptions by package and generate a separate document for each
                (dolist (per-package-annotations (collate #'annotation-name-package annotations))
                  (destructuring-bind (package . annotations) per-package-annotations
                    (let ((destination (make-pathname :name (package-name package) :defaults destination)))
                      (with-resource-processor (processor mime-type :pathname destination)
                        (generate-documentations annotations processsor))))))
               (t
                (with-resource-processor (processor mime-type :pathname destination)
                  (generate-documentations annotations processor))))))

  (:method ((annotations list) (processor mime::text/dot-processor))
           (encode-annotations-as-dot annotations processor))

  (:method ((annotations list) (processor mime::text/xml-processor))
           (encode-annotations-as-xml annotations processor)))
|#
             
#|

(defGeneric encode-annotations-as-xml (annotations stream)
  (let ((collated (collate #'annotation-nature annotations :test #'equal)))
    (encode-parsed-data *xml-declaration* stream)
    (encode-parsed-data *sytlesheet-instruction* stream)
    (encode-start-tag :|manual| '((:|xmlns| . "http://www.w3.org/XML/1998/namespace")
                                  (:|xmlns|:|html| . "http://www.w3.org/1999/xhtml"))
                      stream)
    (encode-parsed-data *head-element* stream)
    (encode-parsed-data *page-header* stream)
    (encode-parsed-data *page-navigation* stream)
    (dolist (annotations-per-nature collated)
      (destructuring-bind (*header-text* . annotations) annotations-per-nature
        (encode-parsed-data *h3-element* stream)
        (encode-start-tag :|definitions| nil stream)
        (dolist (annotation (sort annotations #'string-lessp :key #'annotation-name))
          (encode-annotation annotation stream))
        (encode-end-tag :|definitions| stream)))
    (encode-end-tag :|manual| stream)))

(defMethod encode-parsed-data ((annotation comment-annotation) stream)
  (let ((text (annotate-documentation-text (annotation-string annotation))))
    (typecase text
      (condition (format stream "~%~%error parsing documentation:<pre>~%~a~%</pre>" text))
      (string (encode-parsed-data text stream))
      (cons (case (first text)
              ((:|DOCUMENTATION| :|documentation|)
               (sxml:element-bind ((name) description copyright chronology) text
                 (declare (ignore name))
                 (terpri stream)
                 (format stream "~%</definitions>~%<DOCUMENTATION>~%")
                 (encode-document description stream)
                 (when copyright
                   (sxml:element-bind ((name year author (|href| :|href|) (href :href |href|))) copyright
                     (declare (ignore name))
                     (format stream "~%<COPYRIGHT><YEAR>~A</YEAR><AUTHOR>~A</AUTHOR>" year author)
                     (when href
                       (format stream "~% see <a href='~a'>~a</a>" href href))
                     (format stream "</COPYRIGHT>")))
                 (when chronology
                   (format stream "~%<CHRONOLOGY>")
                   (dolist (delta (sxml:content chronology))
                     (sxml:element-bind ((name date author)) delta
                       (declare (ignore name))
                       (format stream "~% <DELTA>")
                       (if date (format stream "<DATE>~A</DATE>" date) (write-string "<DATE/>" stream))
                       (if author (format stream "<AUTHOR>~A</AUTHOR>" author) (write-string "<AUTHOR/>" stream))
                       (format stream "<COMMENT>")
                       (dolist (content (sxml:content delta)) (encode-document content stream))
                       (format stream "</COMMENT></DELTA>")))
                   (format stream "~% </CHRONOLOGY>"))
                 (format stream "~%</DOCUMENTATION>~%<definitions>~%")))
              (:|div| (let ((open-count (count #\( (content text) :test #'eql))
                            (close-count (count #\) (content text) :test #'eql)))
                        (if (and (plusp open-count) (= open-count close-count))
                          (encode-parsed-data (cons :|pre| (content text)) stream)
                          (encode-parsed-data text stream))))
              (t
               (encode-parsed-data text stream)))))))




|#



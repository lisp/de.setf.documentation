;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-


(in-package :de.setf.utility.implementation)



;;
;;
;;

(defparameter *documentation-terms*
  '((package . package) (packages . package) (class . class) (classes . class)
    (instance . class) (instances . class) (specialized . class) (specializes . class)
    (return . class) (returns . class) (delegate . function) (macro . function) (macros . function)
    (function . function) (functions . function) (method . function) (binding . variable) (bindings . variable)
    (methods . function) (accessor . function) (accessors . function) (bind . function) (binds . function)
    (type . type) (types . type) (argument . class) (arguments . class)))

(defParameter *common-lisp-package* (find-package "COMMON-LISP"))

(defParameter *description-package* (find-package :de.setf.utility.documentation))

(defParameter *xml-declaration* '((:|?xml| (:|version| . "1.0") (:|encoding| . "iso-8859-1"))))
(defParameter *stylesheet-declaration* '((:|?xml-stylesheet| (:|href| . "/Library/CSS/documentation.css") (:|type| . "text/css"))))
(defParameter *doctype-declaration* '((:|!DOCTYPE| :|xhtml|
                                                   (:PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "../DTD/xhtml1-strict.dtd"))))
(defParameter *document-title* "Untitled")
(defParameter *document-topic* "Untitled")
(defParameter *document-date* "Untitled")
(defParameter *document-timestamp* "Untitled")

(defParameter *document-stream* nil)
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


(defGeneric annotate-cross-references (documentation)
  (:method ((documentation string))
           (annotate-cross-references
            (split-string documentation *documentation-punctuation* :punctuation-p t)))
  (:method ((documentation list) &aux type)
           (labels ((conjunction-p (word)
                      (member word *doccumentation-conjunctions* :test #'string-equal))
                    (name-character-p (c)
                      (or (alphanumericp c) (find c "-_")))
                    (anchor-value (word)
                      (if (find-if (complement #'name-character-p) word)
                        (substitute-if #\_ (complement #'name-character-p)  word)
                        word))
                    (relative-url-value (package-name)
                      (format nil "./~a.html" package-name))
                    (annotate-word (word &aux symbol description)
                      (typecase word
                        (string (if type
                                  (cond ((and (not (conjunction-p word))
                                              (setf description (description-as word type))
                                              (setf symbol (annotation-name description)))
                                         (let ((definition-package-name (package-name symbol)))
                                           `(:|definition-reference|
                                             ((:|a| (:|href| . ,(format nil "~a#~a"
                                                                        (relative-url-value definition-package-name)
                                                                        (anchor-value word))))
                                              ,word))))
                                        (t
                                         word))
                                  word))
                        (t
                         word)))
                    (note-documentation-type (word)
                      (typecase word
                        (string
                         (unless type
                           (setf type (rest (assoc word *documentation-terms* :test #'string-equal)))))
                        (t ))))
             (mapcar #'annotate-word documentation))))


(defGeneric annotate-documentation-text (string)
  (:method ((documentation-text string))
           (if (plusp (length documentation-text))
             (if (char= #\< (char documentation-text 0))
               (annotate-documentation-text (read-document documentation-text))
               (list :|div| (annotate-cross-references documentation-text)))
             documentation-text))
  (:method ((documentation-text cons))
           (destructuring-bind (annotations . content) documentation-text
             (cons annotations
                   (collect-list (collect)
                     (dolist (element content) (collect (annotate-documentation-text element)))))))
  (:method ((datum t))
           datum))


|#
#|
this was an earlier implementation, retained here as a reminder.
is fuses the attribute enumeration and encoding implementation, and allows no transformation.

(defAnnotation (generic-function-annotation mime:text/docbook+xml processor)
  (({}generic-function )
   ({}method-combination (encode-annotation annotation-function-method-combination processor))
   ({}methods (dolist (method annotation-function-methods) (encode-annotation method processor)))))

(defAnnotation (method-annotation mime:text/docbook+xml processor)
  (({}function )
   ({}speciailzers annotation-method-specializers)
   ({}qualifiers annotation-method-qualifiers)))
|#


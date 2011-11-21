;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation.xml; -*-


(in-package :de.setf.utility.implementation.xml)

(:documentation  "This file implements xhmtl encoding templates, macros, and components
 for the introspection service."
 
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

 (description
  "This code defines xhtml-encoding operators in the context of an xml-wwriter.
 The make reference to the response stream only and neither extract information from neither the request
 nor the server and do no redirection. Where the mime type determins the encoding form, the operator
 does establish the xml encoding context locally."))

(http::define-url-export-type :svg-image :svg (:image :svg+xml)
  ;; no cause it to hang :data-type :application
  :copy-mode :binary)
(http::define-url-export-type :n3-text :n3 (:text :n3)
  ;; no cause it to hang :data-type :application
  :copy-mode :binary)
(http::define-url-export-type :csv-text :csv (:text :csv)
  :copy-mode :text)
(http::define-url-export-type :ico-image :ico (:image :ico)
  :copy-mode :binary)

(http::define-url-export-type :dot-text :dot (:text :vnd.graphviz)
  :copy-mode :binary)

;;; (http::mime-content-type-primary-extension :text/csv)
;;; (http::mime-content-type-keyword '(:text :csv))
;;; (http::mime-content-type-keyword '(:text :vnd.graphviz))
;;; (http::mime-content-type-primary-extension (dsu:mime-type-minor-type mime:text/csv))


(defgeneric redirect-request (request server target)

  (:method ((request t) (server http::server) (target t))
    (www-utils:notify-log-window "Anomolous redirection (~a ~a ~a)." request server target)
    (error 'http::document-not-found
           :url request
           :reason (format nil "Anomolous redirection (~a ~a ~a)." request server target)))

  (:method ((request t) (server http::server) (target url:url))
    (http:redirect-request server target))

  (:method ((request t) (server http::server) (target string))
    (http:redirect-request server (export-pathname (pathname target))))

  (:method ((request t) (server http::server) (target pathname))
    (http:redirect-request server (export-pathname target))))


(defmethod request-mime-types ((server http::server) (request t))
  (http::with-header-values (accept) (http::server-headers server)
    (when accept
      (loop for media-type in (http::sort-accept-header-media-types accept)
            for mime-type = (ignore-errors (mime:mime-type media-type))
            when mime-type
            collect mime-type))))

(defun export-pathname (pathname &optional (url (pathname-url-namestring pathname)))
  "Returns the exported url and the local-context-relative namestring for use in documents.
 The parent directory must already have been exported."
  (let ((type (http:export-type-for-pathname-type (pathname-type pathname))))
    (values (http:export-url (http::merge-url url (http::local-context)) type
                             :pathname pathname)
            url)))

(defun intern-request-projection (projection)
  (rest (assoc projection `((:dot . ,*graphviz-dot*)
                            (:twopi . ,*graphviz-twopi*)
                            (:rdf . ,*rdf-projection*)
                            (:subject . ,*rdf-subject*)
                            (:predicate . ,*rdf-predicate*)
                            (:object . ,*rdf-object*))
               :test #'string-equal)))

(defun intern-request-relation (relation)
  (rest (assoc relation `((:system . :systems) (:systems . :systems)
                            (:file . :files) (:files . :files)
                            (:module . :modules) (:modules . :modules)
                            (:calls . :calls) (:call . :calls))
               :test #'string-equal)))



(defun transcode-file (pathname)
  (with-open-file (js-stream pathname :direction :input :element-type 'character)
    (let ((buffer (make-array (file-length js-stream) :adjustable t :fill-pointer 0 :element-type 'character)))
      (let ((char #\space))
        (loop (unless (setf char (read-char js-stream nil nil)) (return))
              (vector-push-extend char buffer)))
      (xmlp::encode-character-data buffer))))

(defun pathname-url-namestring (pathname &key (host (pathname-host pathname))
                                         (path (rest (pathname-directory pathname)))
                                         (name (pathname-name pathname))
                                         (type (pathname-type pathname)))
  (format nil "~(/~a/~)~{~a/~}~a.~a" host path name type))


#+mcl
(setf (ccl:assq 'encoding-system-response ccl:*fred-special-indent-alist*) 1)


(defun encode-stylesheet-resources (&optional (resources *stylesheet-resources*))
  "nb. link content type is EMPTY"
  (dolist (resource resources)
    (typecase resource
      (cons
       (destructuring-bind (url . attributes) resource
         (xmlp::encode-sexp-node `(({xhtml}link ({}href ,url) ,@attributes)))))
      (t
       (xml ({xhtml}link ({}rel "stylesheet") ({}type "text/css") ({}href resource)))))))

    
(defun encode-javascript-resources (&optional (resources *javascript-resources*))
  "nb. script content type is PCDATA"
  (dolist (resource resources)
    (typecase resource
      (cons
       (destructuring-bind (url . attributes) resource
         (xmlp::encode-sexp-node `(({xhtml}script ({}src ,url) ,@attributes) ""))))
      (t
       (xml ({xhtml}script ({}src resource) ({}type "text/javascript")) "")))))

(defun encode-header.div (&optional (content "Header Content"))
  (xml ({xhtml}div  ({}class "header"))
       (encode-character-data content)))

(defun encode-javascript-requires (classes)
  (xml ({}script ({}type "text/javascript"))
       (dolist (class classes)
         (encode-newline)
         (encode-format "dojo.require('~a');" class))))


;;


(defgeneric encode-system-projection (request stream system result projection mime-type)

  (:method ((request t) stream (system t) (model cons) (projection rdf-object-projection) (type mime:text/csv))
    "project the objects as .csv by emitting a CSL of the leaves."
    (http:with-successful-response (stream :text)
      (let ((first t))
        (labels ((emit-leaf (object)
                   (typecase object
                     (cons (dolist (relation (rest object))
                             (emit-leaf (second relation))))
                     (t (if first (setf first nil) (write-char #\, stream))
                        (format stream "~a" (definition-name object))))))
          (emit-leaf model)))
      (terpri stream)))

  (:method ((request t) stream (system t) (relations cons) (projection rdf-projection) (type mime:*/json))
    (http:with-successful-response (stream :text)
      (labels ((encode-json (thing)
                 (typecase thing
                   (cons (write-char #\{ stream)
                         (loop for things on thing
                               do (encode-json (first things))
                               unless (null (rest things))
                               do (write-string ", " stream))
                         (write-char #\} stream))
                   (package (encode-json (package-name thing)))
                   (asdf:component (encode-json (asdf:component-name thing)))
                   (t (format stream "'~a'" thing)))))
        (encode-json relations))
      (terpri stream)))
  
  (:method ((request t) stream (system t) (relations cons)  (projection rdf-projection) (type mime:*/html))
    (http:with-successful-response (stream :html)
      (let ((*print-case* :downcase))
        (xmlp:with-xml-writer (stream)
          (encode-xml-declaration)
          (encode-document-type "xhtml" :public xqdm:+xhtml-public-identifier+
                                :system xqdm:+xhtml-system-identifier+)
          (xml ({xhtml}html ({xmlns}||  "http://www.w3.org/1999/xhtml")) 
               ({xhtml}head
                (({}meta ({}name "date") ({}content  (iso-time))))
                (({}meta ({}http-equiv "Content-Type")
                         ({xhtml}content "text/xhtml;charset=iso-8859-1")))
                
                ({}title (encode-format "Introspective Analysis : ~a : Relations"
                                        (asdf:component-name (first relations))))
                ({xhtml}body
                 ({xhtml}pre
                  (encode-escaped-format "~:w" relations)))))
          (encode-newline)))))

  (:method ((request t) stream (system t) (relations cons)  (projection rdf-projection) (type mime:text/n3))
    (http:with-successful-response (stream :n3)
      (destructuring-bind (subject . rest) relations
        (setf subject (definition-uri subject))
        (if (consp rest)
          (if (consp (first rest))
            (dolist (relation rest)
              (destructuring-bind (predicate object) relation
                (typecase object
                  (cons (dolist (object object) (format stream "~&~/n3:format/ ." (list subject predicate object))))
                  (t (format stream "~&~/n3:format/ ." (list subject predicate object))))))
            (destructuring-bind (predicate object) rest
              (typecase object
                (cons (dolist (object object) (format stream "~&~/n3:format/ ." (list subject predicate object))))
                (t (format stream "~&~/n3:format/ ." (list subject predicate object))))))))
      (terpri stream))))





(defun generate-report-index (directory-path &optional (index-file
                                                        (make-pathname :name "index" :type "html" :defaults directory-path)))
  (let ((*print-case* :downcase) (*print-pretty* nil)
        (buffer (make-array 256 :element-type 'character :fill-pointer 0 :adjustable t)))
    (with-open-file (stream index-file :direction :output :if-exists :supersede :if-does-not-exist :create)
      (xmlp:with-xml-writer (stream)
        (encode-xml-declaration)
        (encode-document-type "xhtml" :public xqdm:+xhtml-public-identifier+
                              :system xqdm:+xhtml-system-identifier+)
        
        (xml ({xhtml}html ({xmlns}||  "http://www.w3.org/1999/xhtml")
                          ({xmlns}xlink  "http://www.w3.org/1999/xlink")) 
             ({xhtml}head
              (({}meta ({}name "date") ({}content  (iso-time))))
              (({}meta ({}http-equiv "Content-Type")
                       ({xhtml}content "text/xhtml;charset=iso-8859-1")))
              
              ({}title (encode-format "Introspective Analysis : Reports : ~{~a~^.~}"
                                      (last (pathname-directory directory-path) 3)))
              (({}style ({}type "text/css")) "
               .file { margin-top: 10px; border-top: solid gray 1px; }
               .file .name { font-weight: bold; }
               .file .content { white-space: pre; margin-left: 48px; }"))

             ({xhtml}body
              (({xhtml}div ({}class "header"))
               (encode-format "Introspective Analysis : Reports : ~{~a~^.~}"
                              (last (pathname-directory directory-path) 3)))
              (({xhtml}div ({}class "filelist"))
               (dolist (file-pathname (sort (directory (make-pathname :name :wild :type "txt" :defaults directory-path))
                                            #'string-lessp
                                            :key #'file-namestring))
                 (multiple-value-bind (url namestring) (export-pathname file-pathname)
                   (declare (ignore url))
                   (xml ({xhtml}div ({}class "file"))
                        (({xhtml}div ({}class "name")
                                     ({}onclick (list "showReport('" namestring "');")))
                         (encode-string (pathname-name file-pathname)))
                        (({xhtml}div ({}class "content"))
                         (({xhtml}a ({}href namestring) ({}target "report")
                                    ({}style "position: absolute; left: 4px;"))
                          "<-")
                         (with-open-file (file file-pathname :direction :input)
                           (let* ((char #\space)
                                  (length (file-length file))
                                  (second-half (min 128 (- length 128))))
                             (setf (fill-pointer buffer) 0)
                             (dotimes (x (min 128 length))
                               (unless (setf char (read-char file nil nil)) (return))
                               (vector-push char buffer))
                             (encode-character-data buffer)
                             (when (> second-half 0)
                               (when (> length 256)
                                 (encode-newline)
                                 (encode-character-data "[ ... ]")
                                 (encode-newline))
                               (setf (fill-pointer buffer) 0)
                               (file-position file (- length second-half))
                               (dotimes (x second-half)
                                 (unless (setf char (read-char file nil nil)) (return))
                                 (vector-push char buffer))
                               (encode-character-data buffer)))))))))))))))
;;  (generate-report-index #p"METADATA/

(defun collect-reports (root)
  (let ((base (translate-logical-pathname root)))
    (dolist (root (directory (print (make-pathname :directory (append (or (pathname-directory root) '(:absolute)) '(:wild))
                                                   :host (pathname-host root)
                                                   :name nil :type nil))
                             :directories t))
      (unless (member (nth (length (pathname-directory base)) (pathname-directory root)) '("lib" "reports") :test 'equalp)
        (dolist (pathname (directory (make-pathname :directory (append (pathname-directory root) '(:wild-inferiors))
                                                    :name :wild :type "txt")))
          (multiple-value-bind (sec min hour day month year) (decode-universal-time (file-write-date pathname))
            (declare (ignore sec min hour))
            (let ((to-pathname (parse-namestring (format nil "METADATA:reports;~4,'0d;~2,'0d;~2,'0d;~a--~a.~a"
                                                         year month day
                                                         (first (last (pathname-directory pathname)))
                                                         (pathname-name pathname)
                                                         (pathname-type pathname)))))
              (ensure-directories-exist to-pathname)
              (rename-file pathname to-pathname))))))))

;;; (collect-reports #p"METADATA:")
;;; (generate-report-index #P"METADATA:reports;2010;03;22;")


(defun encode-system-operation-results (request stream system operation &key (return-code 0) aspects)
  "Iff the operation succeeded (rc == 0), then encode the status in-line as the response.
 Otherwise, signal an error and pass the encode operation to include the report in the error mesage."

  (flet ((encode-operation-report (request stream)
           (declare (ignore request))
           (let ((*print-case* :downcase) (*print-pretty* t))
             (xmlp:with-xml-writer (stream)
               ;; encode just the body content as it happenes either here, inline, or as part of
               ;; an error reoprt, in which case the body is already started.
               
               (xml {}div
                    (({xhtml}div ({}class "header"))
                     (encode-format ";;; ~/date:format-iso-time/ : ~a : ~a~@[ return code: ~s~]~%~%----~%"
                                    (get-universal-time) system operation return-code))
                    (({xhtml}div ({}id "Results"))
                     ({xhtml}h3  "Results")
                     ({xhtml}ul
                      (loop for (title pathname) in aspects
                            for id from 0 by 1
                            do (let ((url-string (nth-value 1 (export-pathname pathname))))
                                 (xml {xhtml}li (({xhtml}a ({}href url-string))
                                                 (encode-string title))))))))))))
    (if (zerop return-code)
      (http:with-successful-response (stream :html)
        (let ((*print-case* :downcase) (*print-pretty* t))
          (xmlp:with-xml-writer (stream)
            (encode-xml-declaration)
            (encode-document-type "xhtml" :public xqdm:+xhtml-public-identifier+
                                  :system xqdm:+xhtml-system-identifier+)
            (xml ({xhtml}html ({xmlns}||  "http://www.w3.org/1999/xhtml")
                              ({xmlns}xlink  "http://www.w3.org/1999/xlink")) 
                 ({xhtml}head
                  (({}meta ({}name "date") ({}content  (iso-time))))
                  (({}meta ({}http-equiv "Content-Type")
                           ({xhtml}content "text/xhtml;charset=iso-8859-1")))
                  
                  ({}title (encode-format "Introspective Analysis : ~a : ~a"
                                          (asdf:component-name system) operation)))
                 ({xhtml}body
                  (encode-operation-report request stream))))))
      ;; a non-zero return code is reported as a 'server error'
      (error 'http::server-internal-error :reason (format nil "Operation failed: return code: ~s." return-code)
             :reporter #'encode-operation-report))))

(defun encode-system-operation-failure (system request stream operation &rest aspects
                                               &key (return-code nil)
                                               &allow-other-keys)
  "When the operation fails, signal an error and pass a report to include details in the error mesage."
  (declare (ignore stream))
  (flet ((encode-failure-report (request stream)
           (declare (ignore request))
           (xmlp:with-xml-writer (stream)
             (xml ({xhtml}div ({xmlns}||  "http://www.w3.org/1999/xhtml"))
                  (({xhtml}div ({}id "Results"))
                   ({xhtml}h3  (encode-format "Results : ~/date:format-iso-time/ : ~a : ~a~@[ : ~a~]"
                                  (get-universal-time) (asdf:component-name  system) operation return-code))
                   ({xhtml}ul
                    (loop for (aspect value) on aspects by #'cddr
                          do (typecase value
                               (pathname (let ((url-string (nth-value 1 (export-pathname value))))
                                           (xml {xhtml}li
                                                (encode-format "~a : " aspect)
                                                (({xhtml}a ({}href url-string) ({}target (file-namestring value)))
                                                 (encode-format "~a" (file-namestring value))))))
                               (null )          ; skip it
                               (t (xml {xhtml}li (encode-format "~a : ~a" aspect value)))))))))))

    (error 'http::server-internal-error
           :url request
           :reason (format nil "Operation failed: ~a ~@[: return code: ~s~]."
                           operation return-code)
           :reporter #'encode-failure-report)))



(defun encode-footer ()
  (xml ({xhtml}div ({}class "footer")
                   ({}style "position: absolute; bottom: 24px; width: 99%; border-top: groove; margin-right: 4px; padding-top: 2px;"))
       (({xhtml}div ({}style "position: fixed; left: 4px; font-size: small;"))
        (({xhtml}img ({}src "http://www.digitool.com/img/mcl-made-1.gif"))))
       (({xhtml}div ({}style "position: fixed; right: 4px; font-size: small;"))
        ({xhtml}div (encode-format "Copyright ~a setf.de" (date:year)))
        ({xhtml}div (encode-format "~a" http::*server-version*)))))


#|
not used
(defmacro encoding-system-response ((stream system-name &key (type :html) (title-format "~a")) &rest body)
  (flet ((assoc-element (name)
           ;; just in case the head orbody element have attributes
           ;; nb. head attributes are ignored
           (assoc-if #'(lambda (tag) (or (eq tag name) (and (consp tag) (eq (car tag) name))))
                     body)))
    (let* ((head-form (assoc-element '{xhtml}head))
           (body-form (assoc-element '{xhtml}body))
           (other-forms (remove head-form (remove body-form body))))
      (when (consp (first head-form))
        (warn "head attributes ignroed: ~a." (first head-form)))
      `(flet ((.head () ,@(mapcar #'(lambda (form) (cons 'xml form)) (rest head-form)))
              (.body () ,(cons 'xml body-form))
              (.other () ,@(mapcar #'(lambda (form) (cons 'xml form)) other-forms)))
         (call-encoding-system-response ,stream ,system-name
                                        :type ,type
                                        :title-format ,title-format
                                        :head #'.head :body #'.body :other #'.other)))))

(defun call-encoding-system-response (stream system-name &key (type :html) (title-format "~a")
                                             head body other
                                             (stylesheet *system-stylesheet*)
                                             (pretty t))
  (http:with-successful-response (stream type)
    (let ((*print-case* :downcase) (*print-pretty* pretty))
      (xmlp:with-xml-writer (stream)
          (encode-xml-declaration)
          (encode-newline)
          (encode-document-type "xhtml" :public xqdm:+xhtml-public-identifier+
                                :system xqdm:+xhtml-system-identifier+)
          (xml ({xhtml}html ({xmlns}||  "http://www.w3.org/1999/xhtml")
                            ({xmlns}xlink  "http://www.w3.org/1999/xlink"))
               ({xhtml}head
                (({}meta ({}name "date") ({}content  (iso-time))))
                (({}link ({}rel "stylesheet") ({}type "test/css")
                         ({}href stylesheet)))
                (({}meta ({}http-equiv "Content-Type")
                         ({xhtml}content "text/xhtml;charset=utf-8")))
                ({}title (encode-format title-format system-name))

                (({}style ({}type "text/css"))
                 "body{ font:  'arial'; margin: 1px;}
                 .demoHeaders { margin-top: 2em; }
                 #dialog_link {padding: .4em 1em .4em 20px;text-decoration: none;position: relative;}
                 #dialog_link span.ui-icon {margin: 0 5px 0 0;position: absolute;left: .2em;top: 50%;margin-top: -8px;}
                 ul#icons {margin: 0; padding: 0;}
                 ul#icons li {margin: 2px; position: relative; padding: 4px 0; cursor: pointer; float: left;  list-style: none;}
                 ul#icons span.ui-icon {float: left; margin: 0 4px;}

                 .displays {  zindex: 3;
                   background-color: #f0f0f0;
                   position: fixed; left: 222px; top: 10px; bottom: 10px; right: 10px;}
                 
                 .controls *[href]:hover { color: gray; border: solid black 1px; }
                 .buttons { margin: 4px; }
                 .description { width: 192;
                   position: fixed; top: 10px; left: 20px;
                 }
                 .configuration { width: 192;
                   position: fixed; bottom: 10px; left: 20px;
                 }
                 .controls {
                   color: black; background-color: transparent;  border: outset gray; text-align: center;
                 }
                 .configuration .label {clear: left; float: left; margin-left: 2px; height: 3ex;}
                 .configuration .inputText {vertical-align: bottom;
                   margin-bottom: 4px; width: 190px; height: 120px;
                   text-align: justify;
                 }
                 .configuration select {clear: right; float: right; margin-right: 2px; }
                 #displayTabContainer {
                   position: absolute; left: 0px; right: 0px; top: 10px; bottom: 0px;
                 }
                 ")
                ;; interpolate the head elements
                (when head (funcall head)))
               ;; encode the entire body
               (when body (funcall body))
               ;; encode any other elements
               (when other (funcall other)))))))
|#


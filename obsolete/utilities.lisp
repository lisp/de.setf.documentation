;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-


(in-package :de.setf.utility.implementation)

(:documentation  "This file implements utilities for the introspection service."

  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved"
  "'de.setf.utility' is free software: you can redistribute it and/or modify
 it under the terms of version 3 of the GNU Lesser General Public License as published by
 the Free Software Foundation.

 'de.setf.utility' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 See the GNU Lesser General Public License for more details.

 A copy of the GNU Lesser General Public License should be included with 'de.setf.utility, as `lgpl.txt`.
 If not, see the GNU [site](http://www.gnu.org/licenses/).")



(defun intern-request-mime-type (type)
  (find type '(:dot :sh :txt :json :html :svg :jpg :pdf) :test #'string-equal))

(defmethod request-mime-types ((server http::server) (request t))
  (http::with-header-values (accept) (http::server-headers server)
    (when accept
      (loop for media-type in (http::sort-accept-header-media-types accept)
            for mime-type = (ignore-errors (mime:mime-type media-type))
            when mime-type
            collect mime-type))))

(defun intern-request-projection (projection)
  (find projection '(:dot :rdf :subject :predicate :object) :test #'string-equal))

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
  (format nil "~(/~a/~{~a/~}~a.~a~)" host path name type))



(defun export-pathname (pathname &optional (url (pathname-url-namestring pathname)))
  "Returns the exported url and the local-context-relative namestring for use in documents.
 The parent directory must already have been exported."
  (let ((type (http:export-type-for-pathname-type (pathname-type pathname))))
    (values (http:export-url (http::merge-url url (http::local-context)) type
                             :pathname pathname)
            url)))



;;





(defgeneric redirect-request (request server target)

  (:method ((request t) (server http::server) (target url:url))
    (http:redirect-request server target))

  (:method ((request t) (server http::server) (target pathname))
    (http:redirect-request server (export-pathname target))))



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

;;;
;;; encoding templates

(defun encode-system-operation-results (stream system operation &key aspects return-code)
  (flet ((pathname-url-namestring (pathname)
           (format nil "~(/~a/~{~a/~}~a.~a~)" (pathname-host pathname) (rest (pathname-directory pathname))
                   (pathname-name pathname) (pathname-type pathname))))
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
                  (({xhtml}div ({}class "header"))
                   (encode-format ";;; ~/date:format-iso-time/ : ~a~@[ return code~]: ~s~%~%----~%"
                                  (get-universal-time) operation return-code))
                  ({xhtml}h3 (({xhtml}a ({}id "#"))  "Results"))
                  (({xhtml}div ({}id "Results"))
                   ({xhtml}ul
                    (loop for (title pathname) in aspects
                          for id from 0 by 1
                          do (let ((url-string (nth-value 1 (export-pathname pathname))))
                               (xml {xhtml}li (({xhtml}a ({}href url-string))
                                               (encode-string title))))))))))))))

(defun encode-footer ()
  (xml ({xhtml}div ({}class "footer")
                   ({}style "position: absolute; bottom: 24px; width: 99%; border-top: groove; margin-right: 4px; padding-top: 2px;"))
       (({xhtml}div ({}style "position: fixed; left: 4px; font-size: small;"))
        (({xhtml}img ({}src "http://www.digitool.com/img/mcl-made-1.gif"))))
       (({xhtml}div ({}style "position: fixed; right: 4px; font-size: small;"))
        ({xhtml}div (encode-format "Copyright ~a setf.de" (date:year)))
        ({xhtml}div (encode-format "~a" http::*server-version*)))))


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



#|

(defun /system/*/graph/calls (request stream &key system-name implementation extent
                                    (projection "dot") (encoding "svg") (options '()))
  (declare (ignore request))
  (multiple-value-bind (system return-code graph-script graph-file transcript-file)
                       (graph-system-calls system-name implementation extent
                                           :projection projection
                                           :encoding encoding
                                           :options options)
    (if (zerop return-code)
      ;; encode the graph as an object element -- redirection fails, as the container just depicts the content as text
      (let ((graph-url-string (nth-value 1 (export-pathname graph-file))))
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
                             ({xhtml}content "text/xhtml;charset=iso-8859-1"))))
                   
                   ({xhtml}body
                    (({xhtml}object ({}style "border: inset;")
                                    ({}data graph-url-string) ({}type "image/svg+xml") 
                                    ({}scrolling "no") ({}frameborder "0")
                                    ({}width "99%") ({}height "99%"))
                     (encode-format graph-url-string))))))))
      (encode-system-operation-results stream system "Graph Calls"
                                       :return-code return-code
                                       :aspects
                                       `(("Graph" , graph-file)
                                         ("Script" ,graph-script)
                                         ,@(when transcript-file
                                             `(("Transcript" ,transcript-file))))))))

(defmethod /system/*/graph/packages (request stream &key system-name implementation extent
                                    (projection "dot") (encoding "svg") (options '()))
  (declare (ignore request))
  (multiple-value-bind (system return-code graph-script graph-file transcript-file)
                       (graph-system-packages system-name implementation extent
                                           :projection projection
                                           :encoding encoding
                                           :options options)
    (if (zerop return-code)
      ;; encode the graph as an object element -- redirection fails, as the container just depicts the content as text
      (let ((graph-url-string (nth-value 1 (export-pathname graph-file))))
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
                             ({xhtml}content "text/xhtml;charset=iso-8859-1"))))
                   
                   ({xhtml}body
                    (({xhtml}object ({}style "border: inset;")
                                    ({}data graph-url-string) ({}type "image/svg+xml") 
                                    ({}scrolling "no") ({}frameborder "0")
                                    ({}width "99%") ({}height "99%"))
                     (encode-format graph-url-string))))))))
      (encode-system-operation-results stream system "Graph Calls"
                                       :return-code return-code
                                       :aspects
                                       `(("Graph" , graph-file)
                                         ("Script" ,graph-script)
                                         ,@(when transcript-file
                                             `(("Transcript" ,transcript-file))))))))

|#

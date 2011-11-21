;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation.xml; -*-


(in-package :de.setf.utility.implementation.xml)

(:documentation  "This file define the http resources for the server interface to
 documentation module."
 
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved"
  "'de.setf.utility' is free software: you can redistribute it and/or modify
 it under the terms of version 3 of the GNU Lesser General Public License as published by
 the Free Software Foundation.

 'de.setf.utility' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 See the GNU Lesser General Public License for more details.

 A copy of the GNU Lesser General Public License should be included with 'de.setf.utility, as `lgpl.txt`.
 If not, see the GNU [site](http://www.gnu.org/licenses/)."))
;; add to http:www;robots.text;;Allow: /systems;;Allow: /metadata/lib/html/index.html

;;;
;;; exports

;;; (url::clear-url-table )

(defun export-documentation-urls ()
  ;; in order to export an svg file, one must "make the resource type known"
  ;; also, if the copy mode is not binary, the response is truncated.

  ;; javascript, css, and image directories
  (http:export-url #u"/js/jquery.js" :java-script-file
                   :pathname #p"METADATA:lib;js;jquery.js")
  (http:export-url #u"/js/jquery-ui.js" :java-script-file
                   :pathname #p"METADATA:lib;js;jquery-ui.js")

  (http:export-url #u"/css/jquery.css" :css-file
                   :pathname #p"METADATA:lib;css;black-tie;jquery.css")
  (http:export-url #u"/css/Calendar.css" :css-file
                   :pathname #p"METADATA:lib;css;Calendar.css")


  (http:export-url #u"/images/" :image-directory
                   :pathname #p"METADATA:images;"
                   :recursive-p t)
  (http:export-url #u"/images/ico/" :image-directory
                   :pathname #p"METADATA:images;ico;"
                   :recursive-p t)
  (http:export-url #u"/css/images/" :image-directory
                   :pathname #p"METADATA:lib;css;black-tie;images;")
  (http:export-url #u"/images/system-twopi.svg" :svg-image
                   :pathname #p"METADATA:images;system-twopi.svg")
  (http:export-url #u"/images/systems.svg" :svg-image
                   :pathname #p"METADATA:images;systems.svg")

  (http:export-url #u"/metadata/" :directory-hierarchy
                   :pathname #p"METADATA:"
                   :recursive-p t)
  (http:export-url #u"/html/" :html-directory
                   :pathname #p"METADATA:lib;html;"
                   :recursive-p t)
  (http:export-url #u"/text/" :text-directory
                   :pathname #p"METADATA:lib;html;"
                   :recursive-p t)
  (http:export-url #u"/text/build-project.sh" :text-file
                   :pathname #p"METADATA:lib;html;build-project.sh")
  (http:export-url #u"/html/index.html" :html-file
                   :pathname #p"METADATA:lib;html;index.html")
  (http:export-url #u"/lisp/source/" :lisp-directory
                   :pathname #p"UPLOAD:"
                   :recursive-p t)

  (http:export-url #u"/lisp/source/net/common-lisp/asdf/update" :computed
                   :response-function '/lisp/source/net/common-lisp/asdf/update
                   :authentication-realm :repositories
                   :capabilities '((:get :maintainers)))

  (http:export-url #u"/systems" :computed
                   :response-function '/systems
                   :public t
                   :language :en
                   :documentation "Respond to the root index page with a eco-system diagram.")
  (http:export-url #u"/systems.txt" :computed
                   :response-function '/systems.*
                   :public t
                   :language :en
                   :documentation "Respond to the root with a system list.")
  (http:export-url #u"/systems.csv" :computed
                   :response-function '/systems.*
                   :public t
                   :language :en
                   :documentation "Respond to the root with a system list.")

  (loop for  impl in de.setf.utility.implementation::*build-implementations*
        do (let ((icon-file (make-pathname :host "METADATA" :directory '(:absolute "images" "ico")
                                           :name impl :type"gif")))
             (export-pathname icon-file)))

  (http::unintern-url #u"/system/")
  (setq http::*url-areas* t)
  (setq ppcre:*allow-named-registers* t)
  ;; this uses :get methods as the proper :post method would require to implement a handler to support 
  ;; areas for things which might look like search urls
  (http:export-url-area ("/system/"
                         :computed
                         :content-type '(:text/html :application/xml :application/json)
                         :response-function 'http::respond-to-pattern-area-url
                         :additional-headers '(:allow (:get :put :delete :post))
                         :documentation
                         "The root URL for rest patterns for documented objects.")
    ("(?<system-name>[^/?]+)/index.html$"
     /system/*/index.html :get)
    ("(?<system-name>[^/]+)/index.div$"
     /system/*/index.div :get)
    ("(?<system-name>[^/]+)/new(&((url=(?<url>[^&]+))|(type=(?<type>[^&]+)))?)*$"
     /system/*/new :get
     :authentication-realm :repositories
     :capabilities '((:get :submitters)))
    ("(?<system-name>[^/]+)/update(&((url=(?<url>[^&]+))|(type=(?<type>[^&]+)))?)*$"
     /system/*/update :get
     :authentication-realm :repositories
     :capabilities '((:get :submitters)))
    ;; the distinction between new,update and others is twofolw
    ;; - the former operate on the project source relative to its repository
    ;; - the other are asdf:operation subtypes and operate on the project's system definition
    ;; - projects operate on the built system
    ("(?<system-name>[^/]+)/operation/(?<operation>[^/]+)/(?<lisp-implementation>[^\\.]+)\\.image$"
     /system/*/operation/ :get)
    ("(?<system-name>[^/]+)/projection(/(?<lisp-implementation>[^\\.]+))?/(?<relation>[^-\\.]+)(-(?<projection>[^\\.]+))?\\.(?<encoding>\\w+)(&(extent=(?<extent>\\S+))?)?$"
     /system/*/projection/ :get)

    ("index.html"                        /systems :get))

  ;; redirect top-level requests to the systems resource.
  (http:export-url #u"/"
                   :computed
                   :response-function #'(lambda (url stream)
                                          (declare (ignore url stream))
                                          (http:redirect-request http::*server* #u"/systems"))
	           :keywords '(:lisp :introspection))

  (http::unintern-url #u"/reports/")
  (http:export-url-area ("/reports/"
                         :computed
                         :content-type '(:text/html :text/plain)
                         :response-function 'http::respond-to-pattern-area-url
                         :additional-headers '(:allow (:get :put :delete :post))
                         :documentation
                         "The root URL for rest patterns for documented objects.")
    ("(?<year>[^/]+)/(?<month>[^/]+)/(?<day>\\w+)/?$"
     /reports/*/*/*))

  )

;; (de.setf.utility.implementation.xml::export-documentation-urls)

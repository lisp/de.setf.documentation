;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation.xml; -*-


(in-package :de.setf.utility.implementation.xml)

(:documentation
  "This file defines global parameters for the introspection service."
 
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

(defparameter */systems-title* "Introactive Systems")
(defparameter *css-scripts* '("http://localhost:80/css/jquery.css"))

(defparameter *javascript-resources*
  '(("http://ajax.googleapis.com/ajax/libs/dojo/1.4/dojo/dojo.xd.js"
     ({}type "text/javascript")
     ({}djConfig "parseOnLoad: true"))))

(defparameter *stylesheet-resources*
  '("http://ajax.googleapis.com/ajax/libs/dojo/1.3/dijit/themes/soria/soria.css"))
(defparameter *body-style-class* "soria")

(defparameter *css-resources*
  '("http://ajax.googleapis.com/ajax/libs/dojo/1.3/dijit/themes/soria/soria.css"))

(defparameter *overview-graph* "/images/opensource-projects-2pi.svg")


#+(or)                                  ; the jQuery requirements
(defparameter *javascript-scripts* '("http://localhost:80/js/jquery.js"
                                     "http://localhost:80/js/jquery-ui.js"))

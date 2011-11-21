;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-


(in-package :de.setf.utility.implementation)

(:documentation "This file defines parameters for the metadata-based documentation module of
 the 'de.setf.utility' library."
 
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

;;
;;
;;

(defparameter *build-implementations* '("abcl" "acl" "ccl" "clisp" "cmucl" "ecl" "lw" "sbcl"))

(defparameter *common-lisp-package* (find-package :common-lisp))

(defparameter *default.build-implementation* "ccl")

(defparameter *default.graph-implementation* "ccl")

(defparameter *documentation-categories*
  '(module class macro file function method-combination method package system type variable)
  "A list of documentation types to govern the extracted documentation strings and meta-data.
 The initial value specifies a simplified setf of all standard documentation types, in that, structure is
 subsumed by class and setf is subsumed by function")

(defparameter *documentation-components* (make-hash-table :test 'equal)
  "This is an hash table which registers components by physical pathname. It comprises the extent of the
 documentation metatdata in terms of the definition source files.")

(defparameter *documentation-conjunctions*
  '("and" "or" "not" "with")
  "The conjunctions indicate, that the successor word is still the same category as the predecessor.")

(defparameter *documentation-form* mime:text/xhtml
  "The default form for generating documentation. Rebound by document-as to the specified value.")

(defparameter *documentation-package* (find-package :de.setf.utility.documentation))

(defparameter *documentation-parameters*
  '()
  "In the dynamic context of annotating the documentation from a function, this is bound to a 
list of the parameters names. When one appears in the documentation string, it is marked as code.")

(defparameter *documentation-punctuation*
  ".,()[]{}:;'\""
  "The punctuation charactes are split of from adjacent words when annotating a documentation string.")

(defparameter *documentation-root* #p"LIBRARY:documentation;"
  "This is the default root for generated documentation. Each file is plaecd below this in a location
 which mirrors its original source location.")

(defparameter *documentation-terms*
  '((package . package) (packages . package) (class . class) (classes . class) (structure . class)
    (instance . class) (instances . class) (specialized . class) (specializes . class)
    (return . class) (returns . class) (delegate . function) (macro . function) (macros . function)
    (operator . function) (operators . function)
    (function . function) (functions . function) (method . function) (binding . variable) (bindings . variable)
    (methods . function) (accessor . function) (accessors . function) (bind . function) (binds . function)
    (type . type) (types . type) (argument . class) (arguments . class) (structure . class)
    (extend . class) (extends . class))
  "Collects the terms which should evoke cross-references in documentation text. The terms
 are verbs and adjectives which trigger cross-references for terms adjacent in documentation
 text. Each term is associated with a concrete documentation class, which is used to check to check whether
 documentation actually exists for the adjacent term and, if so, is included in the cross reference
 to indicate the class")

(defparameter *documentation-visibility*
  '(:external)
  "Specifies the visibility constraints for a symbol to be included in the extraction process. It is a list
 of two possible constituents:

  - :external causes definitions for external symbols to be included
  - :internal causes definitions for internal symbols to be included.")

(defparameter *documentation-stream* nil)

(defvar *definition-cycle* nil)

(defparameter *excluded-packages* (list-all-packages)
  "The collection of packages excluded from documentation analysis.")

(defparameter *graph-implementations* '("abcl" "ccl" "sbcl"))

(defparameter *permit-new-projects* nil
  "unless true, suppress the new operation.")

(defparameter *repository-types*
  '(:git :svn :darcs))

(defparameter *test-implementations* '("abcl" "acl" "ccl" "clisp" "cmucl" "ecl" "lw" "sbcl"))

(defparameter *documentation-base-url* nil
  "Indicates the base uri while a document is generated.
 Serves to compute relative urls.")
;;; -*- Mode: LISP; Syntax: Common-lisp; Package: common-lisp-user; Base: 10; Lowercase: Yes -*-

;;;  This file is the package definition for the documentation module of
;;;  the 'de.setf.utility' Common Lisp library.
;;;
;;;  Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved
;;;  'de.setf.utility' is free software: you can redistribute it and/or modify
;;;  it under the terms of version 3 of the GNU Lesser General Public License as published by
;;;  the Free Software Foundation.
;;;
;;;  'de.setf.utility' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
;;;  without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;;  See the GNU Lesser General Public License for more details.
;;;
;;;  A copy of the GNU Lesser General Public License should be included with 'de.setf.utility, as `lgpl.txt`.
;;;  If not, see the GNU [site](http://www.gnu.org/licenses/).

(in-package :common-lisp-user)

(unless (find-package "xhtml")
  (defpackage "xhtml"
    (:use )
    (:nicknames "http://www.w3.org/1999/xhtml")))

(unless (find-package "")               ; careful if the keyword package has this as a nickname
  (defpackage ""
    (:use )))

(unless (find-package "xmlns")
  (defpackage "xmlns"
    (:use )))

(unless (find-package "asdf")
  (defpackage "asdf"
    (:nicknames "http://www.common-lisp.net/project.asdf")
    (:use )))

(modpackage :de.setf.utility.documentation
  (:nicknames :setf.documentation)
  (:documentation
   "The de.setf.documentation package exports the interface operator names for system documentation tools.
 The primary components are the documentation class and the operators update-documentation-model and
 encode-documentation-model. The model extends asdf file components with details - the specialized
 documentation instances, to represent each definition and its dependencies. The extended system model then
 serves as the basis to generate system documentation, module and call diagrams, and to compute test plans
 based on modifications to source code.")

  (:use-by :de.setf.utility.implementation)
  (:import-from :asdf :module :system :file)
  (:export
   :*class.built-in-class-definition* 
   :*class.class-definition* 
   :*class.compiler-macro-definition* 
   :*class.condition-definition* 
   :*class.constant-definition* 
   :*class.defclass-definition* 
   :*class.defconstant-definition* 
   :*class.defgeneric-definition* 
   :*class.define-compiler-macro-definition* 
   :*class.define-condition-definition* 
   :*class.define-method-combination-definition* 
   :*class.define-modify-macro-definition* 
   :*class.define-setf-expander-definition* 
   :*class.define-symbol-macro-definition* 
   :*class.defmacro-definition* 
   :*class.defmethod-definition* 
   :*class.defpackage-definition* 
   :*class.defparameter-definition* 
   :*class.defsetf-definition* 
   :*class.defstruct-definition* 
   :*class.defun-definition* 
   :*class.defvar-definition* 
   :*class.file-definition* 
   :*class.function-definition* 
   :*class.generic-function-definition* 
   :*class.in-package-definition* 
   :*class.macro-definition* 
   :*class.method-combination-definition* 
   :*class.method-definition* 
   :*class.modify-macro-definition* 
   :*class.package-definition* 
   :*class.parameter-definition* 
   :*class.setf-definition* 
   :*class.setf-expander-definition* 
   :*class.structure-definition* 
   :*class.symbol-macro-definition* 
   :*class.type-definition* 
   :*class.variable-definition* 
   :*documentation-categories*
   :*documentation-conjunctions*
   :*documentation-root*
   :*documentation-visibility*
   :*graphviz-dot*
   :*graphviz-twopi*
   :*prototype-systems*
   :*rdf-projection*
   :*rdf-subject*
   :*rdf-predicate*
   :*rdf-object*
   :abstract-class-definition 
   :abstract-function-definition 
   :abstract-macro-definition 
   :abstract-variable-definition 
   :bind-definition 
   :built-in-class-definition 
   :built-in-class-definition-p 
   :class-definition 
   :class-definition-p 
   :compiler-macro-definition 
   :compiler-macro-definition-p 
   :condition-definition 
   :condition-definition-p 
   :constant-definition 
   :constant-definition-p 
   :definition-component
   :definition-annotated-documentation
   :definition-data-directory
   :definition-component-p
   :definition-category 
   :definition-class-of 
   :definition-definition 
   :definition-documentation 
   :definition-end 
   :definition-form 
   :definition-form-documentation 
   :definition-form-name 
   :definition-lambda-list
   :definition-metadata-directory
   :definition-method-combination 
   :definition-name 
   :definition-object 
   :definition-parameters 
   :definition-pathname 
   :definition-qualifiers 
   :definition-setf-p 
   :definition-signature 
   :definition-source-directory
   :definition-specializers 
   :definition-start 
   :definition-string 
   :definition-uri
   :defpackage-definition 
   :defsetf-definition 
   :document-as
   :document
   :definition-documentation-uri
   :definition-documentation-relative-url
   :encode-documentation
   :encode-documentation-as
   :find-system-prototype
   :function-definition 
   :function-definition-p 
   :generic-function-definition 
   :generic-function-definition-p 
   :get-definition-documentation 
   :global-definition 
   :graph-system
   :graph-system-calls
   :graph-system-modules
   :graph-system-packages
   :graphviz-dot
   :graphviz-twopi
   :instance-definition 
   :instance-definition-as 
   :list-prototype-systems
   :macro-definition 
   :macro-definition-p 
   :make-built-in-class-definition 
   :make-class-definition 
   :make-compiler-macro-definition 
   :make-condition-definition 
   :make-constant-definition 
   :make-definition 
   :make-definition-as 
   :make-function-definition 
   :make-generic-function-definition 
   :make-macro-definition 
   :make-method-combination-definition 
   :make-method-definition 
   :make-modify-macro-definition 
   :make-package-definition 
   :make-parameter-definition 
   :make-setf-definition 
   :make-setf-expander-definition 
   :make-structure-definition 
   :make-symbol-macro-definition 
   :make-type-definition 
   :make-variable-definition 
   :method-combination-definition 
   :method-combination-definition-p 
   :method-definition 
   :method-definition-p 
   :modify-macro-definition 
   :modify-macro-definition-p 
   :named-definition 
   :operator-definition 
   :package-definition 
   :package-definition-p 
   :parameter-definition 
   :parameter-definition-p 
   :perform-system-operation
   :perform-system-projection
   :rdf-projection
   :rdf-subject-projection
   :rdf-predicate-projection
   :rdf-object-projection
   :setf-definition 
   :setf-definition-p 
   :setf-expander-definition 
   :setf-expander-definition-p 
   :setf-property-value
   :structure-definition 
   :structure-definition-p 
   :symbol-macro-definition 
   :symbol-macro-definition-p 
   :system-repository-url
   :system-repository-type
   :type-definition 
   :type-definition-p 
   :variable-definition 
   :variable-definition-p 
   :walk-files
   :walk-modules
   :walk-systems
   ))

(defpackage :de.setf.utility.implementation.xml
  (:use :common-lisp
        :de.setf.utility
        :de.setf.utility.documentation
        :de.setf.xml))

(defpackage :asdf-prototype
  (:use )
  (:documentation "A constrained package to use for loading asdf system prototypes.")
  (:export :intern)
  (:import-from :asdf
                :system
                :file
                :module
                :cl-source-file
                :defsystem
                :compile-op
                :load-op
                :test-op)
  (:import-from :common-lisp
                :*load-truename*
                :symbol-name
                :with-open-file
                :merge-pathnames
                :make-pathname
                :read
                :describe
                :string
                :nil
                :t))


;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-


(in-package :de.setf.utility.implementation)

(:documentation  "This file defines a facility to manage prototype system definitions
 as shadows for the 'real' ones in order to test them."
 
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

;; construction rather than namestring in order to ensure the absolute directory
(defparameter *library-root* (make-pathname :host "LIBRARY" :directory '(:absolute)))
(defparameter *prototype-source-root* (make-pathname :host "UPLOAD" :directory '(:absolute)))
(defparameter *prototype-source-host* "UPLOAD")
(defparameter *prototype-metadata-root* (make-pathname :host "METADATA" :directory '(:absolute)))

(defparameter *prototype-readtable* (copy-readtable nil))
(flet ((skip-form (stream sub infix-arg)
         (declare (ignore sub infix-arg))
         (let* ((*read-suppress* t)
                (form (read stream)))
           (when *load-print*
             (format *standard-output* "~&; ~s " form))
           ;; return nil as the reader macro appears in keyword argument lists
           nil)))
  (set-dispatch-macro-character #\# #\. #'skip-form *prototype-readtable*)
  (set-dispatch-macro-character #\# #\, #'skip-form *prototype-readtable*))

(defvar *prototype-systems*
  (make-hash-table :test #'equalp)
  "A registry for prototypical system definitions loaded with the limited read-eval
 mechanism from the uploaded sources. These serve as a cache for the local source location,
 the repository url, and the repository type. An entry is created for each added system.
 They are refreshed from the source tree when the system starts.

 It repeats the mysterious and inexplicable practice to store  timestamp with the system, rather
 that in the system, 'cuase the hastable is used to shadoe the real asdf one.")

(defgeneric find-system-prototype (name)
  (:method ((system asdf:system))
    system)
  (:method ((name t))
    (rest (gethash (string-downcase name) *prototype-systems*))))

(defun (setf find-system-prototype) (system name)
  (setf (gethash (string-downcase name) *prototype-systems*)
        (cons (get-universal-time) system)))

#+(or)
(progn
  (maphash #'(lambda (k v) (unless (consp v) (remhash k *prototype-systems*))) *prototype-systems*)
  (maphash #'(lambda (k v) (print v)) *prototype-systems*)
  (maphash #'(lambda (k v) (when (string-equal "asdf-registry" (first (last (pathname-directory (definition-source-directory (rest v))))))
                             (remhash k *prototype-systems*)))
           *prototype-systems*)
  )


(defun asdf-prototype::funcall (&rest args)
  "Serves as a place-jolder in order to define systems, but should never be called."
  (error "attempted funcall: ~s." args))

(defun asdf-prototype::intern (string &optional package)
  (declare (ignore package))
  (intern string :asdf-prototype))

(defclass asdf-prototype::component (asdf:component) ())

(defclass asdf-prototype::system-with-readtable (asdf:system asdf-prototype::component) ())

(defclass asdf-prototype::abl-foo-source-file (asdf:cl-source-file asdf-prototype::component) ())
(defclass asdf-prototype::atdoc-source-file (asdf:cl-source-file asdf-prototype::component) ())
(defclass asdf-prototype::bdb-c-source (asdf:cl-source-file asdf-prototype::component) ())
(defclass asdf-prototype::c-test-lib (asdf:cl-source-file asdf-prototype::component) ())
(defclass asdf-prototype::cl-source-file-with-readtable (asdf:cl-source-file asdf-prototype::component) ())
(defclass asdf-prototype::cl-file (asdf:cl-source-file asdf-prototype::component) ())
(defclass asdf-prototype::closure-source-file (asdf:cl-source-file asdf-prototype::component) ())
(defclass asdf-prototype::clsr-gcc-xml-c-source-file (asdf:cl-source-file asdf-prototype::component) ())
(defclass asdf-prototype::cluster-ffi-gcc-xml-c-source-file (asdf:cl-source-file asdf-prototype::component) ())
(defclass asdf-prototype::clx-source-file (asdf:cl-source-file asdf-prototype::component) ())
(defclass asdf-prototype::cpp->so (asdf:cl-source-file asdf-prototype::component) ())
(defclass asdf-prototype::css-FILE (asdf:static-file asdf-prototype::component) ())
(defclass asdf-prototype::docudown-source (asdf:cl-source-file asdf-prototype::component) ())
(defclass asdf-prototype::elephant-c-source (asdf:cl-source-file asdf-prototype::component) ())
(defclass asdf-prototype::example-source-file (asdf:cl-source-file asdf-prototype::component) ())
(defclass asdf-prototype::filtered-object (asdf:cl-source-file asdf-prototype::component) ())
(defclass asdf-prototype::gcc-xml-c-source-file (asdf:cl-source-file asdf-prototype::component) ())
(defclass asdf-prototype::gcc-xml-xml-file (asdf:static-file asdf-prototype::component) ())
(defclass asdf-prototype::grovel-file (asdf:cl-source-file asdf-prototype::component) ())
(defclass asdf-prototype::ironclad-source-file (asdf:cl-source-file asdf-prototype::component) ())
(defclass asdf-prototype::jpeg-file (asdf:static-file asdf-prototype::component) ())
(defclass asdf-prototype::local-cl-source-file (asdf:cl-source-file asdf-prototype::component) ())
(defclass asdf-prototype::library (asdf:static-file asdf-prototype::component) ())
(defclass asdf-prototype::load-file-with-tests (asdf:cl-source-file asdf-prototype::component) ())
(defclass asdf-prototype::load-only-cl-source-file (asdf:cl-source-file asdf-prototype::component) ())
(defclass asdf-prototype::legacy-file (asdf:cl-source-file asdf-prototype::component) ())
(defclass asdf-prototype::makefile (asdf:cl-source-file asdf-prototype::component) ())
(defclass asdf-prototype::my-filtered-object (asdf:cl-source-file asdf-prototype::component) ())
(defclass asdf-prototype::MY-OBJECT-CL-PDF-FILE (asdf:static-file asdf-prototype::component) ())
(defclass asdf-prototype::named-readtables-source-file (asdf:cl-source-file asdf-prototype::component) ())
(defclass asdf-prototype::non-required-file (asdf:cl-source-file asdf-prototype::component) ())
(defclass asdf-prototype::OBJECT-CL-PDF-FILE (asdf:static-file asdf-prototype::component) ())
(defclass asdf-prototype::OBJECT-XHTML-FILE (asdf:static-file asdf-prototype::component) ())
(defclass asdf-prototype::object-from-file (asdf:cl-source-file asdf-prototype::component) ())
(defclass asdf-prototype::png-file (asdf:static-file asdf-prototype::component) ())
(defclass asdf-prototype::QUICKTIME-GCC-XML-C-SOURCE-FILE (asdf:cl-source-file asdf-prototype::component) ())
(defclass asdf-prototype::shared-object (asdf:static-file asdf-prototype::component) ())
(defclass asdf-prototype::silent-source-FILE (asdf:source-file asdf-prototype::component) ())
(defclass asdf-prototype::SWANK-LOADER-FILE (asdf:static-file asdf-prototype::component) ())
(defclass asdf-prototype::r-source-file (asdf:static-file asdf-prototype::component) ())
(defclass asdf-prototype::tiff-file (asdf:static-file asdf-prototype::component) ())
(defclass asdf-prototype::tiff-ffi-gcc-xml-c-source-file (asdf:cl-source-file asdf-prototype::component) ())
(defclass asdf-prototype::TINAA-DIRECTORY (asdf:static-file asdf-prototype::component) ())
(defclass asdf-prototype::test-vector-file (asdf:static-file asdf-prototype::component) ())
(defclass asdf-prototype::txt-file (asdf:static-file asdf-prototype::component) ())
(defclass asdf-prototype::unix-dso (asdf:cl-source-file asdf-prototype::component) ())
(defclass asdf-prototype::wrapper-file (asdf:cl-source-file asdf-prototype::component) ())
(defclass asdf-prototype::xrender-source-file (asdf:cl-source-file asdf-prototype::component) ())

(defmethod shared-initialize :after ((instance asdf-prototype::component) (slots t) &key &allow-other-keys)
  )


(defun prototype-system-definition-pathnames (&optional (root *prototype-source-root*) (name :wild))
  (let ((system-pathnames
         (sort
          (remove-if #'(lambda (pathname)
                         (setf pathname (namestring pathname))
                         (or (search "_darcs" pathname)
                             (search ".git" pathname)
                             (search ".svn" pathname)
                             (search "asdf-registry" pathname)))
                     (directory (merge-pathnames (make-pathname :directory '(:relative :wild-inferiors)
                                                                :type "asd" :name name)
                                                 (truename root))))
          #'string-lessp :key #'(lambda (p) (first (last (pathname-directory p)))))))
    system-pathnames))


(defun load-system-prototype (pathname)
  (when *load-verbose*
    (format *trace-output* "~&;Loading prototypes from ~s..." pathname))
  (with-open-file (input pathname :direction :input)
    (with-standard-io-syntax
      (let ((*package* (or (find-package :asdf-prototype) (error "No prototype package.")))
            #+mcl (*features* (list* :openmcl :clozure *features*))       ; masquerade for prototypes
            ;; must shadow the global registry in order that the real systems remain intact
            (asdf::*defined-systems* *prototype-systems*)
            (*readtable* *prototype-readtable*)
            (*print-readably* nil)
            (form nil)
            (definition nil)
            (*load-truename* (truename pathname))
            (system nil)
            (error nil)
            (repository-url-properties nil))
        (loop (setf form (handler-case (read input nil input)
                           ; squelch read errors, warnings
                           (condition (c)
                                       (when *load-verbose* (warn "; prototype system file @~d signaled a condition: ~a"
                                                                  (file-position input) c))
                                       nil)))
              (when (eq form input) (return))
              (when *load-print* (print form *standard-output*))
              (when (consp form)
                (when (eq (first form) 'asdf:defsystem)
                  (setf definition form)
                  (multiple-value-setq (system error)
                    (ignore-errors (eval definition)))
                  (when error
                    (warn "Loading system definition causes an error: ~s,~%~a"
                          pathname error))
                  (when system
                    (when *load-verbose*
                      (format *trace-output* "~&;  Adding prototype ~s." system))
                    (setf (find-system-prototype (asdf:component-name system)) system)
                    (setf (definition-form system) definition)
                    (setf (definition-pathname system) pathname)
                    (when (setf repository-url-properties
                                (let ((url-pathname (or (probe-file (make-pathname :name (asdf:component-name system)
                                                                                   :type "url"
                                                                                   :defaults (definition-metadata-directory system))))))
                                  (if url-pathname
                                    (handler-case (load-repository-url url-pathname)
                                      (error (c) (warn "Load repository information for system failed: ~a~% ~a~% ~a"
                                                       c system url-pathname)))
                                    (warn "No repository information for system: ~a." system))))
                      (destructuring-bind (&key name url type &allow-other-keys) repository-url-properties
                        (when (and (string-equal name (asdf:component-name system)) url type)
                          (setf (system-repository-url system) url
                                (system-repository-type system) type))))))))

        (values system error)))))


(defun load-system-prototypes (&optional (root *prototype-source-root*))
  (map 'list #'load-system-prototype (prototype-system-definition-pathnames root)))

(defun list-prototype-systems (&optional (test-systems-p nil))
  (loop for entry being the hash-value of *prototype-systems*
        for system = (rest entry)
        when (or test-systems-p
                 (not (search "test" (asdf:component-name system) :test #'char-equal)))
        collect system))

;;; (load-system-prototypes #p"UPLOAD:")
;;; (load-system-prototypes #p"UPLOAD:net;common-lisp;asdf;asdf.asd")
;;; (prototype-system-definition-pathnames)
;;; (list-prototype-systems )
;;; (find-system-prototype "bordeaux-threads")
;;; (find-system-prototype "lift")
;;; (prototype-system-definition-pathnames #p"UPLOAD:")
;;; (prototype-system-definition-pathnames #P"LIBRARY:de;setf;")
;;; (prototype-system-definition-pathnames #P"P-LIBRARY:")

#+(or )
(progn  print
  (defparameter *system-repositories*
    '((:net.common-lisp.lift :repository "http://common-lisp.net/project/lift/" :repository-type :darcs)
      (:net.common-lisp.bordeaux-threads
       :repository "http://common-lisp.net/project/bordeaux-threads/darcs/bordeaux-threads/")))
  
  (defparameter *system-repositories-pathname*
    (make-pathname :host "DATA" :directory '(:absolute "data") :name "system-repositories" :type "txt"))
  )


(defun library-directory (system &optional (host "LIBRARY"))
  "Return the prospective system location.
 SYSTEM : (designator string) : the system name
 HOST : (designator logical-host) : by default the production library host
 value : logical-pathname

 Return the source directory location for the system as if it exists as a library.
 This uses neither find-system nor sysdef-hierarchical-search-function, as the source may not yet even have
 been fetched and they matche existing system definition files. It just constructs the prospective
 location given hierarchical names. By default it is located as a production library"

  (make-pathname :host host
                 :directory `(:absolute ,@(cl-user::system-namestring-to-list (string-downcase system)))))


(defun library-upload-directory (system-name)
  "Return the upload directory analogous to the location of an existing system library."
  (let ((library-path (library-directory system-name)))
    (when (probe-file library-path)
      (make-pathname :host *prototype-source-host* :defaults library-path))))


(defgeneric url-upload-directory (host system-name type)
  (:documentation "Given an url, generate a path relative to the upload root, to be used to store the
 source.")
  #+cl-http
  (:method ((url url:url) system-name (type t))
    (host-upload-directory (url:host-string url) system-name))

  (:method ((url string) (system-name string) (type symbol))
    (let* ((authority (search "://" url :test #'char-equal))
           (path-position (when authority (position #\/ url :start (+ authority 3))))
           (host-position (when authority (or (position #\@ url :start (+ authority 3)) (+ authority 2)))))
      (assert (and path-position host-position) ()
              "Invalid repository url: ~s." url)
      (host-upload-directory (subseq url (1+ host-position) path-position) system-name)))

  (:method ((url string) (system-name string) (type (eql :cvs)))
    (let* ((at-position (position #\@ url))
           (colon-position (position #\: url :start at-position)))
      (assert (and at-position colon-position) ()
              "Invalid repository url: ~s." url)
      (host-upload-directory (subseq url (1+ at-position) colon-position) system-name))))

(defun host-upload-directory (host system-name)
  (let* ((host-path (reverse (split-string host #\.)))
         (system-path (split-string (string-downcase system-name) #\.))
         (overlap (member (first system-path) host-path))
         (combined-path (append (ldiff host-path overlap) system-path)))
    (make-pathname :host *prototype-source-host* :directory `(:absolute ,@combined-path))))


(defun save-repository-url (pathname &key system-name repository-url repository-type)
  "Save the current repository location in .url form"
  (ensure-directories-exist pathname)
  (when *load-verbose*
    (format *trace-output* "~&;Save repository URL to ~s (~a ~a ~a)."
            pathname system-name repository-url repository-type))
  (with-open-file (output pathname :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format output "[InternetShortcut]~%URL=~a~%DATE=~/date:format-iso-time/~%NAME=~a~%TYPE=~a~%"
            repository-url (get-universal-time) system-name repository-type)))

(defun load-repository-url (pathname)
  "Load the content of an .url file"
  (with-open-file (input pathname :direction :input)
    (assert (string-equal (read-line input) "[InternetShortcut]") ()
            "Invalid .url file: ~s." pathname)
    (let ((properties ())
          (line ""))
      (loop (setf line (read-line input nil nil))
            (unless line
              (return))
            (let ((=pos (position #\= line)))
              (when =pos
                (let ((key (intern (string-upcase (subseq line 0 =pos)) :keyword))
                      (value (subseq line (1+ =pos))))
                  (cond ((string-equal key "type")
                         (setf properties (list* key (intern (string-upcase value) :keyword) properties)))
                        (t
                         (setf properties (list* key value properties))))))))
      (assert (and (getf properties :name) (getf properties :url) (getf properties :type)) ()
              "Invalid repository.url file: ~s." pathname)
       properties)))


(defgeneric canonical-repository-parameters (system repository-location repository-form &rest args)
  (:documentation "Complete the parameters for other known methods by combining
 the location, or a respective default location, with the system name to produce a standard repository
 resource locator.")

  (:method ((system-name string) (location t) (form string) &rest args)
    (if (string-equal form "get_" :end1 4)
      (apply #'canonical-repository-parameters system-name location (intern (string-upcase (subseq form 4)) :keyword)
             args)
      (error "~%~s ~s ~s . ~s -- invalid repository form."
            system-name form location args)))

  (:method ((system-name string) (location t) (form symbol) &rest args)
    (error "~%~s ~s ~s . ~s -- unsupported repository form."
            system-name form location args))

  (:method ((system-name string) (location null) (type (eql :b9_com)) &rest args)
    (declare (ignore args))
    (values (format nil "git://git.b9.com/~a.git" system-name) :git))

  (:method ((system-name string) (location null) (type (eql :clbuild_mirror)) &rest args)
    (declare (ignore args))
    (values (format nil "http://common-lisp.net/project/clbuild/mirror/~a" system-name) :darcs))

  (:method ((system-name string) (location null) (type (eql :cvs_clnet)) &rest args)
    (apply #'canonical-repository-parameters system-name system-name type args))

  (:method ((system-name string) (branch string) (type (eql :cvs_clnet)) &rest args)
    (declare (ignore args))
    (values  (format nil ":pserver:anonymous:anonymous@common-lisp.net:/project/~a/cvsroot" branch) :cvs))

  (:method ((system-name string) (location string) (type (eql :cvs_full)) &rest args)
    (declare (ignore system-name args))
    (values location :cvs))    

  (:method ((system-name string) (location null) (type (eql :cvs_sfnet)) &rest args)
    (let ((project-name (or (first args) system-name)))
      (values (format nil ":pserver:anonymous:@project.cvs.sourceforge.net:/cvsroot/~a" project-name) :cvs)))

  (:method ((system-name string) (location string) (type (eql :darcs)) &rest args)
    (declare (ignore system-name args))
    (values location type))

  (:method ((system-name string) (location null) (type (eql :ediware)) &rest args)
    (declare (ignore args))
    (values (format nil "http://common-lisp.net/~~loliveira/ediware/~a" system-name) :darcs))

  (:method ((system-name string) (location string) (type (eql :git)) &rest args)
    (declare (ignore system-name args))
    (values location type))

  (:method ((system-name string) (location null) (type (eql :lichteblau_com)) &rest args)
    (declare (ignore args))
    (values (format nil "http://www.lichteblau.com/git/~a.git" system-name) :git))
 
  (:method ((system-name string) (location string) (type (eql :svn)) &rest args)
    (declare (ignore system-name args))
    (values location type))

  (:method ((system-name string) (branch string) (type (eql :svn_clnet)) &rest args)
    (declare (ignore args))
    (values (format nil "svn://common-lisp.net/project/~a/svn/~a" system-name branch) :svn))

  (:method ((system-name string) (location null) (type (eql :xach_com)) &rest args)
    (declare (ignore args))
    (values (format nil "git://git.xach.com/~a.git" system-name) :git))

  (:method ((system asdf:system) location form &rest args)
    (apply #'canonical-repository-parameters (asdf:component-name system) location form
             args)))


(defgeneric retrieve-project (system location form &rest args)
  (:documentation "Given a droject description, update or retrieve anew the content of the located repository.
 SYSTEM : (designator system)
 LOCATION : (designator url) : a resource locator for the repository
 FORM : (or keyword string) : a type indicator.

 The base retrieval methods are :cvs, :darcs, :git, and :svn -- that is, no tar support.
 Use canonicalize-repository-parameters to complete the parameters for other known methods by combining
 the location, or a respective default location, with the system name to produce a standard repository
 resource locator. Given the locator and the type, if the
 system exists, update it. If it does not, add an entry to the source tree and retrieve it anew.
 delegate to perform-system-new and perform-system-update for the operations.")


  (:method ((system t) (location t) (form string) &rest args)
    (multiple-value-bind (location type)
                         (apply #'canonical-repository-parameters system location form args)
      (apply #'retrieve-project system location type args)))

  (:method ((system asdf:system) (location string) (type symbol) &rest args)
    (declare (ignore args))
    (perform-system-update system :repository-url location :repository-type type))

  (:method ((system-name string) (location string) (type symbol) &rest args)
    (let ((system (find-system-prototype system-name)))
      (if system
        (apply #'retrieve-project system location type args)
        (perform-system-new system-name :repository-url location :repository-type type)))))


(defun retrieve-projects (&optional (pathname #p"LIBRARY:net;common-lisp;clbuild;projects"))
    (let ((line "")
          (count 0)
          (gotten 0))
      (with-open-file (stream pathname :direction :input)
        (loop (setf line (read-line stream nil stream))
              (if (eq stream line) (return))
              (when (setf line (split-string (subseq line 0 (position #\# line)) " "))
                (incf count)
                (destructuring-bind (system-name get-form &optional location &rest args) line
                  (format *trace-output* "~&~%~a~32T~s~40T~s~@[ . ~s~]: ensure system ... " system-name get-form location args)
                  (handler-case (multiple-value-bind (system return-code)
                                                     (apply #'retrieve-project system-name location get-form args)
                                  (when (zerop return-code) (incf gotten))
                                  (format *trace-output* " ... ~a~%" system))
                    (error (c) (warn "system operation caused an error: ~a." c))))
                (finish-output *trace-output*))))
      (terpri *trace-output*)
      (values gotten count)))

#+(or)
(progn
  ;; bootstrap
  ;; gets the projects as be cl-build description file.
  ;; note that, clbuild, itself, has no .asd 'cause it's a shell script.
  (asdf:load-system :de.setf.documentation)
  (in-package :de.setf.utility.implementation)
  ;; if there are any
  (load-system-prototypes)
  (retrieve-projects #p"LIBRARY:net;common-lisp;clbuild;projects")
  ;;; 76 / 89
  (retrieve-projects #p"LIBRARY:net;common-lisp;clbuild;wnpp-projects")
  ;;; 91 / 136 systems
  ;;; after the update, rsync from the upload tree to the library, and link to the .asd's (see below)

  ;;; nb. to bootstrap a system w/o an .asd it has to run first as "new" to get the base source.
  ;;; then second, after creating and loading the .asd explicitly, as an update
  (let ((de.setf.utility.implementation::*permit-new-projects* t))
    (de.setf.utility.implementation::retrieve-projects #p"METADATA:lib;projects"))
  (mapc #'de.setf.utility.implementation::load-system-prototype
        '(#P"UPLOAD:com;github;cl-python;cl-python.asd"         ; requires a link for different project name
          #P"UPLOAD:com;github;imap;imap.asd"
          #P"UPLOAD:com;github;ntservice;ntservice.asd"
          #P"UPLOAD:com;github;aserve;aserve.asd"
          #P"UPLOAD:com;github;cl-env;cl-env.asd"
          #P"UPLOAD:com;github;tester;tester.asd"))
  
  ;; various things which system require in order to load their own definitions
  ;;; trivial-features
  ;;; cffi-grovel, requires an explcit registry entry
  ;;; elephant : .asd tree structure must be standardized; system definition extensions factored out of .asd
  ;;; rfc2109 : required load-file-with-tests
  ;;; computed-class : required SYSTEM-WITH-READTABLE
  ;;; cl-serializer : required LOCAL-CL-SOURCE-FILE
  ;;; cl-perec :  required LOCAL-CL-SOURCE-FILE
  ;;; cl-def : required SYSTEM-WITH-READTABLE - from :asdf-system-connections
  ;;; cl-quasi-quote : required SYSTEM-WITH-READTABLE
  ;;; stefil : required LOCAL-CL-SOURCE-FILE
  ;;; cl-store : NON-REQUIRED-FILE
  ;;; quicktime-ffi : ambiguous asd's 
  ;;; cluster-ffi : load-time special variables 
  ;;; clsr : load-time special variables
  ;;;
  ;;; one for each repository variation
  (trace retrieve-projects retrieve-project perform-system-new perform-system-update  canonical-repository-parameters)
  (retrieve-project "md5" nil "get_b9_com")
  (retrieve-project "ironclad" nil "get_clbuild_mirror")
  (retrieve-project "flexichain" nil "get_cvs_clnet")
  (retrieve-project "zip" nil "get_cvs_clnet")
  (retrieve-project "series" ":pserver:anonymous@series.cvs.sourceforge.net:/cvsroot/series" "get_cvs_full")
  (retrieve-project "hunchentoot" nil "get_ediware")
  (retrieve-project "cxml-rng" nil "get_lichteblau_com")
  (retrieve-project "montezuma" "http://montezuma.googlecode.com/svn/trunk/montezuma" "get_svn")
  (retrieve-project "zs3" nil "get_xach_com")
  (retrieve-project "cl-utilities" "cl-utilities" "get_cvs_clnet")
  
  (save-repository-url "DATA:net;common-lisp;bordeaux-threads;repository.url"
                       :system-name :net.common-lisp.bordeaux-threads
                       :repository-url "http://common-lisp.net/project/bordeaux-threads/darcs/bordeaux-threads/"
                       :repository-type :darcs)
  (load-repository-url "DATA:net;common-lisp;bordeaux-threads;repository.url")
  
  
  ;; continuing (see also ec2-builds.txt)
  (asdf:load-system :org.cl-http)       ; on top of the existinf xml code
  
  (http::set-local-context "http://ip-10-251-70-19.ec2.internal:8082")
  (http::initialize-server-authentication)
  (http::load-examples)
  (http::DEFINE-URL-CONTEXT-REMAPPINGS
    (:http "ip-10-251-91-38.ec2.internal" 8082 "ec2-184-73-84-129.compute-1.amazonaws.com" 8082 :http)
    (:http "ip-10-251-91-38.ec2.internal" 8082 "ip-10-251-70-19.ec2.internal" 8082 :http))
  (http:add-virtual-host-nick-name "ec2-184-73-84-129.compute-1.amazonaws.com" 8082 (http:local-context))
  (http:add-virtual-host-nick-name "ip-10-251-70-19.ec2.internal" 8082 (http:local-context))
  (de.setf.utility.implementation.xml::export-documentation-urls)
  
  (http::start :hostname "ip-10-251-91-38.ec2.internal" :port 8082 :type :single)
  ;; external fails since it cannot resolve the ip from inside
  ;; (http::start :hostname "ec2-184-73-84-129.compute-1.amazonaws.com" :port 8082 :type :single)
  
  ;; update library sources and registry
  "sourceRoot=/development/source/library/
   rsync -az /development/source/upload/{be,com,cz,net,org,uk} ${sourceRoot}
     cd ${sourceRoot}
     rm asdf-registry/*
     ln -fs `find . -name '*.asd' -print | fgrep -v asdf-registry | sed s:./:${sourceRoot}:` asdf-registry/"
  
  ;; franzinc adds
  (load-system-prototype "UPLOAD:com;github;imap;imap.asd")
  
  )

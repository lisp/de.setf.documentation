;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-


(in-package :de.setf.utility.implementation)

(:documentation  "This file defines http support for an introspection service."
 
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

(defclass system-projection ()
  ((model-type :reader projection-model-type)
   (encoded-type :reader projection-encoded-type)
   (application :reader projection-application)
   (encoding-arguments :initform nil :reader projection-encoding-arguments)))

(defclass graphviz-projection (system-projection)
  ((model-type :initform "dot" :allocation :class)))
(defclass graphviz-dot (graphviz-projection)
  ((application :initform "dot" :allocation :class)))
(defparameter *graphviz-dot* (make-instance 'graphviz-dot))
(defclass graphviz-twopi (graphviz-projection)
  ((application :initform "twopi" :allocation :class)))
(defparameter *graphviz-twopi* (make-instance 'graphviz-twopi))

(defclass rdf-projection (system-projection)
  ((model-type :initform "n3" :allocation :class)))
(defparameter *rdf-projection* (make-instance 'rdf-projection))
(defclass rdf-subject-projection (rdf-projection)
  ())
(defparameter *rdf-subject* (make-instance 'rdf-subject-projection))
(defclass rdf-predicate-projection (rdf-projection)
  ())
(defparameter *rdf-predicate* (make-instance 'rdf-predicate-projection))
(defclass rdf-object-projection (rdf-projection)
  ())
(defparameter *rdf-object* (make-instance 'rdf-object-projection))


(defun repository-type (repository-url)
  (cond ((search "git" repository-url) :git)
        ((search "svn" repository-url) :svn)
        ((search "darcs" repository-url) :darcs)
        ((search "cvs" repository-url) :cvs)))

(assert (equal (mapcar #'repository-type '("git://github.com/lisp/de.setf.amqp.git"
                                           "http://common-lisp.net/project/cl-graph/darcs/cl-graph"
                                           "http://svn.clozure.com/publicsvn/openmcl/release/1.4/darwinx86/ccl"
                                           ":pserver:anonymous:anonymous@common-lisp.net:/project/zip/cvsroot"))
               '(:git :darcs :svn :cvs)))

(defun intern-repository-type (type)
  (find type *repository-types* :test #'string-equal))

(defun intern-system-operation (operation)
  (let ((symbol (find-symbol (string-upcase operation) :asdf)))
    (when (and symbol (subtypep symbol 'asdf:operation))
      symbol)))

#+mcl
(defun file-unix-namestring (pathname)
  (bsd:file-bsd-namestring pathname))
#-mcl
(defun file-unix-namestring (pathname)
  (setf pathname (translate-logical-pathname pathname))
  (format nil "~:[~;/~]~{~a/~}~@[~a~]~@[.~a~]"
          (eq (first (pathname-directory pathname)) :absolute)
          (rest (pathname-directory pathname))
          (pathname-name pathname)
          (pathname-type pathname)))

(defparameter *shell-path* (list "/opt/local/bin"
                                 (file-unix-namestring (make-pathname :directory '(:absolute "bin") :host "METADATA"))
                                 (file-unix-namestring (make-pathname :directory '(:absolute "bin" "lisp") :host "METADATA"))))

(defparameter *create-script* "#! /bin/bash~%~%
export PATH=~{~a:~}$PATH
sources=~a
systemName=~a
systemDirectory=~a
scmCommand=\"~a\"
registryDirectory=~a

echo \"retrieving system : ${systemName}\"
if (!(test -d ${sources}))
then
  echo \"No source directory found: ${sources}.\"
  exit 1
fi
cd  ${sources}
cd ..
rm -rf ${systemDirectory}
${scmCommand}
cd ${sources}
# _do not_ do this - except for the system itself, the asd's should be from the stable sources
#asdFiles=`find . -name '*.asd' -print | sed s:\./:${sources}/: `
#if ! test -z \"${asdFiles}\"
#then
#  ln -fs ${asdFiles} {registryDirectory}
#fi~%")

(defparameter *update-script* "#! /bin/bash~%~%
export PATH=~{~a:~}$PATH
sources=~a
scmCommand=\"~a\"

if (!(test -d ${sources}))
then
  echo \"No source directory found: ${sources}.\"
  exit 1
fi
cd ${sources}
${scmCommand}
#asdFiles=`find . -name '*.asd' -print | sed s:\./:${sources}/: `
#if ! test -z \"${asdFiles}\"
#then
#  ln -fs ${asdFiles} ${registryDirectory}
#fi~%")

(defparameter *build-script* "#! /bin/bash~%~%
## build a system image
## if an image exists with introspection tools use that to augment the runtime
export PATH=~{~a:~}$PATH
sourceDirectory=\"~a\"
buildDirectory=\"~a\"
asdfRegistry=\"~a\"
buildInit=\"~a\"
systemName=\"~a\"
implementation=\"${1}\"
if [ -z \"${implementation}\" ]; then echo \"runtime implementation required\"; exit 1; fi
lisp=`which ${implementation}`
if [ ! -x \"${lisp}\" ]; then echo \"No lisp found: ${1}.\"; exit 1; fi
ulimit -t 60  # gets one cpu minute to build

cd ${buildDirectory}
${lisp} <<EOF
;;; load the build init file - a distinct form as the asdf package is not yet
(load #p\"${buildInit}\")
(handler-case
  (let ((*compile-verbose* t)
        (*load-verbose* t)
        (system :${systemName})
        (image-path #p\"${buildDirectory}${implementation}.image\"))
    ;; add the system source directory to the registry search path
    (pushnew #p\"${asdfRegistry}\" asdf:*central-registry* :test #'equalp)
    (pushnew #p\"${sourceDirectory}\" asdf:*central-registry* :test #'equalp)
    (format *trace-output* \"~~&;For project ~~s, using ASDF registry ~~s ... \" system asdf:*central-registry*)
    (setf (gethash (asdf::coerce-name system) asdf::*defined-systems*) nil)
    (format *trace-output* \"~~&;System definition purged: ~~s... \" system)
    ;;; special handling for testing asdf itself
    (when (string-equal \"asdf\" system)
      (format *trace-output* \"~~&;Pre-load system ~~s... \" system)
      (load (or (asdf::system-definition-pathname \"asdf\")
                (error \"cannot find an asdf system for pre-load: ~~s.\"
                       asdf:*central-registry*))))
    (format *trace-output* \"~~&;Load system ~~s... \" system)
    (asdf:load-system system)
    (format *trace-output* \"~~&;Load completed. Saving and exit... \")
    ;; save the image in the system-specific directory
    (cl-user::save-image image-path system)
    (format *trace-output* \"~~%;Image saved to ~~s.\" image-path)
    (cl-user::leave-lisp \"Build complete\" 0))
  (error (c)
    (warn \"Build failed with error: ~~a.\" c)
    (cl-user::print-backtrace)
    (cl-user::leave-lisp \"Build failed\"
                         (typecase c
                           (asdf::missing-system 1)
                           (asdf::missing-component 2)
                           (asdf::missing-dependency 3)
                           (asdf::operation-error 4)
                           (t 255)))))
EOF~%")

(defparameter *test-script* "#! /bin/bash~%~%
export PATH=~{~a:~}$PATH

if [ test -z \"${1}\" ]; then echo \"runtime implementation required\"; exit 1; fi
if [ -z \"${2}\" ]; then echo \"test system required\"; exit 1; fi
lisp=`which ${1}`
if [ -z \"${lisp}\" ]; then echo \"No lisp found: ${1}.\"; exit 1; fi
ulimit -t 60  # gets one cpu minute to test

cd ~a
${lisp} <<EOF
(handler-case
  (let ((*compile-verbose* t)
        (*load-verbose* t)
        (test-system :${2}))
    ;; add the _test system_ source directory to the registry search path
    (pushnew ~s asdf:*central-registry* :test #'equalp)
    (format *trace-output* \"~~&;Load test system ~~s... \" test-system)
    (asdf:load-system test-system)
    (format *trace-output* \"~~&;Load of test system completed. Testing...\")
    (asdf:test-system test-system)
    (format *trace-output* \"~~&;Tests complete.\")
    (cl-user::leave-lisp \"test complete\" 0))
  (error (c)
    (warn \"Test failed with error: ~~a.\" c)
    (cl-user::print-backtrace)
    (cl-user::leave-lisp \"Tests failed\" 255)))
EOF~%")

(defparameter *graph-script* "#! /bin/bash~%~%
export PATH=~{~a:~}$PATH
buildDirectory=\"~a\"
graphInit=\"~a\"
relation=\"~a\"
extent=\"~a\"
systemName=\"~a\"
outputFile=\"~a\"
options=\"~a\"
application=\"~@[~a~]\"
encodedFile=\"~@[~a~]\"
encodingType=\"~@[~a~]\"
lisp=`which ${1}`

if [ ! -x \"${lisp}\" ]; then echo \"No lisp found: ${1}.\"; exit 1; fi

cd ${buildDirectory}
echo \"Generate a graph of ${systemName} x ${relation} as ${outputFile} with ${lisp}\"
${lisp} ${1}.image <<EOF
(in-package :de.setf.utility.implementation)
(load #p\"${graphInit}\")                               ; until initialization code is built in
(handler-case
  (progn (print '(encode-graph :${relation} \'${extent} :dot :name :${systemName} :stream #p\"${outputFile}\" :options \'${options}))
         (encode-graph :${relation} \'${extent} :dot :root :${systemName} :stream #p\"${outputFile}\" :options \'${options})
         (format *trace-output* \"~~&;Graph complete.\")
         (cl-user::leave-lisp \"graph generation complete\" 0))
  (error (c)
    (warn \"Graph failed with error: ~~a.\" c)
    (cl-user::leave-lisp \"graph generation failed\" 255)))
EOF
if [ -f \"${outputFile}\" ]
then
  if [ -n \"${application}\" ]
  then
    ${application} -T${encodingType} -o${encodedFile} ${outputFile}
  fi
  echo Graph complete.
else
  echo \"No output produced.\"
  exit 1
fi
")

(defparameter *rdf-script* "#! /bin/bash~%~%
export PATH=~{~a:~}$PATH
buildDirectory=\"~a\"
graphInit=\"~a\"
relation=\"~a\"
extent=\"~a\"
systemName=\"~a\"
outputFile=\"~a\"
options=\"~a\"
lisp=`which ${1}`

if [ ! -x \"${lisp}\" ]; then echo \"No lisp found: ${1}.\"; exit 1; fi

cd ${buildDirectory}
echo \"Generate a graph of ${systemName} x ${relation} as ${outputFile} with ${lisp}\"
${lisp} ${1}.image <<EOF
(in-package :de.setf.utility.implementation)
(load #p\"${graphInit}\")                               ; until initialization code is built in
(handler-case
  (progn (print '(encode-graph :${relation} \'${extent} :rdf :root :${systemName} :stream #p\"${outputFile}\" :options \'${options}))
         (encode-graph :${relation} \'${extent} :rdf :root :${systemName} :stream #p\"${outputFile}\" :options \'${options})
         (format *trace-output* \"~~&;Graph complete.\")
         (cl-user::leave-lisp \"graph generation complete\" 0))
  (error (c)
    (warn \"Graph failed with error: ~~a.\" c)
    (cl-user::leave-lisp \"graph generation failed\" 255)))
EOF
")


(defun create-update-script (pathname source-directory metadata-directory repository-url repository-type system-name)
  (declare (ignore metadata-directory system-name))
  (with-open-file (script-stream pathname :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format script-stream  *update-script*
            *shell-path*
            (file-unix-namestring source-directory)
            (case repository-type
              (:cvs (format nil "cvs -z3 update -P ~a" repository-url))
              (:darcs "darcs pull")
              (:git "git pull")
              (:svn "svn update")))))


(defun create-build-script (pathname source-directory metadata-directory system-name)
  (with-open-file (script-stream pathname :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format script-stream  *build-script*
            *shell-path*
            (file-unix-namestring source-directory)
            (file-unix-namestring metadata-directory)
            (file-unix-namestring #p"LIBRARY:asdf-registry;")
            (file-unix-namestring #p"METADATA:lib;build-init.lisp")
            system-name)))


(defun create-test-script (pathname test-system-source-directory metadata-directory system-name)
  (with-open-file (script-stream pathname :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format script-stream  *test-script*
            *shell-path*
            (file-unix-namestring metadata-directory)
            (file-unix-namestring test-system-source-directory)
            system-name)))


(defun create-rdf-script (pathname source-directory metadata-directory output-filename system-name
                                     relation extent
                                     &key (options '()))
  (declare (ignore source-directory))
  (with-open-file (script-stream pathname :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format script-stream *rdf-script*
            *shell-path*
            (file-unix-namestring metadata-directory)
            (file-unix-namestring #p"METADATA:lib;graph-init.lisp")
            relation
            (remove-duplicates (split-string extent ", ") :test #'equal :from-end t)
            system-name
            (file-namestring output-filename)
            options)))

(defun create-graph-script (pathname source-directory metadata-directory output-filename system-name
                                     relation extent encoded-file
                                     &key (projection *graphviz-dot*) (options '()))
  (declare (ignore source-directory))
  (with-open-file (script-stream pathname :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format script-stream *graph-script*
            *shell-path*
            (file-unix-namestring metadata-directory)
            (file-unix-namestring #p"METADATA:lib;graph-init.lisp")
            relation
            (remove-duplicates (split-string extent ", ") :test #'equal :from-end t)
            system-name
            (file-namestring output-filename)
            options
            (when encoded-file (projection-application projection))
            (when encoded-file (file-namestring encoded-file))
            (when encoded-file (pathname-type encoded-file)))))

(defun run-script (system-name script-filename &rest args)
  "Run a shell script with given arguments. redirect all output to an analogously names '.txt' file.
 Return the script's exist code and the name of the transcript file."
  #-mcl (setf script-filename (file-unix-namestring script-filename))
  (multiple-value-bind (second minute hour day month year) (decode-universal-time (get-universal-time) 0)
    (declare (ignore second minute hour))
    (flet ((~d (x) (format nil "~2,'0d" x)))
      (let* ((impl (find-if #'(lambda (arg) (member arg *build-implementations* :test #'string-equal)) args))
             (timestamp (date:|yyyyMMddTHHmmssZZ| (get-universal-time)))
             (output-filename (make-pathname :host "METADATA"
                                             :directory `(:absolute "reports" ,(~d year) ,(~d month) ,(~d day))
                                             :name (format nil "~a-~a-~@[~a-~]~a"
                                                           system-name (pathname-name script-filename) impl timestamp)
                                             :type "txt")))
        (ensure-directories-exist output-filename)
        (values (asdf:run-shell-command "sh ~a ~{~a ~}> ~a 2>&1" script-filename args
                                        #+mcl output-filename
                                        #-mcl (file-unix-namestring output-filename))
                output-filename)))))



(defun initialize-system-directories (system-name source-pathname &key repository-url repository-type)
  "Given a system designator, test that a source exists and ensure that a corresponding data directory also
 exist, and that the data directory contains the scripts: get-system, update-system, build-system, and
 test-system.
 Return the source and metadata pathnames, and the respository type."

  (let* ((source-directory (translate-logical-pathname (make-pathname :name nil :type nil :defaults source-pathname)))
         (metadata-directory (compute-metadata-directory source-directory))
         (registry-directory  #p"UPLOAD:asdf-registry;"))
    (ensure-directories-exist source-directory)
    (ensure-directories-exist metadata-directory)
    (ensure-directories-exist registry-directory)

    (let ((unix-source-directory (file-unix-namestring source-directory))
          (last-directory-name (first (last (pathname-directory source-directory))))
          (unix-registry-directory (file-unix-namestring registry-directory)))

      ;; refresh / create the scripts
      (with-open-file (script-stream (make-pathname :name "new-system" :type "sh" :defaults metadata-directory)
                                     :direction :output :if-exists :supersede :if-does-not-exist :create)
        (format script-stream  *create-script*
                *shell-path* unix-source-directory system-name last-directory-name
                (ecase repository-type
                  (:cvs (format nil "cvs -z3 -d~a checkout ~a" repository-url last-directory-name))
                  (:darcs (format nil "darcs get ~a ~a" repository-url last-directory-name))
                  (:git (format nil "git clone ~a ~a" repository-url last-directory-name))
                  (:svn (format nil "svn co ~a ~a" repository-url last-directory-name)))
                unix-registry-directory))
      (save-repository-url (make-pathname :name system-name :type "url" :defaults metadata-directory)
                           :system-name system-name
                           :repository-url repository-url
                           :repository-type repository-type)
      (list :metadata-directory metadata-directory
            :source-directory source-directory))))


(defun perform-system-new (system-name &key repository-url repository-type)
  "If the system is known, reuse the existing upload location. Otherwise use a location analogous with a known
 library, or corresponding with the url path."
  (let* ((system (find-system-prototype system-name))
         (upload-directory (cond ((and system (definition-source-directory system)))
                                 ((library-upload-directory system-name))
                                 ((url-upload-directory repository-url system-name repository-type))
                                 (t
                                  (error 'http::bad-syntax-provided
                                         :reason (format nil "Cannot derive a source location given system and url: ~s, ~s."
                                                         system-name repository-url))))))
    (destructuring-bind (&key metadata-directory source-directory)
                        (initialize-system-directories system-name upload-directory
                                                       :repository-url repository-url
                                                       :repository-type repository-type)
      (let ((new-script (make-pathname :name "new-system" :type "sh" :defaults metadata-directory))
            (collected-systems ()))
        (multiple-value-bind (return-code transcript-file)
                             (run-script system-name new-script)
          (when (zerop return-code)
            (let* ((system-asd-pathnames (prototype-system-definition-pathnames source-directory)))
              (if (zerop (length system-asd-pathnames))
                (error  "No system .asd pathnames: ~s." system-name)
                (dolist (asd-pathname system-asd-pathnames)
                  (let ((asd-name (pathname-name asd-pathname))
                        (asd-system (handler-case (load-system-prototype asd-pathname)
                                      (error (c) (warn "Error loading .asd: ~a" c) nil))))
                    (when asd-system
                      (push asd-system collected-systems)
                      (setf asd-name (asdf:component-name asd-system))
                      (when (equalp asd-name system-name)
                        (setf system asd-system))
                      (save-repository-url (make-pathname :name asd-name :type "url" :defaults metadata-directory)
                                           :system-name asd-name
                                           :repository-url repository-url
                                           :repository-type repository-type)
                      (setf (system-repository-url asd-system) repository-url)
                      (setf (system-repository-type asd-system) repository-type)))))))
          (values system return-code new-script transcript-file collected-systems))))))


(defun perform-system-update (system &key
                                     (repository-url (system-repository-url system) ru-s)
                                     (repository-type (if ru-s
                                                        (or (repository-type repository-url) (system-repository-type system))
                                                        (system-repository-type system))))
  "Update the system source from the system's repository. If a new repository url is provided, change
 the source in the respective update script."

  ;; at least for the moment, permit requested value only if none were present
  (when (system-repository-url system) (setf repository-url (system-repository-url system)))
  (when (system-repository-type system) (setf repository-type (system-repository-type system)))

  (assert (and repository-url repository-type) ()
          "Invalid system repository information: ~a." system)
  (let* ((system-name (asdf:component-name system))
         (update-script (make-pathname :name "update-system" :type "sh" :defaults (definition-metadata-directory system))))
    (create-update-script update-script
                          (definition-source-directory system)
                          (definition-metadata-directory system)
                          repository-url
                          repository-type
                          system-name)
    (multiple-value-bind (return-code transcript-file)
                         (run-script (asdf:component-name system) update-script)
      (when (and (zerop return-code) repository-url)
        (setf (system-repository-url system) repository-url)
        (setf (system-repository-type system) repository-type)
        (save-repository-url (make-pathname :name (asdf:component-name system) :type "url"
                                            :defaults (definition-metadata-directory system))
                             :system-name system-name
                             :repository-url repository-url :repository-type repository-type))
      (values system return-code update-script transcript-file))))


(defgeneric perform-system-operation (system operation &rest args)
  (:method ((system-designator t) operation &rest args)
    (apply #'perform-system-operation (or (find-system-prototype system-designator)
                                      (error 'http::document-not-found
                                             :reason (format nil "System not found: ~s." system-designator)))
           operation
           args))

  (:method ((system asdf:system) (operation t) &rest args)
    (declare (ignore args))
    (error 'http::bad-syntax-provided
           :reason (format nil "invalid operation: ~s, ~s." (asdf:component-name system) operation)))

  (:method ((system-designator t) (operation (eql 'asdf:load-op)) &rest args)
    (apply #'perform-system-operation system-designator :compile-op args))
  
  (:method ((system asdf:system) (operation (eql 'asdf:compile-op))
            &key (lisp-implementation *default.build-implementation*))
    (let ((build-script (make-pathname :name "build-system" :type "sh" :defaults (definition-metadata-directory system))))
      (create-build-script build-script
                           (definition-source-directory system)
                           (definition-metadata-directory system)
                           (asdf:component-name system))
      (multiple-value-bind (return-code transcript-file)
                           (run-script (asdf:component-name system) build-script lisp-implementation)
      (values system return-code build-script transcript-file))))

  (:method ((system asdf:system) (operation (eql 'asdf:test-op))
            &key (lisp-implementation *default.build-implementation*) test-system)
    (let ((system-name (asdf:component-name system)))
      (unless test-system
        (setf test-system (or (find-system-prototype (cons-symbol :keyword system-name "-test"))
                              (find-system-prototype (cons-symbol :keyword system-name ".test"))
                              (error 'http::document-not-found
                                     :reason (format nil "No test system found for system: ~s."
                                                     system-name)))))
      (let ((test-script (make-pathname :name "test-system" :type "sh" :defaults (definition-metadata-directory system))))
        (create-test-script (make-pathname :name "test-system" :type "sh" :defaults (definition-metadata-directory system))
                            (definition-source-directory test-system)
                            (definition-metadata-directory system)
                            system-name)
        (multiple-value-bind (return-code transcript-file)
                             (run-script (asdf:component-name system) test-script lisp-implementation (asdf:component-name test-system))
          (values system return-code test-script transcript-file))))))
   

(defgeneric compute-system-rdf-projection (system dimension projection &rest args)
  (:method ((system t) (dimension string)(projection rdf-projection) &rest args)
    (apply #'compute-system-rdf-projection system (intern (string-upcase dimension) :keyword) projection 
           args))

  (:method ((system asdf:system) (dimension symbol) (projection rdf-projection) &key)
    (error "Invalid dependency dimension: ~a." dimension))

  (:method ((system asdf:system) (dimension (eql :systems)) (projection rdf-projection) &key)
    (values system
            `(,system ,@(mapcar #'(lambda (other) `(:depends-on ,other))
                                (remove-duplicates
                                 (collect-list (collector)
                                   (walk-systems system (list system)
                                                 #'(lambda (&rest args)
                                                     (collector (second args))))))))))
  
  (:method ((system asdf:system) (relation (eql :module)) (projection rdf-projection)  &rest args)
    (apply #'compute-system-rdf-projection system :modules projection args))

  (:method ((system asdf:system) (dimension (eql :modules)) (projection rdf-projection) &key)
    (values system
            `(,system ,@(mapcar #'(lambda (other) `(:component ,other))
                                (remove-duplicates
                                 (collect-list (collector)
                                   (walk-modules system (list system)
                                                 #'(lambda (&rest args)
                                                     (collector (second args))))))))))
  
  (:method ((system asdf:system) (dimension (eql :files)) (projection rdf-projection) &key)
    (values system
            `(,system ,@(mapcar #'(lambda (other) `(:component ,other))
                                (remove-duplicates
                                 (collect-list (collector)
                                   (walk-files system (list system)
                                               #'(lambda (&rest args)
                                                   (collector (second args))))))))))
  
  (:method ((system asdf:system) (dimension (eql :packages)) (projection rdf-projection) &key)
    ;; for the moment
    nil)

  (:method ((system asdf:system) (relation (eql :calls)) (projection rdf-projection) &key
            (lisp-implementation *default.graph-implementation*) (options nil) (extent ""))
    (setf relation (string-downcase relation))
    (let* ((rdf-file (make-pathname :name (format nil "~a" relation)
                                      :type (projection-model-type projection)
                                      :defaults (definition-metadata-directory system)))
           (rdf-script (make-pathname :name (format nil "rdf-system-~a" (pathname-name rdf-file))
                                      :type "sh"
                                      :defaults rdf-file))
           (rdf-date (file-write-date rdf-file))
           (rdf-script-date (file-write-date rdf-script)))
      ;; when both the script and the graph file exist _and_ the graph is newer, there's nothing to do
      (cond ((and rdf-date rdf-script-date (>= rdf-date rdf-script-date))
             (values system rdf-file rdf-script nil))
            (t                          ; if not up-to-date generate anew
             (create-rdf-script rdf-script
                                (definition-source-directory system)
                                (definition-metadata-directory system)
                                rdf-file
                                (asdf:component-name system)
                                relation extent
                                :options options)
             (multiple-value-bind (return-code transcript-file)
                                  (run-script (asdf:component-name system) rdf-script lisp-implementation)
               (if (zerop return-code)
                 (values system rdf-file rdf-script transcript-file)
                 (values system return-code rdf-script transcript-file)))))))

  (:method ((system asdf:system) (dimension (eql :tests)) (projection rdf-projection) &key)
    ;; for the moment
    nil))


(defgeneric perform-system-projection (system-name relation projection &key lisp-implementation mime-type extent options)

  (:method ((system-designator t) relation projection &rest args)
    (apply #'perform-system-projection (or (find-system-prototype system-designator)
                              (error 'http::document-not-found
                                     :reason (format nil "System not found: ~s." system-designator)))
           relation projection
           args))
  
  (:method ((system asdf:system) (relation t) (projection t) &rest args)
    (declare (ignore args))
    (error "Invalid projection: ~s ~s." relation projection))

  (:method ((system asdf:system) (relation t) (projection rdf-projection) &rest args)
    (declare (ignore args))
    (compute-system-rdf-projection system relation projection))

;;; (funcall (perform-system-projection :bordeaux-threads :module :object) t t *trace-output* :object mime:text/csv)
;;; (compute-system-rdf-projection (find-system-prototype :lisp-on-lines) :files)

  (:method ((system asdf:system) relation (projection graphviz-projection) &key 
            (lisp-implementation *default.graph-implementation*) extent (mime-type mime:image/svg+xml) options)
    "Generate and run a script to invoce the system's image and use it to generate the specified graph.
 If the resut encoding is not that of the direct result, runs a second script to encode the intermediate file."

    (setf relation (string-downcase relation))
    (let* ((application (projection-application projection))
           (graph-file (make-pathname :name (format nil "~a-~a" relation application)
                                      :type (projection-model-type projection)
                                      :defaults (definition-metadata-directory system)))
           (graph-script (make-pathname :name (format nil "graph-system-~a" (pathname-name graph-file))
                                        :type "sh"
                                        :defaults graph-file))
           (graph-date (file-write-date graph-file))
           (graph-script-date (file-write-date graph-script))
           (encoded-type (dsu:mime-type-file-type mime-type))
           (encoded-file (unless (string-equal encoded-type "dot")
                           (make-pathname :type encoded-type :defaults graph-file)))
           (encoded-date (and encoded-file (file-write-date encoded-file))))
      ;; when both the script and the graph file exist _and_ the graph is newer, there's nothing to do
      (cond ((and encoded-date graph-date (>= encoded-date graph-date)) (print :already-encoded)
             (values system encoded-file graph-script nil))
            ((and (not encoded-file) graph-date graph-script-date (>= graph-date graph-script-date)) (print :already-generated)
             (values system graph-file graph-script nil))
            (t                          ; if not up-to-date generate anew
             (create-graph-script graph-script
                                  (definition-source-directory system)
                                  (definition-metadata-directory system)
                                  graph-file
                                  (asdf:component-name system)
                                  relation extent
                                  encoded-file
                                  :projection projection
                                  :options options)
             (multiple-value-bind (return-code transcript-file)
                                  (run-script (asdf:component-name system) graph-script lisp-implementation)
               (if (zerop return-code)
                 (values system (or encoded-file graph-file) graph-script transcript-file)
                 (values system return-code  graph-script transcript-file))))))))

(defun graph-system-calls (system projection &rest args)
  (apply #'perform-system-projection system :calls projection args))

(defun graph-system-modules (system projection &rest args)
  (apply #'perform-system-projection system :modules projection args))

(defun graph-system-packages (system projection &rest args)
  (apply #'perform-system-projection system :packages projection args))



(defun perform-asdf-update (system)
  "Update the asdf source from the its repository and build all runtimes."

  (let ((update-script #p"LIBRARY:introspection-build.sh"))
    (multiple-value-bind (return-code transcript-file)
                         (run-script "asdf.production" update-script)
      (values system return-code update-script transcript-file))))


;; exhaustive builds
(defun build-all-systems (&key
                          (runtimes (directory (make-pathname :name :wild :type nil :defaults #p"METADATA:bin;lisp;")))
                          (build-runtimes (remove-if-not #'(lambda (impl)
                                                             (find impl runtimes :test #'string-equal :key #'pathname-name))
                                                         *build-implementations*))
                          (systems (sort (list-prototype-systems) #'string-lessp :key #'asdf:component-name)))
  (let* ((timestamp (date:|yyyyMMddTHHmmssZZ| (get-universal-time)))
         (name (format nil "build-~a" timestamp))
         (csv-stream (open (make-pathname :host "METADATA" :directory '(:absolute "lib" "html") :type "csv" :name name)
                           :direction :output :if-exists :supersede :if-does-not-exist :create))
         (html-stream (open (make-pathname :host "METADATA" :directory '(:absolute "lib" "html") :type "html" :name name)
                            :direction :output :if-exists :supersede :if-does-not-exist :create)))
    (format *trace-output* "Project Builds: ~% ~s~% ~s~%~%" (namestring csv-stream) (namestring html-stream))
    (de.setf.utility.implementation.xml::export-pathname (pathname csv-stream))
    (de.setf.utility.implementation.xml::export-pathname (pathname html-stream))
    (when (and csv-stream html-stream)
      (unwind-protect
        (xmlp:with-xml-writer (html-stream)
          (xmlp:encode-xml-declaration)
          (xmlp:encode-document-type "xhtml" :public xqdm:+xhtml-public-identifier+
                                     :system xqdm:+xhtml-system-identifier+)
          (xmlp:xml ({xhtml}html ({xmlns}||  "http://www.w3.org/1999/xhtml")
                                 ({xmlns}xlink  "http://www.w3.org/1999/xlink")) 
                    ({xhtml}head
                     (({}meta ({}name "date") ({}content timestamp)))
                     (({}meta ({}http-equiv "Content-Type")
                              ({xhtml}content "text/xhtml;charset=iso-8859-1"))))
                    
                    ({xhtml}body
                     ({xhtml}table
                      ({xhtml}tr ({xhtml}th "system")
                                 ({xhtml}th "start")
                                 (dolist (runtime build-runtimes)
                                   (xmlp:xml {xhtml}th (xmlp:encode-string runtime))
                                   (xmlp:xml {xhtml}th (xmlp:encode-format "~a duration" runtime)))
                                 ({xhtml}th "system duration"))
                      (xmlp:encode-newline)
                      (format csv-stream "SYSTEM,START~{,~a,~:*~a_DURATION~},DURATION" build-runtimes)
                      
                      (dolist (system systems)
                        (let* ((system-start (get-universal-time))
                               (runtime-start system-start)
                               (runtime-end 0)
                               (total 0) (success 0)
                               (system-name (asdf:component-name system)))
                          (xmlp:xml {xhtml}tr
                                      (format csv-stream "~&~a" system-name)
                                      ({xhtml}th (xmlp:encode-string system-name))
                                      ;; (format csv-stream ",~/date:format-excel-time/" runtime-start)
                                      (format csv-stream ",~/date:format-iso-time/" runtime-start)
                                      ({xhtml}td (xmlp:encode-format "~/date:format-iso-time/" runtime-start))
                                      (dolist (runtime build-runtimes)
                                        (handler-case
                                          (multiple-value-bind (system return-code script transcript-file)
                                                               (perform-system-operation system 'asdf:compile-op :lisp-implementation runtime)
                                            (declare (ignore system script))
                                            (incf total)
                                            (if (zerop return-code) (incf success))
                                            (setf runtime-end (get-universal-time))
                                            (format csv-stream ",~d" return-code)
                                            (xmlp:xml ({xhtml}td
                                                       ({}style (if (zerop return-code) "background-color: green;" "background-color: red;")))
                                                      (({xhtml}a ({}href (de.setf.utility.implementation.xml::pathname-url-namestring transcript-file)))
                                                       (xmlp:encode-format "~d" return-code))))
                                          (error (c)
                                                 (warn "System ~s signalled error: ~a" system c)
                                                 (setf runtime-end (get-universal-time))
                                                 (format csv-stream ",~a,~d" (type-of c) (- runtime-end runtime-start))
                                                 (xmlp:xml ({xhtml}td ({}style "background-color: red;"))
                                                           (xmlp:encode-format "~a" (type-of c)))))
                                        (format csv-stream ",~d" (- runtime-end runtime-start))
                                        (xmlp:xml {xhtml}td (xmlp:encode-format "~d" (- runtime-end runtime-start)))
                                        (setf runtime-start runtime-end))
                                      (format csv-stream ",~d" (- runtime-end system-start))
                                      ({xhtml}td (xmlp:encode-format "~d" (- runtime-end system-start))))
                            (xmlp:encode-newline)
                          (format *trace-output* " ~a: ~d/~d" system-name success total))
                        (finish-output csv-stream)
                        (finish-output html-stream))
                      (terpri csv-stream)
                      (xmlp:encode-newline))
                     (xmlp:encode-newline))
                    (xmlp:encode-newline))
          (xmlp:encode-newline))
        (when html-stream (close html-stream))
        (when csv-stream (close csv-stream))))))

#+(or)
(build-all-systems)

#+(or)
(build-all-systems :systems (subseq (member "cl-unification-lib" (sort (list-prototype-systems) #'string-lessp :key #'asdf:component-name)
                                            :test #'string-equal :key #'asdf:component-name)
                                    0 1))

#+(or)
(build-all-systems :systems (mapcar #'find-system-prototype
                                    '("alexandria" "cl-unification-lib" "cl-unification" "elephant" "montezuma")))

#+(or)
(progn
  ;; bootstrap
  (multiple-value-list
   (ignore-errors
    (perform-system-new "clbuild" 
                        :repository-url "http://common-lisp.net/project/clbuild/clbuild"
                        :repository-type :darcs)))

  (perform-system-new "cffi-net" :repository-url "http://cffi-net.accela.net/darcs/cffi-net/" :repository-type :darcs)
  (perform-system-new "ftd" :repository-url "http://common-lisp.net/~dlichteblau/inofficial/ftd/" :repository-type :darcs)
  (perform-system-new "trivial-features" :repository-url "http://common-lisp.net/~loliveira/darcs/trivial-features"  :repository-type :darcs)
  (perform-system-new "relational-objects-for-lisp" :repository-url "http://common-lisp.net/project/lisp-on-lines/repo/relational-objects-for-lisp" :repository-type :darcs)

  (perform-system-new "bordeaux-threads"
                      :repository-url "http://common-lisp.net/project/bordeaux-threads/darcs/bordeaux-threads/")

  (perform-system-update "bordeaux-threads"
                         :repository-url "http://common-lisp.net/project/bordeaux-threads/darcs/bordeaux-threads/")

  (perform-system-build "bordeaux-threads" "ccl")

  (graph-system "bordeaux-threads" "ccl" :modules "bordeaux-threads" :dot :jpg)
  (graph-system "bordeaux-threads" "ccl" :modules "bordeaux-threads" :twopi :svg)


  http://yoda.setf.de:8000/lisp/update-system?system=net.common-lisp.bordeaux-threads
  http://yoda.setf.de:8000/lisp/build-system?system=net.common-lisp.bordeaux-threads&lisp-implementation=ccl
  "http://yoda.setf.de:8000/lisp/test-system?system=net.common-lisp.bordeaux-threads&lisp-implementation=ccl&test-system=net.common-lisp.bordeaux-threads-test"

  
  (apply #'ensure-system-directories (assoc :net.common-lisp.bordeaux-threads *system-repositories*))
  
  (destructuring-bind (&key add-system &allow-other-keys)
                      (ensure-system-directories :net.common-lisp.bordeaux-threads)
    (run-script "test" add-system))
  
  (destructuring-bind (&key update-script &allow-other-keys)
                      (ensure-system-directories :net.common-lisp.bordeaux-threads)
    (run-script "test" update-script))
  
  (destructuring-bind (&key build-script &allow-other-keys)
                      (ensure-system-directories :net.common-lisp.bordeaux-threads)
    (run-script "test" build-script "ccl"))

  (destructuring-bind (&key test-script &allow-other-keys)
                      (ensure-system-directories :net.common-lisp.bordeaux-threads)
    (run-script "test" test-script "ccl" "net.common-lisp.bordeaux-threads-test"))

  
  (apply #'ensure-system-directories (assoc :net.common-lisp.lift *system-repositories*))
  (destructuring-bind (&key add-system transcript-file &allow-other-keys)
                      (ensure-system-directories :net.common-lisp.lift)
    (run-script "test" add-system transcript-file))
  


  ;; initial graphs
  (let ((prototypes (list-prototype-systems))
        (*WALK-EXTENT-BOUNDARY* :closed)
        (asdf::*defined-systems* *prototype-systems*))
    (graph-systems prototypes
                   :root prototypes
                   :name "allProjects"
                   :stream #p"METADATA:ecosystem-modules.dot"
                   :pretty t
                   :url-encoder #'component-url
                   :size "50,50"
                   :graph-arguments '( :ranksep "3" )   ; :overlap "scale"
                   :node-test #'(lambda (component) (typep component 'asdf:module))
                   :link-test #'(lambda (component other relation)
                                  (declare (ignore relation))
                                  (and (typep component 'asdf:module)
                                       (typep other 'asdf:module)))
                   :relation-map '((asdf:compile-op (asdf:compile-op . requires)
                                                    (asdf:load-op . requires))
                                   (asdf:load-op (asdf:compile-op . requires)
                                                 (asdf:load-op . requires)))))
  ;; (trace put-component-graph-statement component-url)
  ;; cd $METADATA
  ;; dot -Tsvg -oecosystem-modules-dot.svg ecosystem-modules.dot
  (length (list-prototype-systems))
  (first (list-prototype-systems))


#| links to add buttons
<g id="toother" transform="scale(0.187988 0.187988) rotate(0) translate(195.502 9379.58)">
<a xlink:href="./opensource-projects-2pi.svg" xlink:title="to 2pi" xlink:target="opensource-projects-2pi">
<rect fill="#f0f0f0" stroke="#000000" x="1600" y="-9180" width="410" height="100"/>
<rect fill="none" stroke="#000000" x="1605" y="-9175" width="400" height="90"/>
<text text-anchor="middle" x="1800" y="-9110" font-family="Times Roman,serif" font-size="80.00"> -> 2pi</text>
</a>
</g>

<g id="toother" transform="scale(0.916168 0.916168) rotate(0) translate(43.2941 1680.91)">
<a xlink:href="./opensource-projects-dot.svg" xlink:title="to dot" xlink:target="opensource-projects-dot">
<rect fill="#e0e0e0" stroke="#000000" x="0" y="-1640" width="200" height="40"/>
<rect fill="none" stroke="#f0f0f0" x="1" y="-1639" width="198" height="38"/>
<text text-anchor="middle" x="100" y="-1614" font-family="Times Roman,serif" font-size="30.00"> -> dot</text>
</a>
</g>
|#
  )
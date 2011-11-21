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
echo \"retrieving system : ~a\"
if (!(test -d ${sources}))
then
  echo \"No source directory found: ${sources}.\"
  exit 1
fi
cd  ${sources}
cd ..
rm -rf ~a
~a
cd ${sources}
ln -fs  *.asd ~a~%")

(defparameter *update-script* "#! /bin/bash~%~%
export PATH=~{~a:~}$PATH
sources=~a
if (!(test -d ${sources}))
then
  echo \"No source directory found: ${sources}.\"
  exit 1
fi
# one could do this to add the .asd files to the registry.
# probably not the best tactic, since they're untested code.
# the test script does it for the individual test system
# cd  ${sources}
# ~a~%")

(defparameter *build-script* "#! /bin/bash~%~%
## build a system image
## if an image exists with introspection tools use that to augment the runtime
export PATH=~{~a:~}$PATH
if ( test -z \"${1}\" ) then echo \"runtime implementation required\"; exit 1; fi
lisp=`which ${1}`
if ( test -z \"${lisp}\" ) then echo \"No lisp found: ${1}.\"; exit 1; fi
if ( test -f \"${lisp}.image\" )
then
  lispimage=\"${lisp}.image\"
else
  lispimage = ''
fi

cd ~a
${lisp} ${lispimage} <<EOF
;;; load the build init file - a distinct form as the asdf package is not yet
(load ~s)
(handler-case
  (let ((*compile-verbose* t)
        (*load-verbose* t))
    ;; add the system source directory to the registry search path
    (pushnew ~s asdf:*central-registry* :test #'equalp)
    (let ((system ~s))
      (format *trace-output* \"~~&;Load system ~~s... \" system)
      (asdf:load-system system))
    (format *trace-output* \"~~&;Load completed. Saving and exit... \")
    ;; save the image in the system-specific directory
    (let ((image-path \"~a${1}.image\"))
      (cl-user::save-image image-path)
      (format *trace-output* \"~~%;Image saved to ~~s.\" image-path))
    (cl-user::leave-lisp \"Build complete\" 0))
  (error (c)
    (warn \"Build failed with error: ~~a.\" c)
    (cl-user::leave-lisp \"Build failed\" 255)))
EOF~%")

(defparameter *test-script* "#! /bin/bash~%~%
export PATH=~{~a:~}$PATH
if ( test -z \"${1}\" ) then echo \"runtime implementation required\"; exit 1; fi
if ( test -z \"${2}\" ) then echo \"test system required\"; exit 1; fi
lisp=`which ${1}`
if ( test -z \"${lisp}\" ) then echo \"No lisp found: ${1}.\"; exit 1; fi

cd ~a
${lisp} \"${1}.image\" <<EOF
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
    (cl-user::leave-lisp \"Tests failed\" 255)))
EOF~%")

(defparameter *graph-script* "#! /bin/bash~%~%
export PATH=~{~a:~}$PATH
lisp=`which ${1}`
if ( test -z \"${lisp}\" )
then
  echo \"No lisp found: ${1}.\"
  exit 1
fi

cd ~a
${lisp} \"${1}.image\" <<EOF
(in-package :cl-user)
(load #p\"METADATA:lib;graph-init.lisp\")                               ; initialization code
(handler-case
  (progn (encode-graph :~a \'~s :root ~s :stream ~s :options \'~s)
         (format *trace-output* \"~~&;Graph complete.\")
         (leave-lisp \"graph generation complete\" 0))
  (error (c)
    (warn \"Graph failed with error: ~~a.\" c)
    (leave-lisp \"graph generation failed\" 255)))
EOF
~(~a~) -T~(~a -o~a ~a~)
echo graphviz vomplete.
")


(defun create-update-script (pathname source-directory metadata-directory repository-url repository-type)
  (declare (ignore metadata-directory))
  (with-open-file (script-stream pathname :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format script-stream  *update-script*
            *shell-path*
            (file-unix-namestring source-directory)
            (case repository-type
              (:git (format nil "git pull" repository-url))
              (:darcs (format nil "darcs pull" repository-url))
              (:svn (format nil "svn update" repository-url))))))


(defun create-build-script (pathname source-directory metadata-directory system-name)
  (with-open-file (script-stream pathname :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format script-stream  *build-script*
            *shell-path*
            (file-unix-namestring source-directory)
            (if (equal system-name "asdf")
              (file-unix-namestring #p"METADATA:lib;build-asdf-init.lisp")
              (file-unix-namestring #p"METADATA:lib;build-init.lisp"))
            (file-unix-namestring source-directory)
            system-name
            (file-unix-namestring metadata-directory))))


(defun create-test-script (pathname test-system-source-directory metadata-directory system-name)
  (with-open-file (script-stream pathname :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format script-stream  *test-script*
            *shell-path*
            (file-unix-namestring metadata-directory)
            (file-unix-namestring test-system-source-directory)
            system-name)))

(defun create-graph-script (pathname source-directory metadata-directory output-filename system-name dimensions extent
                                     &key (projection "dot") (encoding "svg") (options '()))
  (declare (ignore source-directory))
  (with-open-file (script-stream pathname :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format script-stream *graph-script*
            *shell-path*
            (file-unix-namestring metadata-directory)
            dimensions
            (remove-duplicates (cons system-name (split-string extent ", ")) :test #'equal :from-end t)
            system-name
            (file-namestring output-filename)
            options
            projection
            encoding
            (file-namestring (make-pathname :type (string encoding) :defaults output-filename))
            (file-namestring output-filename))))


(defun run-script (system-name script-filename &rest args)
  "Run a shell script with given arguments. redirect all output to an analogously names '.txt' file.
 Return the script's exist code and the name of the transcript file."
  #-mcl (setf script-filename (file-unix-namestring script-filename))
  (multiple-value-bind (second minute hour day month year) (decode-universal-time (get-universal-time) 0)
    (declare (ignore second minute hour))
    (flet ((~d (x) (format nil "~2,'0d" x)))
      (let ((output-filename (make-pathname :host "METADATA"
                                            :directory `(:absolute "reports" ,(~d year) ,(~d month) ,(~d day))
                                            :name (format nil "~a-~a-~/date:format-iso-time/"
                                                          system-name (pathname-name script-filename) (get-universal-time))
                                            :type "txt")))
        (ensure-directories-exist output-filename)
        #-mcl (setf output-filename (file-unix-namestring output-filename))
        (values (asdf:run-shell-command "sh ~a ~{~a ~}> ~a 2>&1" script-filename args output-filename)
                output-filename)))))



(defun initialize-system-directories (system source-pathname &key repository-url repository-type)
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
                *shell-path* unix-source-directory  system last-directory-name
                (ecase repository-type
                  (:git (format nil "git clone ~a ~a" repository-url last-directory-name))
                  (:darcs (format nil "darcs get ~a ~a" repository-url last-directory-name))
                  (:svn (format nil "svn co ~a ~a" repository-url last-directory-name)))
                unix-registry-directory))

      (list :metadata-directory metadata-directory
            :source-directory source-directory))))


(defun perform-system-new (system-name &key repository-url repository-type)
  "If the system is known, reuse the existing upload location. Otherwise use a location analogous with a known
 library, or corresponding with the url path."
  (let* ((system (find-system-prototype system-name))
         (upload-directory (cond (system (or (definition-source-directory system)
                                             (error 'http::server-internal-error 
                                                    :reason "Prototype system lacks source directory: ~s" system)))
                                 ((library-upload-directory system-name))
                                 ((url-upload-directory repository-url system-name))
                                 (t
                                  (error 'http::bad-syntax-provided
                                         :reason "Cannot derive a source location given system and url: ~s, ~s."
                                         system-name repository-url)))))
    (destructuring-bind (&key metadata-directory source-directory)
                        (initialize-system-directories system-name upload-directory
                                                       :repository-url repository-url
                                                       :repository-type repository-type)
      (let ((new-script (make-pathname :name "new-system" :type "sh" :defaults metadata-directory)))
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
                      (setf asd-name (asdf:component-name asd-system))
                      (when (equalp asd-name system-name)
                        (setf system asd-system))
                      (save-repository-url (make-pathname :name asd-name :type "url"
                                                          :defaults (definition-metadata-directory asd-system))
                                           :system-name asd-name
                                           :repository-url repository-url :repository-type repository-type)
                      (setf (system-repository-url asd-system) repository-url)
                      (setf (system-repository-type asd-system) repository-type)))))))
          (values system return-code new-script transcript-file))))))


(defun perform-system-update (system &key
                                     (repository-url (system-repository-url system) ru-s)
                                     (repository-type (if ru-s
                                                        (or (repository-type repository-url) (system-repository-type system))
                                                        (system-repository-type system))))
  "Update the system source from the system's repository. If a new repository url is provided, change
 the source in the respective update script."

  (assert (and repository-url repository-type) ()
          "Invalid system repository information: ~a." system)
  (let ((update-script (make-pathname :name "update-system" :type "sh" :defaults (definition-metadata-directory system))))
    (create-update-script update-script
                          (definition-source-directory system)
                          (definition-metadata-directory system)
                          repository-url
                          repository-type)
    (multiple-value-bind (return-code transcript-file)
                         (run-script (asdf:component-name system) update-script)
      (when (and (zerop return-code) repository-url)
        (setf (system-repository-url system) repository-url)
        (setf (system-repository-type system) repository-type)
        (save-repository-url (make-pathname :name (asdf:component-name system) :type "url"
                                            :defaults (definition-metadata-directory system))
                             :system-name (asdf:component-name system)
                             :repository-url repository-url :repository-type repository-type))
      (values system return-code update-script transcript-file))))


(defgeneric perform-system-operation (system operation &rest args)
  (:method ((system-designator t) operation &rest args)
    (apply #'perform-system-build (or (find-system-prototype system-designator)
                                      (error 'http::document-not-found
                                             :reason (format nil "System not found: ~s." system-designator)))
           operation
           args))

  (:method ((system-designator t) (operation (eql 'asdf:load-op)) &rest args)
    (apply #'perform-system-build system-designator :compile-op args))
  
  (:method ((system asdf:system) (operation (eql 'asdf:compile-op))
            &key (lisp-implementation de.setf.utility.implementation.xml::*default.build-implementation*))
    (let ((build-script (make-pathname :name "build-system" :type "sh" :defaults (definition-metadata-directory system))))
      (create-build-script build-script
                           (definition-source-directory system)
                           (definition-metadata-directory system)
                           (asdf:component-name system))
      (multiple-value-bind (return-code transcript-file)
                           (run-script (asdf:component-name system) build-script lisp-implementation)
      (values system return-code build-script transcript-file))))

  (:method ((system asdf:system) (operation (eql 'asdf:test-op))
            &key (lisp-implementation de.setf.utility.implementation.xml::*default.build-implementation*) test-system)
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
   


(defgeneric compute-system-projection (system-name relation projection &key lisp-implementation extent options)

  (:method ((system-designator t) relation projection &rest args)
    (apply #'compute-system-projection (or (find-system-prototype system-designator)
                              (error 'http::document-not-found
                                     :reason (format nil "System not found: ~s." system-designator)))
           relation projection
           args))

  (:method ((system asdf:system) (relation t) (projection (eql :object)) &rest args)
    (declare (ignore args))
    (let ((result (compute-system-relations system relation)))
      (flet ((encode-projection (system request stream projection mime-type)
               (declare (ignore system request))
               (de.setf.utility.implementation.xml::encode-system-projection  result stream projection mime-type)))
        (values #'encode-projection system nil nil)))))

;;; (funcall (compute-system-projection :bordeaux-threads :module :object) t t *trace-output* :object mime:text/csv)
;;; (compute-system-relations (find-system-prototype :lisp-on-lines) :files)




(defgeneric project-system (system lisp-implementation relations extent &key projection encoding options)
  (:method ((system-designator t) lisp-implementation relations extent &rest args)
    (apply #'graph-system (or (find-system-prototype system-designator)
                              (error 'http::document-not-found
                                     :reason (format nil "System not found: ~s." system-designator)))
           lisp-implementation relations extent
           args))

  (:method ((system asdf:system) lisp-implementation relations extent &key (projection "dot") (encoding "svg") options)
    (setf relations (string-downcase relations)
          projection (string-downcase projection)
          encoding (string-downcase encoding))
    (let* ((graph-script (make-pathname :name (format nil "graph-system-~a-~a" relations projection)
                                        :type "sh"
                                        :defaults (definition-metadata-directory system)))
           (dot-file (make-pathname :name (format nil "~a-~a" relations projection)
                                    :type "dot" :defaults graph-script))
           (graph-script-date (file-write-date graph-script))
           (graph-date (file-write-date dot-file))
           (output-file (make-pathname :type encoding :defaults dot-file)))
      ;; when both the script and the output file exist _and_ the output is newer, there's nothing to do
      (cond ((and graph-date graph-script-date (>= graph-date graph-script-date))
             (values system 0 graph-script output-file nil))
            (t
             (create-graph-script graph-script
                                  (definition-source-directory system)
                                  (definition-metadata-directory system)
                                  dot-file
                                  (asdf:component-name system)
                                  relations extent
                                  :projection projection
                                  :encoding encoding
                                  :options options)
             (multiple-value-bind (return-code transcript-file)
                                  (run-script (asdf:component-name system) graph-script lisp-implementation)
               (copy-file graph-script (make-pathname :name "graph-system" :defaults graph-script) :if-exists :supersede)
               (values system return-code graph-script output-file transcript-file)))))))

(defun graph-system-calls (system implementation extent &rest args)
  (apply #'graph-system system implementation :calls extent
         args))

(defun graph-system-modules (system implementation extent &rest args)
  (apply #'graph-system system implementation :modules extent
         args))

(defun graph-system-packages (system implementation extent &rest args)
  (apply #'graph-system system implementation :packages extent
         args))





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
  (let ((prototypes (list-prototype-systems)))
    (graph-systems prototypes
                   :root prototypes
                   :name "allProjects"
                   :stream "METADATA:ecosystem-modules.dot"
                   :pretty t
                   :url-encoder #'component-url
                   :size "32,32"
                   :node-test #'(lambda (component) (typep component 'asdf:module))
                   :link-test #'(lambda (component other relation)
                                  (declare (ignore relation))
                                  (and (typep component 'asdf:module)
                                       (typep other 'asdf:module)))
                   :relation-map '((asdf:compile-op (asdf:compile-op . requires)
                                                    (asdf:load-op . requires))
                                   (asdf:load-op (asdf:compile-op . requires)
                                                 (asdf:load-op . requires)))))
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
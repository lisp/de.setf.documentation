;;; -*- package: cl-user -*-

(in-package :cl-user)

;; the service build requires pre-load for http
;; for mcl 5.2
;; fix http::*standard-character-type* to be 'character
;; add to http:www;robots.text
;;Allow: /systems
;;Allow: /metadata/lib/html/index.html
#+ccl-5.2
(progn                                  ; nb. the package doesn't exist yet
  (asdf:load-system :org.cl-http)
  (http:digest-authentication-random-seed t)    ; x n
  (http:save-authentication-object
   (http:intern-user :server "Webmaster"
                     :personal-name "mr anderson"
                     :password "lisp@cl-http"
                     :email-address "james.anderson@setf.de"))
  (http:add-realm :repositories :digest)
  (http:add-group :repositories :submitters)
  (http:add-group :repositories :maintainers)
  (http:add-user "Librarian" :repositories)
  (http:save-authentication-object
   (http:intern-user :repositories "Librarian"
                     :groups '(:submitters :maintainers)
                     :personal-name "mr anderson"
                     :password "20100409@ec2"
                     :email-address "james.anderson@setf.de"))
  (asdf:load-system :de.setf.documentation.service)
  (let ((*load-verbose* nil))
    (de.setf.utility.implementation::load-system-prototypes))
  (de.setf.utility.implementation.xml::export-documentation-urls))

;; (in-package :de.setf.utility.implementation.xml)
#+(or )
(progn
  (xmlp:with-xml-writer (*trace-output*)
    (encode-project-div nil *trace-output* :system-name "alexandria" :xhr-request-p nil))

  (xmlp:with-xml-writer (*trace-output*)
    (/system/*/index.html  nil *trace-output* :system-name "alexandria"))

  (xmlp:with-xml-writer (*trace-output*)
    (/system/*/index.div  nil *trace-output* :system-name "alexandria"))
  )

;; the runtimes to support applciation graphs need just the docuentation package
;;
;; cd $LIBRARY
;;

;; abcl :
;; /development/bin/abcl --load build-init.lisp


;; allegro alisp :
;;
;; /Development/Applications/LISP/AllegroCL/alisp -qq -L build-init.lisp
;; /development/bin/alisp -qq -L build-init.lisp
#+allegro
(cl-user::save-image (make-pathname :directory (append (butlast (pathname-directory *build-init-pathname*))
                                                       '("metadata" "bin" "lisp"))
                                    :name "acl" :type "image"))

;; clisp
;; /development/bin/clisp -i build-init.lisp
#+clisp
(cl-user::save-image (make-pathname :directory (append (butlast (pathname-directory *build-init-pathname*))
                                                       '("metadata" "bin" "lisp"))
                                    :name "clisp" :type "image"))

;; clozure:
;;
;; $ ccl=/Development/Applications/LISP/ccl-1-4/dppccl
;; $ cd /development/source/library
;; $ ccl --no-init -l build-init.lisp
;;
#+clozure
(progn
  ;; first the image for building projects
  (asdf:load-system :org.cl-http)
  (http:add-realm :repositories :digest)
  (http:add-group :repositories :submitters)
  (http:add-group :repositories :maintainers)
  (http::initialize-server-authentication)
  (asdf:load-system :de.setf.documentation.service)
  (cl-user::save-image (make-pathname :host "METADATA"
                                      :directory '(:absolute "bin" "lisp")
                                      :name "ccl" :type "image"))

  ;; then the server ! built in a different order
  ;; (asdf:load-system :org.cl-http)
  (setq de.setf.utility.implementation::*excluded-packages*
        (list-all-packages))
  (cl-user::save-image (make-pathname :host "LIBRARY"
                                      :directory '(:absolute)
                                      :name "ccl-cl-http" :type "image"))
  ;; ccl --no-init -I ccl-cl-http.image

  (setq http:*server-mail-address* "james.anderson@setf.de")
  (setq http:*bug-http-server* "james.anderson@setf.de")
  ;; fails to permit connections because socket creation fails.
  (setq *resolve-ip-addresses* nil)
  (http:save-authentication-object
   (http:intern-user :server "Webmaster"
                     :personal-name "mr anderson"
                     :password "lisp@cl-http"
                     :email-address "james.anderson@setf.de"))
  (http:add-user "Librarian" :repositories)
  (http:save-authentication-object
   (http:intern-user :repositories "Librarian"
                     :groups '(:submitters :maintainers)
                     :personal-name "mr anderson"
                     :password "20100409@ec2"
                     :email-address "james.anderson@setf.de"))
  (let ((*load-verbose* nil))
    (de.setf.utility.implementation::load-system-prototypes))

  (cl-user::save-image (make-pathname :host "LIBRARY"
                                      :directory '(:absolute)
                                      :name "ccl-service" :type "image"))

  ;; ccl --no-init -I ccl-service.image
  (http::set-local-context "http://ec2-174-129-66-148.compute-1.amazonaws.com:8000")
  (http::initialize-server-authentication)
  (http::load-examples)
  (http:digest-authentication-random-seed t) ;; x n

  ;; see simple-server.lisp in kpoeck;allegro ...
  ;; the context must reflect the external location - otherwise internal links are
  ;; to the internal host and do not work
  (http::add-virtual-host-nick-name "ip-10-251-122-82.ec2.internal"
                                    8000
                                    (http:local-context))
  (http::start :hostname "ec2-174-129-66-148.compute-1.amazonaws.com" :port 8000 :type :stupid-multi)
  (http::disable-proxy-service)         ; if it starts
  (de.setf.utility.implementation.xml::export-documentation-urls)
  (http::log-file-logging-on (http::current-access-logs) t)
  ;; (http::stop)

  )

;; update
;; rsync -avz /Development/Source/dev/Library/de/setf/documentation ${ec2}:/development/source/library/de/setf/
;; then  + authentication controls
;; (load (compile-file #p"LIBRARY:de;setf;documentation;system-operations.lisp"))
;; (load (compile-file #p"LIBRARY:de;setf;documentation;service;xhtml-encoding.lisp"))
;; (load (compile-file #p"LIBRARY:de;setf;documentation;service;xhtml-interface.lisp"))
;;

;; cmucl :
;;
;; $ cmucl=/development/bin/cmucl
;; $ cd /development/source/library
;; $ $cmucl -load build-init.lisp  # with patch to overwrite library search path
#+cmu
(cl-user::save-image (make-pathname :directory (append (butlast (pathname-directory *build-init-pathname*))
                                                       '("metadata" "bin" "lisp"))
                                    :name "cmucl" :type "image"))

;; in order to do any ecl work, need to re-install the gmp package --
;; -- it's not saved in the os image
;; cd /development/downloads/gmp-4.3.2
;; sudo make install
;; # load it all first to make sure it's ok and to get the compler ops for building
;; /development/bin/ecl -load build-init.lisp
;; nb. load in the initialization brings in the build operators
;; still two failures:
#+ecl
(let ((o-files (loop for lisp in '(#p"LIBRARY:net;common-lisp;asdf;asdf.lisp"
                                   #p"LIBRARY:net;common-lisp;asdf;asdf-ecl.lisp"
                                   #p"LIBRARY:de;setf;utility;package.lisp"
                                   #p"LIBRARY:de;setf;utility;pathnames.lisp"
                                   #p"LIBRARY:de;setf;utility;asdf;hierarchical-names.lisp")
                     collect (compile-file lisp :system-p t))))
  (c:build-program (make-pathname :name "ecl" :type "image") :lisp-files o-files)
  (c:build-fasl (make-pathname :name "ecl" :type "fas") :lisp-files o-files))
;; cp ecl.fas /development/source/metadata/bin/lisp/

;; /usr/local/bin/sbcl --no-sysinit --userinit build-init.lisp
#+sbcl  !! can't build it until (gray) issues are resolved
(progn
  ;; (asdf:load-system :de.setf.documentation)
  (cl-user::save-image (make-pathname :directory (append (butlast (pathname-directory *build-init-pathname*))
                                                       '("metadata" "bin" "lisp"))
                                      :name "sbcl" :type "image")))

;; checking loading the prototypes
#+(or)
(let ((*load-print* t) (*load-verbose* nil))
  (de.setf.utility.implementation::load-system-prototype
   #p"yoda:Development:Source:dev:upload:org:cyrusharmon:cl-bio:cl-bio-taxonomy.asd"))
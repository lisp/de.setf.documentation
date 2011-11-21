;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: url; -*-


(in-package :url)

;;
;;
;;

(defclass git-url (http-object user-id-and-pw-mixin)
  ((url::scheme :initform "git")
   (url::protocol :initform "git")))

(defclass svn-url (http-object user-id-and-pw-mixin)
  ((url::scheme :initform "svn")
   (url::protocol :initform "svn")))

(defclass darcs-url (http-object user-id-and-pw-mixin)
  ((url::scheme :initform "darcs")
   (url::protocol :initform "darcs")))


(define-scheme-parser
  git
  (:classes (git-url))
  (url start end)
  (multiple-value-bind (user-id pw host-index host-end)
      (get-user-id-and-password url start end)
    (multiple-value-bind (host-string port path-index)
        (get-host-port-info url host-index host-end t)
      ;; extract the path components
      (multiple-value-bind (path object-index next-index)
          (get-path-info url path-index end)
        ;; The spec requires a path. Should we signal an error when there
        ;; is no path?  create the appropriate URL
        (cond ;; get the object components when present         
          (object-index
           (multiple-value-bind (object extension)
               (get-object-info url object-index next-index)
             (make-instance 'git-url
                            :name-string (when *retain-original-name-string* (subseq url start end))
                            :host-string host-string
                            :port port
                            :user-id user-id
                            :password pw
                            :path path
                            :object object
                            :extension extension)))
          (t
           (error "invalid url syntax: ~s" url)))))))

(defmethod canonicalize-url ((scheme (eql :git)) url-string &optional (start 0) (end (length url-string)) destructive-p)
  (%canonicalize-host-prefixed-url :git "git://" 6 url-string start end destructive-p))

(define-scheme-parser
  svn
  (:classes (svn-url))
  (url start end)
  (multiple-value-bind (user-id pw host-index host-end)
      (get-user-id-and-password url start end)
    (multiple-value-bind (host-string port path-index)
        (get-host-port-info url host-index host-end t)
      ;; extract the path components
      (multiple-value-bind (path object-index next-index)
          (get-path-info url path-index end)
        ;; The spec requires a path. Should we signal an error when there
        ;; is no path?  create the appropriate URL
        (cond ;; get the object components when present         
          (object-index
           (multiple-value-bind (object extension)
               (get-object-info url object-index next-index)
             (make-instance 'svn-url
                            :name-string (when *retain-original-name-string* (subseq url start end))
                            :host-string host-string
                            :port port
                            :user-id user-id
                            :password pw
                            :path path
                            :object object
                            :extension extension)))
          (t
           (error "invalid url syntax: ~s" url)))))))

(define-scheme-parser
  darcs
  (:classes (darcs-url))
  (url start end)
  (multiple-value-bind (user-id pw host-index host-end)
      (get-user-id-and-password url start end)
    (multiple-value-bind (host-string port path-index)
        (get-host-port-info url host-index host-end t)
      ;; extract the path components
      (multiple-value-bind (path object-index next-index)
          (get-path-info url path-index end)
        ;; The spec requires a path. Should we signal an error when there
        ;; is no path?  create the appropriate URL
        (cond ;; get the object components when present         
          (object-index
           (multiple-value-bind (object extension)
               (get-object-info url object-index next-index)
             (make-instance 'darcs-url
                            :name-string (when *retain-original-name-string* (subseq url start end))
                            :host-string host-string
                            :port port
                            :user-id user-id
                            :password pw
                            :path path
                            :object object
                            :extension extension)))
          (t
           (error "invalid url syntax: ~s" url)))))))

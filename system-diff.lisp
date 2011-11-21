;;; -*- package: de.setf.utility.implementation -*-

(in-package :de.setf.utility.implementation)

(:documentation "Generate a graph which records the difference between
 two versions of an ASDF system."

  (description "Given a list of systems, walk the respective call graph and record all
 relations annotated by version. Collect the links and sort by call precedence.
 assign each version a color. Generate a graph in which each node/link color is 
 the union of the version's color in which it is present."))

(defparameter *system-versions* ())

(defun system-watermark (&key (exclude ()))
  (let* ((registry (when (find-package :asdf)
                    (symbol-value (find-symbol (string :*defined-systems*) :asdf))))
         (name-op (when registry (fdefinition (find-symbol (string :component-name) :asdf)))))
    (when registry
      (loop for entry being each hash-value of registry
            for system = (rest entry)
            unless (find (funcall name-op system) exclude :test #'string=)
            collect system))))

(defun package-watermark (&key (exclude ()))
  (setf exclude (mapcar #'(lambda (e)
                            (etypecase e
                              (package (package-name e))
                              ((or string symbol) (string e))))
                        exclude))
  (remove-if #'(lambda (p) (intersection (cons (package-name p) (package-nicknames p))
                                         exclude
                                         :test #'string=))
             (list-all-packages)))

(defstruct system-version name version systems packages functions calls color)


(defun load-system-versions (name &key sources (packages (list name)) (systems (list name)))
  (let ((system-watermark (system-watermark :exclude systems))
        (package-watermark (package-watermark :exclude packages))
        (system-versions ()))
    (dolist (source sources)
        (let ((load-op nil)
              (name-op nil)
              (registry nil)
              (system-type nil))
          (flet ((load-op ()
                   (or load-op (setf load-op (fdefinition (find-symbol (string :load-system) :asdf)))))
                 (name-op ()
                   (or name-op (setf name-op (fdefinition (find-symbol (string :component-name) :asdf)))))
                 (registry ()
                   (or registry (setf registry (symbol-value (find-symbol (string :*defined-systems*) :asdf)))))
                 (system-type ()
                   (or system-type (setf system-type (find-symbol (string :system) :asdf)))))
            (destructuring-bind (source &key version color) source
              (cond ((pathnamep source)
                     (load source)
                     (dolist (system (system-watermark :exclude system-watermark))
                       (funcall (load-op) system)))
                    ((or (typep source (system-type)) (stringp source) (symbolp source))
                     (funcall (load-op) source))
                    (t
                     (error "invalid source: ~s." source)))
              (let* ((systems (system-watermark :exclude (mapcar (name-op) system-watermark)))
                     (packages (package-watermark :exclude package-watermark))
                     (functions ())
                     (calls ()))
                (flet ((collect (function &optional (other-function nil other-p) relation)
                         (when (typep function 'function)
                           (cond ((and (eq relation 'calls) other-p)
                                  ;; a call
                                  (when (typep other-function 'function)
                                    (push (cons function other-function) calls)))
                                 (t
                                  (push function functions))))))
                  
                  (walk-image packages packages #'collect
                              :excluded-qualifiers '(callers))
                  
                  (push (make-system-version :name name
                                             :version version
                                             :systems systems
                                             :packages packages
                                             :functions functions
                                             :calls calls
                                             :color color)
                        system-versions)
          
                  (dolist (system systems)
                    (remhash (funcall (name-op) system) (registry)))
                  (cerror "Delete the packages" "Loaded packages to be deleted: ~a?" packages)
                  (dolist (package packages)
                    (dolist (user (package-used-by-list package))
                      (unuse-package package user))
                    (delete-package package))))))))
    system-versions))


;; (remove-if-not #'(lambda (p) (search "ASDF" (package-name p))) (list-all-packages))
#+(or )
(let ((*load-verbose* t))
  (setq *system-versions* (load-system-versions :asdf
                                                :packages '(:asdf :asdf-extensions :asdf-utilities)
                                                :sources '((#p"LIBRARY:net;common-lisp;asdf-20090617;asdf.lisp" :version 1.352 :color #x00ff00)
                                                           (#p"LIBRARY:net;common-lisp;asdf;asdf.lisp" :version 1.502 :color #xFFFF00)
                                                           (#p"UPLOAD:net;common-lisp;asdf-external;asdf.lisp" :version 1.700 :color #xff0000))))
  (length *system-versions*))

(defun graph-system-versions (versions &key (name nil) (stream *standard-output*) graph-arguments)
  "First, collect the function instances and calls. For each one, record the instance and ther version.
 Then sort the call list by call precedence. Then emit function instances, colored by version appearance.
 Finally emit calls, with the same coloring."
  (flet ((function-nickname (function packages)
           (let* ((designator (dsw:function-name function))
                  (name (if (consp designator) (second designator) designator))
                  (package (symbol-package name)))
             (if (or (null package) (member package packages))
               (symbol-name name)
               (let ((p-nick (or (first (package-nicknames package))
                                 (package-name package))))
                 (concatenate 'string p-nick ":" (symbol-name name)))))))
    ;; operate on the versions oldest-first
    (setf versions (sort (copy-list versions) #'< :key #'system-version-version))
             
    (let ((functions (make-hash-table :test 'equal))
          (calls (make-hash-table :test 'equal))
          (max-color 0)
          (name name)
          (all-color #xa0a0a0)
          (max-version nil))
      (dolist (version versions)
        (setf max-version version)
        (setf max-color (+ max-color (system-version-color version)))
        (setf name (format nil "~@[~a-~]~a" name (system-version-version version)))
        ;; indicate the presense of the function / function-call for this version
        (let ((packages (system-version-packages version)))
          (dolist (function (system-version-functions version))
            (pushnew version (gethash (function-nickname function packages) functions)))
          (dolist (call (system-version-calls version))
            (destructuring-bind (caller . called) call
              (let ((caller-name (function-nickname caller packages))
                    (called-name (function-nickname called packages)))
                (pushnew version (gethash (cons caller-name called-name) calls)))))))

      ;; emit a graph in which the color indicate the appearance version (last in the list), and
      ;; whether it is still present in the latest version
      (labels ((color (versions)
                 (let ((color (system-version-color (first (last versions))))
                       (latest-version (first versions)))
                   (if (eq latest-version max-version)
                     color
                     (+ color #x7f7f7f))))
               
               (emit-version-graph ()
                 (maphash #'(lambda (name versions)
                              (let ((color (color versions))
                                    ;; iff it's still present, fill it
                                    (filled (eq (first versions) max-version)))
                                (when (eql color max-color) (setf color all-color))
                                (setf.dot:put-node name :color (format nil "#~6,'0x" color)
                                                   :style (when filled "filled")
                                                   :fillcolor (when filled (format nil "#~6,'0x3f" color)))))
                          functions)
                 (maphash #'(lambda (call versions)
                              (let ((color (color versions)))
                                (when (eql color max-color) (setf color all-color))
                                (destructuring-bind (caller . called) call
                                  (setf.dot:put-edge caller called :color (format nil "#~6,'0x" color)))))
                          calls)))
        (apply #'setf.dot:context-put-graph stream name
               #'emit-version-graph
               graph-arguments)))))

#+(or)
(graph-system-versions *system-versions* :name "asdf versions: 1.500/1.702" :stream #p"METADATA:images;asdf-versions.dot"
                       :graph-arguments '(:size "10,10"
                                          ; :overlap "scale"
                                          :ranksep "4"
                                          ))
#+(or)
(mapcar #'(lambda (v) (list (system-version-version v) (length (system-version-functions v)) (length (system-version-calls v))))
        *system-versions*)

#+(or)
(inspect *system-versions*)
;;; (dsw:function-calls #'asdf::output-files)
;;; (c2mop:generic-function-methods #'asdf::output-files)
;;; (dsw:function-calls (first (c2mop:generic-function-methods #'asdf::output-files)))
;;; (map nil #'print (ccl::disassemble-list (c2mop:method-function (first (c2mop:generic-function-methods #'asdf::output-files)))))

#+(or)
(graph-functions (list (find-package :asdf) (find-package :asdf-utilities))
                 :stream #p"METADATA:images;asdf-calls.dot"
                 :root (list #'asdf::output-files #'asdf::input-files)
                 :graph-arguments '(:size "10,10" :ranksep "4"))
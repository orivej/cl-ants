;;;; client.lisp

(in-package #:quicklisp-client)

(defvar *quickload-verbose* nil)
(defvar *quickload-prompt* nil)
(defvar *quickload-explain* t)

(define-condition system-not-quickloadable (error)
  ((system
    :initarg :system
    :reader not-quickloadable-system)))

(defgeneric quickload (systems &key verbose prompt explain &allow-other-keys)
  (:documentation
   "Load SYSTEMS the quicklisp way. SYSTEMS is a designator for a list
   of things to be loaded.")
  (:method (systems &key prompt verbose &allow-other-keys)
    (unless (consp systems)
      (setf systems (list systems)))
    (dolist (thing systems systems)
      (flet ((ql ()
               (autoload-system-and-dependencies thing :prompt prompt)))
        (if verbose
            (ql)
            (call-with-quiet-compilation #'ql))))))

(defmethod quickload :around (systems &key verbose prompt explain
                                      &allow-other-keys)
  (declare (ignorable systems verbose prompt explain))
  (with-consistent-dists
    (call-next-method)))

(defun system-list ()
  (provided-systems t))

(defun update-dist (dist &key (prompt t))
  (when (stringp dist)
    (setf dist (find-dist dist)))
  (let ((new (available-update dist)))
    (cond (new
           (show-update-report dist new)
           (when (or (not prompt) (press-enter-to-continue))
             (update-in-place dist new)))
          ((not (subscribedp dist))
           (format t "~&You are not subscribed to ~S."
                   (name dist)))
          (t
           (format t "~&You already have the latest version of ~S: ~A.~%"
                   (name dist)
                   (version dist))))))

(defun update-all-dists (&key (prompt t))
  (let ((dists (remove-if-not 'subscribedp (all-dists))))
    (format t "~&~D dist~:P to check.~%" (length dists))
    (dolist (old dists)
      (with-simple-restart (skip "Skip update of dist ~S" (name old))
        (update-dist old :prompt prompt)))))

(defun help ()
  "For help with Quicklisp, see http://www.quicklisp.org/beta/")

(defun uninstall (system-name)
  (let ((system (find-system system-name)))
    (when system
      (ql-dist:uninstall system))))

(defun uninstall-dist (name)
  (let ((dist (find-dist name)))
    (when dist
      (ql-dist:uninstall dist))))

(in-package :cl-user)
(defpackage cl-brewer
  (:use :cl)
  (:export :main))

(in-package :cl-brewer)

(defclass <formula> ()
  (
   (missing-systems
    :initarg :missing-systems
    :accessor missing-systems)
   (included-systems
    :initarg :included-systems
    :accessor included-systems)))

(defun main (args)
  (if (stringp args)
      (main (list args))
      (let* ((system (car args))
             (system-deps (asdf::system-depends-on (asdf::find-system system)))
             (formula (expand-deps system-deps)))
        (format nil "The system in question is ~a with existing deps ~a and missing deps~a" system (included-systems formula) (missing-systems formula))
        (if (> (length (missing-systems formula)) 0)
            (format nil "There are systems that cannot be found with quicklisp, aborting there. ~a" (missing-systems formula))
            (format nil "Dependencies lookup was successful, proceeding")
            ))))

(defun expand-deps (deps)
  (let ((existing-systems)
        (missing-systems))
    (labels ((expand-dep (name)
               (let ((system (ql-dist:find-system name)))
                 (cond (system
                        (push system existing-systems)
                        (dolist (subname (ql-dist:required-systems system))
                          (expand-dep subname)))
                       (t
                        (push name missing-systems))))))
      (dolist (subname deps) (expand-dep subname))
      (make-instance '<formula>
                     :missing-systems (remove-duplicates missing-systems :test #'string=)
                     :included-systems (remove-duplicates existing-systems)))))

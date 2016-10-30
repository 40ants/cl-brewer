(in-package :cl-user)
(defpackage cl-brewer
  (:use :cl)
  (:export :main))

(in-package :cl-brewer)

(defclass <formula> ()
  (
   (root-system
    :initarg :root-system
    :accessor root-system)
   (missing-systems
    :initarg :missing-systems
    :accessor missing-systems)
   (included-systems
    :initarg :included-systems
    :accessor included-systems)))

(defun main (args)
  (if (stringp args)
      (main (list args))
      (let* ((formula (create-formula (car args))))
        (if (> (length (missing-systems formula)) 0)
            (format nil "There are systems that cannot be found with quicklisp, aborting there. ~a" (missing-systems formula))
            (format nil "Dependencies lookup was successful, proceeding ~a" formula)
            ))))

(defgeneric create-formula (system))

(defmethod create-formula ((system string))
  (create-formula (asdf::find-system system)))

(defmethod create-formula (system)
  (let ((deps (asdf::system-depends-on system))
        (existing-systems)
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
                     :root-system system
                     :missing-systems (remove-duplicates missing-systems :test #'string=)
                     :included-systems (remove-duplicates existing-systems)))))

(defgeneric print-formula (formula))

(defmethod print-formula ((formula <formula>))
  (format nil "
class ~a < Formula
  desc ~S
  homepage ~S
  url ~S
  sha256 ~S
  head ~S
"
          (name formula)
          (description formula)
          (home-page formula)
          (url formula)
          (sha256 formula)
          (repo-head formula)
          ))

(defun name (formula)
  (asdf::component-name (root-system formula)))

(defun description (formula)
  (asdf::component-description (root-system formula)))

(defun home-page (formula)
    (declare (ignore formula))
  "<home-page to be implemented>")

(defun url (formula)
  (declare (ignore formula))
  "<url to be implemented>")

(defun sha256 (formula)
  (declare (ignore formula))
  "<sha256 to be implemented>")

(defun repo-head (formula)
  (declare (ignore formula))
  "<head to be implemented>")

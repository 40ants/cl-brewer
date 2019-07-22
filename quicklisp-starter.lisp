(defpackage #:ql-setup
  (:use #:cl)
  (:export #:*quicklisp-home*
           #:qmerge
           #:qenough))
(in-package ql-setup)

(defvar *quicklisp-home* "")


(defun qmerge (pathname)
  "Return PATHNAME merged with the base Quicklisp directory."
  (merge-pathnames pathname *quicklisp-home*))


(defun qenough (pathname)
  (enough-namestring pathname *quicklisp-home*))


;; (eval-when (:compile-toplevel
;;             :load-toplevel
;;             :execute)
;;   (format t "====================================================================================================~%")
;;   (format t "SBCL: ~A~%"
;;           (lisp-implementation-version))
;;   (format t "====================================================================================================~%")
;;   (setf sb-ext:*on-package-variance*
;;         nil
;;         ;; '(:warn (:swank :swank-backend :swank-repl) :error t)
;;         ))

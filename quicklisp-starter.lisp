(defpackage #:ql-setup
  (:use #:cl)
  (:export #:*quicklisp-home*
           #:qmerge
           #:qenough))
(in-package ql-setup)

(defvar *quicklisp-home*
  (asdf:component-pathname
   (asdf:find-system :quicklisp)))


(defun qmerge (pathname)
  "Return PATHNAME merged with the base Quicklisp directory."
  (merge-pathnames pathname *quicklisp-home*))


(defun qenough (pathname)
  (enough-namestring pathname *quicklisp-home*))

#|
This file is a part of cl-brewer project.
|#

(in-package :cl-user)
(defpackage cl-brewer2-asd
  (:use :cl :asdf))
(in-package :cl-brewer2-asd)

(defsystem cl-brewer
  :description "Homebrew formula builder for common lisp applications"
  :author "Dmitry Petrov <dpetroff@gmail.com>"
  :version "0.1.0"
  :license "Public Domain"
  :depends-on (#:quicklisp
               #:ironclad)
  :components ((:module "src"
                :components
                (
                 (:file "main"))))
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.md"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op cl-brewer-test))))

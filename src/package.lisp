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

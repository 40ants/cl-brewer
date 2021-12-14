(in-package :cl-user)

(defpackage cl-brewer
  (:use :cl :command-line-arguments)
  (:import-from #:alexandria
                #:make-keyword)
  (:export #:main
           #:create-formula
           #:save-formula
           #:get-implicit-dependencies
           #:formula))

(in-package :cl-brewer)

(defclass formula ()
  ((root-system
    :initarg :root-system
    :accessor root-system)
   (missing-systems
    :initarg :missing-systems
    :accessor missing-systems)
   (included-systems
    :initarg :included-systems
    :accessor included-systems)))


(defclass buildapp-formula (formula)
  ())


(defclass deploy-formula (formula)
  ())


(defmethod print-object ((obj formula) stream)
  (print-unreadable-object (obj stream :type t)
    (let ((missing (length (missing-systems obj))))
      (format stream
              "~S depends on ~A system~:p~@[ but ~A of them are missing in known dists~]"
              (asdf:component-name (root-system obj))
              (length (included-systems obj))
              (when (> missing 0)
                missing)))))

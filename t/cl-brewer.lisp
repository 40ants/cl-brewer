(in-package :cl-user)
(defpackage cl-brewer-test
  (:use :cl
   :cl-brewer
        :prove))
(in-package :cl-brewer-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-brewer)' in your Lisp.

(plan nil)

(finalize)

#|
This file is a part of cl-brewer project.
|#

(in-package :cl-user)
(defpackage cl-brewer-test-asd
  (:use :cl :asdf))
(in-package :cl-brewer-test-asd)

(defsystem cl-brewer-test
  :author ""
  :license ""
  :depends-on (:cl-brewer
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "cl-brewer"))))
  :description "Test system for cl-brewer"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))

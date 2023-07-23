(uiop:define-package #:cl-brewer-tests/core
  (:use #:cl)
  (:import-from #:rove
                #:deftest
                #:ok
                #:testing))
(in-package #:cl-brewer-tests/core)


(deftest test-example ()
  (ok t "Replace this test with something useful."))

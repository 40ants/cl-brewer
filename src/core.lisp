(uiop:define-package #:cl-brewer
  (:use #:cl)
  (:nicknames #:cl-brewer/core)
  
  (:export #:create-formula
           #:save-formula
           #:get-implicit-dependencies
           #:formula))
(in-package #:cl-brewer)

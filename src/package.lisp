(defpackage #:cl-brewer
  (:use #:cl
        #:command-line-arguments)
  (:import-from #:alexandria
                #:make-keyword)
  (:export #:main
           #:create-formula
           #:save-formula
           #:get-implicit-dependencies
           #:formula))
(in-package #:cl-brewer)





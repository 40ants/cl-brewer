(uiop:define-package #:cl-brewer
  (:use #:cl)
  (:nicknames #:cl-brewer/core)
  
  ;; We need these dependencies to make
  ;; sure these packages will be loaded
  ;; before we'll try to recycle symbols
  ;; from them:
  (:import-from #:cl-brewer/formula)
  (:import-from #:cl-brewer/formula-impl)
  
  (:export #:create-formula
           #:save-formula
           #:get-implicit-dependencies
           #:formula)
  (:recycle #:cl-brewer
            #:cl-brewer/formula
            #:cl-brewer/formula-impl))
(in-package #:cl-brewer)

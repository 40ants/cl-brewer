(uiop:define-package #:cl-brewer/buildapp/formula
  (:use #:cl)
  (:import-from #:cl-brewer/formula
                #:name
                #:print-dependencies
                #:print-build-commands
                #:define-quesser)
  (:export
   #:buildapp-formula))
(in-package #:cl-brewer/buildapp/formula)


(defclass buildapp-formula (formula)
  ()
  (:documentation "This formula class uses [Buildapp](https://xach.com/lisp/buildapp/) to build a binary."))


(define-quesser guess-buildapp-formula (system)
  (let* ((build-operation (asdf/component:component-build-operation system)))
    (unless (string-equal build-operation
                        "deploy-op")
      'deploy-formula)))


(defmethod print-build-commands ((formula buildapp-formula)
                                 &key (stream t) entry-point preload
                                 &allow-other-keys)
  (format stream
          "
    system \"buildapp\", \"--compress-core\", ")
  
  (dolist (item (alexandria:ensure-list preload))
    (format stream "\"--load-system\", \"~A\", " item))
  
  (format stream
          "\"--load-system\", \"~A\", \"--output\", \"~A\", \"--entry\", \"~A\"

    bin.install ~S
"
          (name formula)
          (name formula)
          (if (null entry-point)
              (format nil "~a.main" (name formula))
              entry-point)
          (name formula)))


(defmethod print-dependencies :after ((formula buildapp-formula) &key (stream t))
  (format stream "  depends_on \"buildapp\" => :build~%"))

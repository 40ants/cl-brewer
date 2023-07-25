(uiop:define-package #:cl-brewer/deploy/hooks
  (:use #:cl)
  (:import-from #:deploy
                #:define-hook))
(in-package #:cl-brewer/deploy/hooks)

(defvar *libexec-path*
  (uiop:getenv "LIBEXEC_PATH"))


(define-hook (:deploy ignore-system-libraries) ()
  (loop for lib in (deploy:list-libraries)
        when (eql (cffi:foreign-library-type lib)
                  :system)
          do (setf (deploy:library-dont-deploy-p lib)
                   t)
          and collect (or (deploy:library-path lib)
                          (deploy:library-name lib)) into ignored-libs
        finally (when ignored-libs
                  (deploy:status 1 "Ignoring these system libraries:")
                  (loop for lib in ignored-libs
                        do (deploy:status 2 "~A" lib)))))


(define-hook (:boot restore-path-to-libexec
              ;; we need to ensure this hook will be executed before
              ;; Deploy's attempt to load libraries:
              (1+ most-positive-fixnum)) ()
  (cond
    (*libexec-path*
     (deploy:status 0 "Adding ~A to cffi:*foreign-library-directories*."
                    *libexec-path*)
     (push *libexec-path*
           cffi:*foreign-library-directories*))
    (t
     (deploy:status 0 "~S is NIL"
                    '*libexec-path*))))


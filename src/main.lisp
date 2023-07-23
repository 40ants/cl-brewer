(uiop:define-package #:cl-brewer/main
  (:use #:cl)
  (:import-from #:cl-brewer/formula
                #:missing-systems
                #:create-formula)
  (:import-from #:command-line-arguments
                #:show-option-help
                #:handle-command-line)
  (:import-from #:cl-brewer/formula-impl
                #:save-formula)
  (:import-from #:cl-brewer/utils
                #:split-string))
(in-package #:cl-brewer/main)


(defparameter +command-line-spec+
  '(
    (("main" #\m) :type string
                  :optional t
                  :documentation "Specify an entrypoint for a system. If system uses Deploy, then entrypoint is determined automatically from ASDF system definition. Otherwise, default is main function in a package with system's name.")
    (("preload" #\p) :type string
                     :optional t
                     :documentation "A comma-separated list of system names to preload before starting a build.")
    (("help" #\h) :type boolean
                  :optional t
                  :documentation "Show help message")
    (("version" #\v) :type boolean
                     :optional t
                     :documentation "Show program version")))

(defun get-version ()
  (asdf:component-version
   (asdf:find-system :cl-brewer)))


(defun print-help ()
  (format t "cl-brewer version ~a~%
Usage: cl-brewer [options] <system-name>~%~%" (get-version))
  (show-option-help +command-line-spec+ :sort-names t))


(defun print-version ()
  (format t "cl-brewer version ~a~%" (get-version)))


(defun split (text)
  (when text
    (split-string text #\,)))


(defun start-slynk-or-slime ()
  (when (uiop:getenv "SLYNK_PORT")
    (let ((port (parse-integer (uiop:getenv "SLYNK_PORT")))
          (interface (or (uiop:getenv "SLYNK_INTERFACE")
                         "127.0.0.1")))
      (format t "Starting slynk on ~A:~A~%" interface port)
    
      (unless (find-package :slynk)
        (format t "Unable to find SLYNK package. Please, build program with Slynk support."))

      ;; To make it possible to connect to a remote SLYNK server where ports are closed
      ;; with firewall.
      (setf (symbol-value (intern "*USE-DEDICATED-OUTPUT-STREAM*"
                                  (find-package :slynk)))
            nil)
      (uiop:symbol-call :slynk :create-server 
                        :port port
                        :interface interface
                        :dont-close t)
      (format t "Waiting for connection to Slynk on ~A:~A port~%"
              interface port)
      (ignore-errors
       (unwind-protect (loop do (sleep 1))
         (format t "Continuing~%")))))


  (when (uiop:getenv "SWANK_PORT")
    (let ((port (parse-integer (uiop:getenv "SWANK_PORT")))
          (interface (or (uiop:getenv "SWANK_INTERFACE")
                         "127.0.0.1")))
      (format t "Starting Swank on ~A:~A~%" interface port)
    
      (unless (find-package :swank)
        (format t "Unable to find SWANK package. Please, build program with Swank support."))

      (uiop:symbol-call :swank :create-server 
                        :port port
                        :interface interface
                        :style :spawn
                        :dont-close t)
      (format t "Waiting for connection to Swank on ~A:~A port~%"
              interface port)
      (ignore-errors
       (unwind-protect (loop do (sleep 1))
         (format t "Continuing~%"))))))


(defun bake-system (args &key main help preload version)
  ;; We need this to make it possible to run a binary under qlot exec
  (setf (uiop:getenv "SBCL_HOME") "")

  ;; We need this to make it possible to run cl-brewer under the Qlot
  ;; If we don't check for existence of QUICKLISP_HOME variable
  ;; and make (quicklisp:setup), then quicklisp client creates
  ;; dist, local-projects and tmp in the current directory.
  (when (uiop:getenv "QUICKLISP_HOME")
    (setf ql:*quicklisp-home*
          (probe-file
           (or (uiop:getenv "QUICKLISP_HOME")
               "")))

    (let ((*trace-output* (make-broadcast-stream)))
      (quicklisp:setup)))
  
  ;; Buildapp version does not work without this line
  (asdf:clear-configuration)
  
  (start-slynk-or-slime)


  (let ((name (car args))
        (preload (split preload)))
    (cond
      (version (print-version))
      ((or help (null name)) (print-help))
      (t
       (format t "Creating formula for ~S...~%" name)

       ;; We need to remove all systems known to ASDF and having a filename,
       ;; because these filenames belong to a temprorary directory, created by Homebrew.
       ;; Without this, cl-brewer can fail to load some systems, which depends on the
       ;; same libraries as cl-brewer do.
       (let* ((orig-registered-systems asdf/system-registry:*registered-systems*)
              (asdf/system-registry:*registered-systems* (make-hash-table :test 'equal)))
         (maphash (lambda (key system)
                    (unless (and (slot-boundp system 'asdf/component:absolute-pathname)
                                 (slot-value system 'asdf/component:absolute-pathname))
                      (setf (gethash key asdf/system-registry:*registered-systems*)
                            system)))
                  orig-registered-systems)
         ;; Quickload does not load :defsystem-depends-on :(
         (tagbody
          retry
            (handler-case (ql:quickload name :silent t)
              (asdf:missing-dependency (condition)
                (let ((required-dependency
                       (alexandria:make-keyword
                        (string-upcase (asdf/find-component:missing-requires
                                        condition)))))
                  (format t "Loading ~A's depdendency: ~A~%"
                          name
                          required-dependency)
                  (ql:quickload required-dependency)
                  (format t "RETRYING~%")
                  (go retry)))))
       
         (format t "System ~A was loaded~%"
                 name)
         
         (let* ((formula (create-formula name)))
           (format t "Formula was created: ~S~%" formula)
           (cond ((> (length (missing-systems formula)) 0)
                  (format t "There are systems that cannot be found with quicklisp, aborting there. ~a~%"
                          (missing-systems formula)))
                 (t
                  (format t "Dependencies lookup was successful, proceeding~%")
                  (save-formula formula name
                                :entry-point main
                                :preload preload)))))))))

(defun main (&rest args)
  (handle-command-line
   +command-line-spec+
   'bake-system
   :command-line args
   :name "cl-brewer"
   :positional-arity 0
   :rest-arity t))


(defun asdf-main ()
  (apply #'main
         (uiop:command-line-arguments)))


(defun buildapp-main (args)
  ;; First item in args is a program's name
  ;; we need to strip it first.
  (apply #'main (cdr args)))


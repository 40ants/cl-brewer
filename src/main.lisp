(in-package :cl-brewer)

(defparameter *version* "0.3.0")

(defparameter +command-line-spec+
  '(
    (("main" #\m) :type string
                  :optional t
                  :documentation "Specify an entrypoint for a system. Default is main function in a package with system's name")
    (("preload" #\p) :type string
                     :optional t
                     :documentation "A comma-separated list of system names to preload before starting a build.")
    (("help" #\h) :type boolean
                  :optional t
                  :documentation "Show help message")))


(defun print-help ()
  (format t "cl-brewer version ~a~%
Usage: cl-brewer [options] <system-name>~%~%" *version*)
  (show-option-help +command-line-spec+ :sort-names t))


(defun split (text)
  (when text
    (split-string text #\,)))


(defun bake-system (args &key main help preload)
  (let ((name (car args))
        (preload (split preload)))
    (cond
      ((or help (null name)) (print-help))
      (t
       (format t "Creating formula for ~S...~%" name)
       (let* ((formula (create-formula name)))
         (format t "Formula was created: ~S~%" formula)
         (cond ((> (length (missing-systems formula)) 0)
                (format t "There are systems that cannot be found with quicklisp, aborting there. ~a~%"
                        (missing-systems formula)))
               (t
                (format t "Dependencies lookup was successful, proceeding~%")
                (save-formula formula name
                              :entry-point main
                              :preload preload))))))))

(defun main (&rest args)
  (handle-command-line
   +command-line-spec+
   'bake-system
   :command-line args
   :name "cl-brewer"
   :positional-arity 0
   :rest-arity t))


(defun buildapp-main (args)
  ;; First item in args is a program's name
  ;; we need to strip it first.
  (apply #'main (cdr args)))


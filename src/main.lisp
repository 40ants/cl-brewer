(in-package :cl-brewer)

(defparameter *version* "0.3.0")

(defparameter +command-line-spec+
  '(
    (("main" #\m) :type string
                  :optional t
                  :documentation "Specify an entrypoint for a system. Default is main function in a package with system's name")
    (("help" #\h) :type boolean
                  :optional t
                  :documentation "Show help message")
    ))

(defun print-help ()
  (format t "cl-brewer version ~a~%
Usage: cl-brewer [options] <system-name>~%~%" *version*)
  (show-option-help +command-line-spec+ :sort-names t))

(defun bake-system (args &key main help)
  (let ((name (car args)))
    (cond
      ((or help (null name)) (print-help))
      (t (let* ((formula (create-formula name)))
           (if (> (length (missing-systems formula)) 0)
               (format t "There are systems that cannot be found with quicklisp, aborting there. ~a~%" (missing-systems formula))
               (progn
                 (format t "Dependencies lookup was successful, proceeding~%")
                 (save-formula formula name :entry-point main))))))))

(defun main (&rest args)
  (handle-command-line
   +command-line-spec+
   'bake-system
   :command-line args
   :name "cl-brewer"
   :positional-arity 0
   :rest-arity t))


(defun buildapp-main (args)
  (apply #'main args))


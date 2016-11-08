(in-package :cl-brewer)

(defun main (args)
  (if (stringp args)
      (main (list args))
      (let* ((formula (create-formula (car args))))
        (if (> (length (missing-systems formula)) 0)
            (format t "There are systems that cannot be found with quicklisp, aborting there. ~a~%" (missing-systems formula))
            (progn
              (format t "Dependencies lookup was successful, proceeding~%")
              (save-formula formula (car args)))
            ))))

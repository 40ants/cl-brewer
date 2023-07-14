(in-package :cl-brewer)

(defgeneric sha256 (formula))

(defmethod sha256 ((formula formula))
  (let ((fname (make-pathname :directory '(:absolute "tmp")
                              :name (substitute #\- #\/ (name formula))))
        (url (url formula)))
    (trivial-download:download url fname)
    (sha256 fname)))

(defmethod sha256 ((path pathname))
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-file :sha256 path)))

(defmethod sha256 ((release ql-dist:release))
  (sha256 (ql-dist:ensure-local-archive-file release)))

(defun split-string (string chr)
  (loop for i = 0 then (1+ j)
        as j = (position chr string :start i)
        collect (subseq string i j)
        while j))


(defun startswith (prefix string)
  (and (<= (length prefix)
           (length string))
       (string-equal prefix
                     string
                     :end2 (length prefix))))

(defun rubyize-name (name)
  (apply #'concatenate 'string
         (mapcar #'(lambda (str) (string-capitalize str :start 0 :end 1))
                 (split-string (substitute #\- #\/ name) #\-))))

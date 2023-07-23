(uiop:define-package #:cl-brewer/utils
  (:use #:cl)
  (:import-from #:cl-brewer/hash
                #:sha256)
  (:import-from #:ironclad
                #:byte-array-to-hex-string
                #:digest-file))
(in-package #:cl-brewer/utils)


(defmethod sha256 ((path pathname))
  (byte-array-to-hex-string
   (digest-file :sha256 path)))

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

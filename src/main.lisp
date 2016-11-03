(in-package :cl-user)
(defpackage cl-brewer
  (:use :cl)
  (:export :main))

(in-package :cl-brewer)

(defclass <formula> ()
  (
   (root-system
    :initarg :root-system
    :accessor root-system)
   (missing-systems
    :initarg :missing-systems
    :accessor missing-systems)
   (included-systems
    :initarg :included-systems
    :accessor included-systems)))

(defun main (args)
  (if (stringp args)
      (main (list args))
      (let* ((formula (create-formula (car args))))
        (if (> (length (missing-systems formula)) 0)
            (format nil "There are systems that cannot be found with quicklisp, aborting there. ~a" (missing-systems formula))
            (format nil "Dependencies lookup was successful, proceeding ~a" formula)
            ))))

(defgeneric create-formula (system))

(defmethod create-formula ((system string))
  (create-formula (asdf::find-system system)))

(defmethod create-formula (system)
  (let ((deps (asdf::system-depends-on system))
        (existing-systems)
        (missing-systems))
    (labels ((expand-dep (name)
               (let ((system (ql-dist:find-system name)))
                 (cond (system
                        (push system existing-systems)
                        (dolist (subname (ql-dist:required-systems system))
                          (expand-dep subname)))
                       (t
                        (push name missing-systems))))))
      (dolist (subname deps) (expand-dep subname))
      (make-instance '<formula>
                     :root-system system
                     :missing-systems (remove-duplicates missing-systems :test #'string=)
                     :included-systems (remove-duplicates existing-systems)))))

(defgeneric print-formula (formula))

(defmethod print-formula ((formula <formula>))
  (format t "~:
class ~a < Formula
  desc ~S
  homepage ~S
  url ~S
  sha256 ~S
  head ~S

  depends_on \"sbcl\"
  depends_on \"buildapp\" => :build

~{~a~^~%~}

  def install
    resources.each do |resource|
      resource.stage buildpath/\"lib\"/resource.name
    end

    ENV[\"CL_SOURCE_REGISTRY\"] = \"#{buildpath}/lib//:#{buildpath}//\"
    ENV[\"ASDF_OUTPUT_TRANSLATIONS\"] = \"/:/\"
    system \"buildapp\", \"--load-system\", \"~A\", \"--output\", \"~A\", \"--entry\", \"cl-journal.main'

    bin.install ~S
  end
"
          (name formula)
          (description formula)
          (home-page formula)
          (url formula)
          (sha256 formula)
          (repo-head formula)
          (mapcar #'print-formula (included-systems formula))
          (name formula)
          (name formula)
          (name formula)
          ))

(defmethod print-formula ((dist ql-dist:system))
  (let ((release (ql-dist:release dist)))
    (format nil "~:
  resource ~S do
    url ~S
    sha256 ~S
  end~%~%"
            (ql-dist:name release)
            (ql-dist::archive-url release)
            (sha256 dist))))

(defun name (formula)
  (asdf::component-name (root-system formula)))

(defun description (formula)
  (asdf::component-description (root-system formula)))

(defun home-page (formula)
  (asdf::system-homepage (root-system formula)))

(defun url (formula)
  (concatenate 'string (home-page formula)
               "/archive/v"
               (asdf::component-version (asdf::find-system :cl-journal))
               ".tar.gz"))

(defun repo-head (formula)
  (asdf::system-source-control (root-system formula)))

(defgeneric sha256 (formula))

(defmethod sha256 ((formula <formula>))
  (declare (ignore formula))
  "<sha256 to be implemented>")

(defmethod sha256 ((path pathname))
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-file :sha256 path)))

(defmethod sha256 ((dist ql-dist:system))
  (sha256 (ql-dist:ensure-local-archive-file (ql-dist:release dist))))

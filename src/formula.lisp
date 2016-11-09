(in-package :cl-brewer)


(defparameter +formula-body+ "~:
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
    system \"buildapp\", \"--load-system\", \"~A\", \"--output\", \"~A\", \"--entry\", \"~A\"

    bin.install ~S
  end
end
")

(defparameter +dependency-body+ "~:
  resource ~S do
    url ~S
    sha256 ~S
  end~%~%")


(defun save-formula (formula name entry-point)
  (let* ((output-file (make-pathname :name name :type "rb")))
    (with-open-file (stream output-file
                            :direction :output
                            :if-exists :overwrite
                            :if-does-not-exist :create)
      (print-formula formula :stream stream :entry-point entry-point))))

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

(defgeneric print-formula (formula &key stream entry-point))

(defmethod print-formula ((formula <formula>) &key (stream t) entry-point)
  (format stream +formula-body+
          (rubyize-name (name formula))
          (description formula)
          (home-page formula)
          (url formula)
          (sha256 formula)
          (repo-head formula)
          (mapcar #'print-formula (included-systems formula))
          (name formula)
          (name formula)
          (if (null entry-point)
              (format nil "~a.main" (name formula))
              entry-point)
          (name formula)
          ))

(defmethod print-formula ((dist ql-dist:system) &key (stream t) entry-point)
  (declare (ignore stream entry-point))
  (let ((release (ql-dist:release dist)))
    (format nil +dependency-body+
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

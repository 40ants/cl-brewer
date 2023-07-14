(in-package #:cl-brewer)


(defparameter +formula-header+ "~:
class ~a < Formula
  desc ~S
  homepage ~S
  url ~S
  sha256 ~S
  head ~S
")

(defparameter +formula-footer+ "~:
end
")


(defparameter +dependency-body+ "~:
  resource ~S do
    url ~S
    sha256 ~S
  end~%~%")


;; we skip systems that we know for sure are available
(defparameter +whitelisted-systems+ '("asdf" "sb-introspect"))


(defun save-formula (formula name &key entry-point preload)
  (let* ((output-file (make-pathname :name name :type "rb")))
    (with-open-file (stream output-file
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (print-formula formula
                     :stream stream
                     :entry-point entry-point
                     :preload preload))))

(defgeneric create-formula (system)
  (:documentation "Create <formula> object based on asdf:system with a list of all dependencies"))

(defmethod create-formula ((system string))
  (create-formula (asdf::find-system system)))

(defmethod create-formula ((system symbol))
  (create-formula (asdf::find-system system)))


(defgeneric get-implicit-dependencies (system-name)
  (:documentation "Some systems, like cl-unicode have implicit dependencies in their asdf methods:
https://github.com/edicl/cl-unicode/blob/8073fc5634c9d4802888ac03abf11dfe383e16fa/cl-unicode.asd#L67-L70
use this method to provide information about such dependencies.

System name is a keyword and method should return a one keyword or a list of keywords with names of systems.
Each returned system should be possible to find with ql-dist:find-system.")
  (:method ((system-name t))
    nil)
  (:method :around ((system-name t))
    (flet ((to-system-name (value)
             (string-downcase
              (etypecase value
                (keyword (symbol-name value))
                (string value)))))
      (mapcar #'to-system-name
              (alexandria:ensure-list (call-next-method)))))
  (:method ((system-name string))
    (get-implicit-dependencies (make-keyword (string-upcase system-name))))
  (:method ((system-name (eql :cl-unicode)))
    :flexi-streams))


(defmethod create-formula (system)
  (let* ((build-operation (asdf/component:component-build-operation system))
         ;; We support Buildapp or Shinmera's Deploy as a build systems.
         ;; 
         ;; TODO: Probably we also should search and build any Roswell scripts,
         ;; found in the ./roswell/ subdirectory.
         (class (cond
                  ((string-equal build-operation
                                 "deploy-op")
                   'deploy-formula)
                  (t
                   'buildapp-formula)))
         (deps (append (asdf:system-depends-on system)
                       ;; We also need to include build dependencies
                       ;; into the system.
                       (asdf:system-defsystem-depends-on system)
                       (when (eql class 'deploy-formula)
                         ;; We need this system because these hooks
                         ;; restore path to dynamic libs:
                         (list "cl-brewer/deploy/hooks"))))
         (root-primary-name (asdf:primary-system-name system))
         (existing-systems)
         (missing-systems))
    (labels ((expand-deps (deps)
               (dolist (subname deps)
                 (expand-dep subname)))
             (expand-dep (name)
               (check-type name string)
               
               (let* ((primary-name (asdf:primary-system-name name))
                      (package-inferred-subsystem-p
                        (string-equal primary-name
                                      root-primary-name))
                      (ql-system (unless package-inferred-subsystem-p
                                   (ql-dist:find-system name)))
                      (asdf-system (when package-inferred-subsystem-p
                                     (asdf:find-system name))))
                 (cond (ql-system
                        (pushnew ql-system existing-systems
                                 ;; This fixes a problem of duplicate
                                 ;; dependencies.
                                 :test #'string-equal
                                 :key #'ql-dist:name)
                        (expand-deps (ql-dist:required-systems ql-system))
                        (expand-deps (get-implicit-dependencies (ql-dist:name ql-system))))
                       ;; If this is asdf package inferred subsystem,
                       ;; then we don't need to add it as a dependency itself,
                       ;; but still need to process it's dependencies.
                       (asdf-system
                        (expand-deps (asdf:system-depends-on asdf-system))
                        (expand-deps (get-implicit-dependencies
                                      (asdf:component-name asdf-system))))
                       ;; ignore if whitelisted
                       ((find name +whitelisted-systems+ :test #'string=) nil)
                       ;; store as missing if it is not available in Quicklisp
                       (t
                        (pushnew name missing-systems
                                 :test #'string-equal))))))
      (dolist (subname deps) (expand-dep subname))
      (make-instance class
                     :root-system system
                     :missing-systems (remove-duplicates missing-systems :test #'string=)
                     :included-systems (remove-duplicates existing-systems)))))


(defun print-release (release &key (stream t))
  (check-type release ql-dist:release)
  (format stream +dependency-body+
          (ql-dist:name release)
          (ql-dist::archive-url release)
          (sha256 release)))


(defun print-formula (formula &key (stream t) entry-point preload)
  (check-type formula formula)
  (print-header formula :stream stream)
  (print-dependencies formula :stream stream)
  (print-releases formula :stream stream)
  (print-install formula
                 :stream stream
                 :entry-point entry-point
                 :preload preload)
  (print-footer formula :stream stream))


(defgeneric print-releases (formula &key stream)
  (:method ((formula formula) &key (stream t))
      (let* ((systems (included-systems formula))
             ;; We want to print only releases, not systems.
             ;; Because one release can contain multiple systems in it's archive.
             (releases (loop with result = nil
                             for system in systems
                             for release = (ql-dist:release system)
                             do (pushnew release result
                                         :key #'ql-dist:archive-url
                                         :test #'string-equal)
                                ;; Also, we want all releases to be sorted.
                                ;; This way diffs in formula will be minimal.
                             finally (return (sort result
                                                   #'string<
                                                   :key (alexandria:compose
                                                         #'string-downcase
                                                         #'ql-dist:name))))))
        (loop for release in releases
              do (print-release release :stream stream)))))


(defgeneric print-install (formula &key stream entry-point preload)
  (:documentation "Outputs \"install\" method for the formula.")
  (:method ((formula formula) &key (stream t) entry-point preload)
    (format stream
            "  def install
    resources.each do |resource|
      resource.stage buildpath/\"lib\"/resource.name
    end
")
    (print-env-vars formula :stream stream)
    (print-build-commands formula
                          :stream stream
                          :entry-point entry-point
                          :preload preload)
    (format stream "  end~%")))


(defgeneric env-vars (formula)
  (:documentation "Should return an alist with environment variables for \"install\" method of the formula.")
  (:method ((formula formula))
    (list
     (cons "CL_SOURCE_REGISTRY"
           "#{buildpath}/lib//:#{buildpath}//")
     (cons "ASDF_OUTPUT_TRANSLATIONS"
           "/:/"))))


(defgeneric print-env-vars (formula &key stream)
  (:documentation "Outputs environment variables for \"install\" method of the formula.")
  (:method :before ((formula formula) &key (stream t))
    (terpri stream))
  (:method ((formula formula) &key (stream t))
    (loop for (key . value) in (env-vars formula)
          do (format stream
                     "    ENV[\"~A\"] = \"~A\"~%"
                     key value))))


(defgeneric print-build-commands (formula &key stream entry-point preload
                                  &allow-other-keys)
  (:documentation "Outputs build commands for \"install\" method of the formula.")
  (:method ((formula formula) &key (stream t) entry-point)
    (declare (ignorable formula stream entry-point))))


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


(defgeneric print-header (formula &key stream)
  (:method ((formula formula) &key (stream t))
    (format stream +formula-header+
            (rubyize-name (name formula))
            (description formula)
            (home-page formula)
            (url formula)
            (sha256 formula)
            (repo-head formula))))


(defgeneric print-footer (formula &key stream)
  (:method ((formula formula) &key (stream t))
    (declare (ignorable formula))
    (format stream +formula-footer+)))


(defgeneric print-dependencies (formula &key stream)
  (:method :around ((formula formula) &key (stream t))
    (format stream "~2&")
    (call-next-method)
    (format stream "~%"))
  (:method ((formula formula) &key (stream t))
    (format stream "  depends_on \"sbcl\" => :build~%")))


(defmethod print-dependencies :after ((formula buildapp-formula) &key (stream t))
  (format stream "  depends_on \"buildapp\" => :build~%"))


(defun name (formula)
  (asdf:component-name (root-system formula)))

(defun description (formula)
  (asdf::component-description (root-system formula)))


(defun ensure-github-url (url formula-name)
  (cond
    ((null url)
     (error "Please set :source-control or :homepage in the ~A.asd file. It should point to the GitHub project."
            formula-name))
    ((startswith "https://github.com/" url)
     url)
    (t
     (error "Please set :source-control or :homepage in the ~A.asd file. It should point to the GitHub project. Right now it is ~S."
            formula-name
            url))))


(defun github-page (formula)
  (ensure-github-url
   (or (getf (asdf:system-source-control  (root-system formula))
             :git)
       (asdf:system-homepage (root-system formula)))
   (name formula)))


(defun url (formula)
  (let* ((system (root-system formula))
         (version (asdf:component-version system)))
    
    (unless version
      (error "Unable to determine a version of ~A. Ensure you have a :version in your system's definition."
             system))
    (concatenate 'string
                 (github-page formula)
                 "/archive/v"
                 version
                 ".tar.gz")))

(defun repo-head (formula)
  (let ((source (asdf:system-source-control (root-system formula))))
    (typecase source
      (string source)
      ;; Other VCS aren't supported yet.
      ;; Feel free to add and make a PR:
      (list (or (getf source :git)
                "")))))

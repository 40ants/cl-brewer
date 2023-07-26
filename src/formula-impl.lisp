(uiop:define-package #:cl-brewer/formula-impl
  (:use #:cl)
  (:import-from #:trivial-download
                #:download)
  (:import-from #:ironclad
                #:digest-file
                #:byte-array-to-hex-string)
  ;; We need to use some symbols from this core
  ;; package because they provide a core functionality
  (:import-from #:cl-brewer/core
                #:save-formula)
  (:import-from #:cl-brewer/formula
                #:get-additional-dependencies
                #:env-vars
                #:print-dependencies
                #:root-system
                #:print-footer
                #:print-env-vars
                #:print-build-commands
                #:missing-systems
                #:included-systems
                #:repo-head
                #:url
                #:home-page
                #:description
                #:name
                #:*formula-class-guessers*
                #:get-implicit-dependencies
                #:create-formula
                #:print-header
                #:formula)
  (:import-from #:cl-brewer/hash
                #:sha256)
  (:import-from #:cl-brewer/utils
                #:rubyize-name))
(in-package #:cl-brewer/formula-impl)


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
;; Probably we need to include all SBCL contribs here:
;; /Users/art/.roswell/impls/arm64/darwin/sbcl-bin/2.3.3/lib/sbcl/contrib/
(defparameter +whitelisted-systems+ '("asdf"
                                      ;; "sb-introspect"
                                      ))


(defun guess-formula-class (system)
  (loop for guesser in *formula-class-guessers*
          thereis (funcall guesser system)))


(defmethod create-formula ((system string))
  (create-formula (asdf::find-system system)))

(defmethod create-formula ((system symbol))
  (create-formula (asdf::find-system system)))

(defmethod create-formula ((system asdf:system))
  (let* ((class (or (guess-formula-class system)
                    (error "Unable to guess FORMULA class for system ~A."
                           system)))
         (system-deps (append (asdf:system-depends-on system)
                              ;; We also need to include build dependencies
                              ;; into the system.
                              (asdf:system-defsystem-depends-on system)))
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
      (let* ((formula
               (make-instance class
                              :root-system system)))
        (mapc #'expand-dep (append system-deps
                                   (get-additional-dependencies formula)))

        (setf (included-systems formula)
               (remove-duplicates
                existing-systems))
        (setf (missing-systems formula)
              (remove-duplicates missing-systems :test #'string=))
        (values formula)))))


(defun save-formula (formula name &key entry-point preload)
  "Saves Homebrew formula definition into the file with given NAME.

   If ENTRY-POINT argument was given, then it might be used as entry-point,
   but some formula classes like CL-BREWER/DEPLOY/FORMULA:DEPLOY-FORMULA
   might ignore this argument.

   PRELOAD argument if given, should be a list of strings with
   ASDF system names to be preloaded before cl-brewer will build a binary."
  (let* ((output-file (make-pathname :name name :type "rb")))
    (with-open-file (stream output-file
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (print-formula formula
                     :formula-name name
                     :stream stream
                     :entry-point entry-point
                     :preload preload))))


(defmethod print-object ((obj formula) stream)
  (print-unreadable-object (obj stream :type t)
    (let ((missing (length (missing-systems obj))))
      (format stream
              "~S depends on ~A system~:p~@[ but ~A of them are missing in known dists~]"
              (asdf:component-name (root-system obj))
              (length (included-systems obj))
              (when (> missing 0)
                missing)))))


(defun print-release (release &key (stream t))
  (check-type release ql-dist:release)
  (format stream +dependency-body+
          (ql-dist:name release)
          (ql-dist::archive-url release)
          (sha256 release)))


(defun print-formula (formula &key formula-name (stream t) entry-point preload)
  (check-type formula formula)
  (print-header formula
                :stream stream
                :formula-name formula-name)
  (print-dependencies formula :stream stream)
  (print-releases formula :stream stream)
  (print-install formula
                 :stream stream
                 :entry-point entry-point
                 :preload preload)
  (print-footer formula :stream stream))


(defmethod print-releases ((formula formula) &key stream)
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
          do (print-release release :stream stream))))


(defmethod print-install ((formula formula) &key stream entry-point preload)
  (format stream
          "  def install
    resources.each do |resource|
      resource.stage buildpath/\"_brew_resources\"/resource.name
    end
")
  (print-env-vars formula :stream stream)
  (print-build-commands formula
                        :stream stream
                        :entry-point entry-point
                        :preload preload)
  (format stream "  end~%"))


(defmethod env-vars ((formula formula))
  (list
   (cons "CL_SOURCE_REGISTRY"
         "#{buildpath}/:#{buildpath}/_brew_resources//")
   (cons "ASDF_OUTPUT_TRANSLATIONS"
         "/:/")))


(defmethod cl-brewer/formula::print-env-vars :before ((formula formula) &key stream)
  (terpri stream))


(defmethod cl-brewer/formula::print-env-vars ((formula formula) &key stream)
  (loop for (key . value) in (env-vars formula)
        do (format stream
                   "    ENV[\"~A\"] = \"~A\"~%"
                   key value)))


(defmethod cl-brewer/formula::print-build-commands ((formula formula) &key stream entry-point preload
                                            &allow-other-keys)
  (declare (ignorable formula stream entry-point preload)))


(defmethod cl-brewer/formula::print-header ((formula formula) &key stream formula-name)
  (format stream +formula-header+
          (rubyize-name (or formula-name
                            (name formula)))
          (description formula)
          (home-page formula)
          (url formula)
          (sha256 formula)
          (repo-head formula)))


(defmethod print-footer ((formula formula) &key stream)
  (declare (ignorable formula))
  (format stream +formula-footer+))


(defmethod cl-brewer/formula::print-dependencies :around ((formula formula) &key stream)
  (format stream "~2&")
  (call-next-method)
  (format stream "~%"))


(defmethod cl-brewer/formula::print-dependencies (formula &key stream)
  (format stream "  depends_on \"sbcl\" => :build~%"))


(defmethod sha256 ((formula formula))
  (let ((fname (make-pathname :directory '(:absolute "tmp")
                              :name (substitute #\- #\/ (name formula))))
        (url (url formula)))
    (download url fname)
    (sha256 fname)))

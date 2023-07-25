(uiop:define-package #:cl-brewer/deploy/formula
  (:use #:cl)
  (:import-from #:cl-brewer/formula
                #:get-additional-dependencies
                #:formula
                #:name
                #:print-build-commands
                #:env-vars
                #:print-dependencies
                #:define-quesser)
  (:import-from #:str
                #:replace-all)
  (:export #:deploy-formula))
(in-package #:cl-brewer/deploy/formula)


(defclass deploy-formula (formula)
  ()
  (:documentation "This formula class uses [Deploy](https://shinmera.github.io/deploy/) to build a binary.

                   The core difference from CL-BREWER/BUILDAPP/FORMULA:BUILDAPP-FORMULA is that
                   this type of formula also builds and distributes all necessary dynamic libraries."))


(define-quesser guess-deploy-formula (system)
  ;; We support Buildapp or Shinmera's Deploy as a build systems.
  ;; 
  ;; TODO: Probably we also should search and build any Roswell scripts,
  ;; found in the ./roswell/ subdirectory.
  (let* ((build-operation (asdf/component:component-build-operation system)))
    (when (string-equal build-operation
                        "deploy-op")
      'deploy-formula)))


(defun extract-formula-name-from (path)
  (check-type path pathname)
  (let* ((components (pathname-directory path))
         (cellar-idx (position "Cellar" components
                               :test #'string-equal)))
    (when (and cellar-idx
               (< (1+ cellar-idx)
                  (length components)))
      (elt components
           (1+ cellar-idx)))))


(defun get-brew-formulas-which-provide-dynlibs ()
  (loop with results = (make-hash-table :test 'equal)
        for lib in (deploy:list-libraries)
        for path = (deploy:library-path lib)
        for lib-name = (deploy:library-name lib)
        for formula-name = (when path
                             (extract-formula-name-from path))
        when formula-name
          do (push lib-name
                   (gethash formula-name results))
        finally (return (sort (alexandria:hash-table-alist results)
                              #'string<
                              :key #'car))))


(defmethod get-additional-dependencies ((formula deploy-formula))
  (list* "cl-brewer-deploy-hooks"
         (call-next-method)))


(defmethod print-dependencies :after ((formula deploy-formula) &key (stream t))
  (loop for (formula . libs) in (get-brew-formulas-which-provide-dynlibs)
        do (format stream "  # required by: ~{~S~#[~; and ~:;, ~]~}~%"
                   libs)
           (format stream "  depends_on \"~A\"~%"
                   formula)))


(defmethod env-vars ((formula deploy-formula))
  ;; We need to specify this env var to pass
  ;; the path to the final folder where all *.dylibs
  ;; will be installed. Code from src/deploy/hooks.lisp
  ;; will restore this path in CFFI on boot and CFFI will
  ;; be able to find these libraries even if they are
  ;; not symlinked to /opt/homebrew/lib
  (list* (cons "LIBEXEC_PATH"
               "#{libexec}/")
         (call-next-method)))


(defmethod print-build-commands ((formula deploy-formula)
                                 &key (stream t) entry-point preload
                                 &allow-other-keys)
  (declare (ignorable entry-point))
  (let ((evals (list
                ;; These hooks will store and restore the path
                ;; to dynamic libs in Homebrew's Cellar:
                "(asdf:load-system :cl-brewer-deploy-hooks)"
                ;; Now we need to turn off Deploy's debug messages:
                "(push :deploy-console *features*)"
                "(require :asdf)")))
    (format stream
            "
    system \"sbcl\"")
  
    (dolist (item (alexandria:ensure-list preload))
      (push (format nil "(handler-case (asdf:load-system :~A) (error (e) (format *error-output* \"~~A~~%\" e) (uiop:quit 1)))"
                    item)
            evals))

    (push (format nil "(handler-case (asdf:make :~A) (error (e) (format *error-output* \"~~A~~%\" e) (uiop:quit 1)))"
                  (name formula))
          evals)

    (loop for eval in (reverse evals)
          do (format stream ", \"--eval\", \"~A\""
                     (replace-all "\"" "\\\"" eval)))

    ;; Deploy puts all dynamic libraries into the bin directory,
    ;; but if we leave them there, Homebrew will symlink these dylibs into /opt/homebrew/bin.
    ;; To prevent this, we'll move all *.dylib files into the libexec folder from there they will not
    ;; be symlinked to the /opt/homebrew/bin nor /opt/homebrew/lib
    (format stream "

    system \"bash\", \"-c\", \"mkdir dyn-libs && find bin/ -name '*.dylib' -exec mv '{}' dyn-libs/ \\\\;\"
")
  
    (format stream
            "
    bin.install Dir[\"bin/*\"]
    libexec.install Dir[\"dyn-libs/*\"]
")))


(uiop:define-package #:cl-brewer/formula
  (:use #:cl)
  (:import-from #:alexandria
                #:ensure-list
                #:make-keyword)
  (:import-from #:cl-brewer/utils
                #:startswith)
  ;; We need to use some symbols from this core
  ;; package because they provide a core functionality
  (:import-from #:cl-brewer/core
                #:formula
                #:create-formula
                #:get-implicit-dependencies)
  (:export #:define-quesser
           #:included-systems
           #:missing-systems
           #:root-system
           #:get-additional-dependencies))
(in-package #:cl-brewer/formula)


(defvar *formula-class-guessers* nil
  "A list of functions accepting one argument - ASDF system and returning NIL or symbol denoting a class-name of a formula to be created.

   Use DEFINE-GUESSER macro to add items to this list.")


(defclass formula ()
  ((root-system
    :initarg :root-system
    :accessor root-system)
   (missing-systems
    :initarg :missing-systems
    :accessor missing-systems)
   (included-systems
    :initarg :included-systems
    :accessor included-systems))
  (:documentation "Base class for Homebrew formula definition."))


(defgeneric create-formula (system)
  (:documentation "Create <formula> object based on asdf:system with a list of all dependencies"))


(defgeneric get-implicit-dependencies (system-name)
  (:documentation "Some systems, like cl-unicode have implicit dependencies in their asdf methods:
<https://github.com/edicl/cl-unicode/blob/8073fc5634c9d4802888ac03abf11dfe383e16fa/cl-unicode.asd#L67-L70>
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
              (ensure-list (call-next-method)))))
  (:method ((system-name string))
    (get-implicit-dependencies (make-keyword (string-upcase system-name))))
  (:method ((system-name (eql :cl-unicode)))
    :flexi-streams))


(defgeneric get-additional-dependencies (formula)
  (:documentation "Some formulas might add dependencies needed to build a binary. For example, Deploy formula adds cl-brewer because it uses it's cl-brewer/deploy/hooks subsystem.")
  (:method ((formula t))
    nil)
  (:method :around ((formula t))
    (flet ((to-system-name (value)
             (string-downcase
              (etypecase value
                (keyword (symbol-name value))
                (string value)))))
      (mapcar #'to-system-name
              (ensure-list (call-next-method))))))


(defgeneric print-releases (formula &key stream))


(defgeneric print-install (formula &key stream entry-point preload)
  (:documentation "Outputs \"install\" method for the formula."))


(defgeneric env-vars (formula)
  (:documentation "Should return an alist with environment variables for \"install\" method of the formula."))


(defgeneric print-env-vars (formula &key stream)
  (:documentation "Outputs environment variables for \"install\" method of the formula."))


(defgeneric print-build-commands (formula &key stream entry-point preload
                                  &allow-other-keys)
  (:documentation "Outputs build commands for \"install\" method of the formula."))


(defgeneric print-header (formula &key stream formula-name))


(defgeneric print-footer (formula &key stream))


(defgeneric print-dependencies (formula &key stream))


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


(defun home-page (formula)
  (or (asdf:system-homepage (root-system formula))
      ""))


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


(defmacro define-quesser (name (asdf-system) &body body)
  "Use this macro to define a function to guess a formula class.

   The function should accept a one argument - an ASDF system and
   return a symbol denoting a class derived from FORMULA class.

   If guesser does not know how to create a formula for the system,
   then it should return a NIL value."
  `(progn
     (defun ,name (,asdf-system)
       ,@body)
     (pushnew ',name
              *formula-class-guessers*)))

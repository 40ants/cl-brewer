(defsystem cl-brewer
  :description "Homebrew formula builder for common lisp applications"
  :author "Dmitry Petrov <dpetroff@gmail.com>"
  :version "0.8.2"
  :license "Public Domain"
  :homepage "https://github.com/40ants/cl-brewer"
  :source-control (:git "https://github.com/40ants/cl-brewer")

  :defsystem-depends-on ("deploy")
  :build-operation "deploy-op"
  :build-pathname "cl-brewer"
  :entry-point "cl-brewer::asdf-main"
  
  :depends-on ("cl-plus-ssl-osx-fix"
               "quicklisp"
               "alexandria"
               "ironclad"
               "command-line-arguments"
               "trivial-download")
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "utils")
                 (:file "formula")
                 (:file "main"))))
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.md"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op cl-brewer-test))))



(defsystem "cl-brewer/deploy"
  :description "Hooks for making Shinmera's Deploy work with Homebrew."
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :version "0.8.2"
  :license "Public Domain"
  :homepage "https://github.com/40ants/cl-brewer"
  :source-control (:git "https://github.com/40ants/cl-brewer")
  
  :depends-on ("deploy"
               "cffi")
  :components ((:module "src"
                :components
                ((:file "deploy-hooks")))))

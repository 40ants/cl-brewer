#-asdf3.1 (error "cl-brewer requires ASDF 3.1 because for lower versions pathname does not work for package-inferred systems.")
(defsystem "cl-brewer"
  :description "Homebrew formula builder for Common Lisp applications."
  :author "Dmitry Petrov <dpetroff@gmail.com>"
  :maintainer "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Public Domain"
  :homepage "https://40ants.com/cl-brewer/"
  :source-control (:git "https://github.com/40ants/cl-brewer")
  :bug-tracker "https://github.com/40ants/cl-brewer/issues"
  :class :40ants-asdf-system
  :defsystem-depends-on ("40ants-asdf-system"
                         "deploy")
  :pathname "src"
  :depends-on ("cl-plus-ssl-osx-fix"
               "quicklisp"
               "cl-brewer/buildapp/formula"
               "cl-brewer/deploy/formula"
               "cl-brewer/deploy/hooks"
               "cl-brewer/main")

  :build-operation "deploy-op"
  :build-pathname "cl-brewer"
  :entry-point "cl-brewer/main::asdf-main"

  :in-order-to ((test-op (test-op "cl-brewer-tests"))))

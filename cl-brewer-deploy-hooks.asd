(defsystem "cl-brewer-deploy-hooks"
  :description "A small helper to use Deploy for Homebrew packaging."
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Public Domain"
  :homepage "https://40ants.com/cl-brewer/"
  :source-control (:git "https://github.com/40ants/cl-brewer")
  :bug-tracker "https://github.com/40ants/cl-brewer/issues"
  :pathname "src"
  :depends-on ("deploy")
  :components ((:module "deploy"
                :components ((:file "hooks")))))

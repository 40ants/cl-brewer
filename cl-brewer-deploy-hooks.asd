(defsystem "cl-brewer-deploy-hooks"
  :description "A small helper to use Deploy for Homebrew packaging."
  :pathname "src"
  :depends-on ("deploy")
  :components ((:module "deploy"
                :components ((:file "hooks")))))

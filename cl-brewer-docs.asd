(defsystem "cl-brewer-docs"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/cl-brewer/"
  :class :package-inferred-system
  :description "Provides documentation for cl-brewer."
  :source-control (:git "https://github.com/40ants/cl-brewer")
  :bug-tracker "https://github.com/40ants/cl-brewer/issues"
  :pathname "docs"
  :depends-on ("cl-brewer"
               "cl-brewer-docs/index"))

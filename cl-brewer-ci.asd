(defsystem "cl-brewer-ci"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/cl-brewer/"
  :class :package-inferred-system
  :description "Provides CI settings for cl-brewer."
  :source-control (:git "https://github.com/40ants/cl-brewer")
  :bug-tracker "https://github.com/40ants/cl-brewer/issues"
  :pathname "src"
  :depends-on ("40ants-ci"
               "cl-brewer-ci/ci"))

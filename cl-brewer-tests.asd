(defsystem "cl-brewer-tests"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/cl-brewer/"
  :class :package-inferred-system
  :description "Provides tests for cl-brewer."
  :source-control (:git "https://github.com/40ants/cl-brewer")
  :bug-tracker "https://github.com/40ants/cl-brewer/issues"
  :pathname "t"
  :depends-on ("cl-brewer-tests/core")
  :perform (test-op (op c)
                    (unless (symbol-call :rove :run c)
                      (error "Tests failed"))))

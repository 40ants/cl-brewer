(uiop:define-package #:cl-brewer-docs/changelog
  (:use #:cl)
  (:import-from #:40ants-doc/changelog
                #:defchangelog))
(in-package #:cl-brewer-docs/changelog)


(defchangelog (:ignore-words ("SLY"
                              "ASDF"
                              "REPL"
                              "52M"
                              "13M"
                              "HTTP"))
  (0.9.1 2023-07-14
         "* Fixed an error happened in case if program does not have any dynamic libs to install into the Cellar.")
  (0.9.0 2023-07-14
         "Now when using Deploy, dynamic libraries copied into the libexec
          folder inside Cellar and not symlinked into the /ope/homebrew/bin folder.

          Also, dynamic libraries provided by other Homebrew formulas are not
          copied into the Cellar, instead these formulas are listed as a dependency
          of the formula, created by cl-homebrew.
          ")
  (0.8.0 2019-09-12
         "* Now cl-brewer suppress debug output of the Deploy's startup process.
          * Added a `--version` command line option.
          * Fixed creation of dist, local-projects and tmp in the current directory.
")
  (0.7.0 2019-09-11
         "* Added a generic-function CL-BREWER:GET-IMPLICIT-DEPENDENCIES which can be used to
            specify additional dependencies for systems when asdf is not possible to figure them out.

            It can be used like that:
  
            ```
            (defmethod cl-brewer:get-implicit-dependencies ((system-name (eql :cl-unicode)))
                :flexi-streams)
            ```

            It can return a single keyword or a list of keywords.")
  (0.6.0 2019-07-30
         "* Now cl-brewer can be installed from the Homebrew tap as a binary.")
  (0.5.6 2019-07-22
         "* Fixed arguments list in buildapp's entrypoint.")
  (0.5.5 2019-07-22
         "* Fixed a bug in preloading code on buildapp.")
  (0.5.4 2019-07-22
         "* Added support for `--preload` option.
            It allows to embedd Quicklisp client when building a binary for a homebrew.")
  (0.5.3 2019-07-22
         "* More diagnostics to understand why does not work a binary built by homebrew.")
  (0.5.2 2019-07-22
         "* Added an entry point for buildapp.")
  (0.5.1 2019-07-21
         "* Kickstarting quicklisp client when building cl-brewer.")
  (0.5.0 2019-07-21
         "* Now cl-brewer can work with package-inferred asdf systems.
          * Previously, some archives were included more than once because
            they contains multiple systems. Now this is fixed.
          * Added option `--compress-core`. It reduces size of a simple
            \"hello world\" from 52M to 13M.
          * Function `cl-brewer::create-formula` was made external and now
            can accept system name as a symbol.
          * Function `cl-brewer::save-formula` was made external.
          * ASDF option `defsystem-depends-on` was supported.")
  (0.4.0 2016-12-17
         "* Provide a way to skip some systems if not found by quicklisp. Right now it's only sb-introspect")
  (0.3.0 2016-11-10
         "* A bit of doc strings.
          * Cli interface.
          * Code was split into multiple files.")
  (0.2.0 2016-11-04
         "Initial working functionality."))

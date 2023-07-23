(uiop:define-package #:cl-brewer-docs/index
  (:use #:cl)
  (:import-from #:pythonic-string-reader
                #:pythonic-string-syntax)
  #+quicklisp
  (:import-from #:quicklisp)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:40ants-doc
                #:defsection
                #:defsection-copy)
  (:import-from #:cl-brewer-docs/changelog
                #:@changelog)
  (:import-from #:docs-config
                #:docs-config)
  (:import-from #:40ants-doc/autodoc
                #:defautodoc)
  (:export #:@index
           #:@readme
           #:@changelog))
(in-package #:cl-brewer-docs/index)

(in-readtable pythonic-string-syntax)


(defmethod docs-config ((system (eql (asdf:find-system "cl-brewer-docs"))))
  ;; 40ANTS-DOC-THEME-40ANTS system will bring
  ;; as dependency a full 40ANTS-DOC but we don't want
  ;; unnecessary dependencies here:
  #+quicklisp
  (ql:quickload "40ants-doc-theme-40ants")
  #-quicklisp
  (asdf:load-system "40ants-doc-theme-40ants")
  
  (list :theme
        (find-symbol "40ANTS-THEME"
                     (find-package "40ANTS-DOC-THEME-40ANTS")))
  )


(defsection @index (:title "cl-brewer - Homebrew formula builder for Common Lisp applications."
                    :ignore-words ("JSON"
                                   "HTTP"
                                   "TODO"
                                   "Unlicense"
                                   "REPL"
                                   "ASDF:PACKAGE-INFERRED-SYSTEM"
                                   "ASDF"
                                   "40A"
                                   "API"
                                   "MIT"
                                   "CI"
                                   "Buildapp"
                                   "GUI"
                                   "SBCL"
                                   "OS"
                                   "URL"
                                   "URI"
                                   "RPC"
                                   "L67-L70"
                                   "GIT"))
  (cl-brewer system)
  "
[![](https://github-actions.40ants.com/40ants/cl-brewer/matrix.svg?only=ci.run-tests)](https://github.com/40ants/cl-brewer/actions)

![Quicklisp](http://quickdocs.org/badge/cl-brewer.svg)
"
  (@about section)
  (@installation section)
  (@usage section)
  (@building-cl-brewer section)
  (@local-install section)
  (@contribute section)
  (@license section)
  (@api section))


(defsection-copy @readme @index)


(defsection @about (:title "About"
                    :external-links (("buildapp" . "https://xach.com/lisp/buildapp/")
                                     ("deploy" . "https://shinmera.github.io/deploy/")))
  "
Currently there is now easy way to distribute common lisp applications.
One promising way is to use [roswell](https://github.com/roswell/roswell) project
however it might be to complicated for the users how just want to install
an application and are not really interested in having one more package manager
for that.

In Mac OS X world the most popular solution is to use brew package manager and all
we need is to be able to generate formula that will handle installation proccess.
Since homebrew guidelines are not really fond of using third-party managers to
get dependencies, we need to generate list manually and feed it to brew.

cl-brewer uses SBCL and targets command line applications written in Common Lisp.
GUI applications haven't been tested and might require additional changes.

This application is based on the awesome [quicklisp-homebrew-roundup](https://github.com/benesch/quicklisp-homebrew-roundup)
but has a purpose to make a process simplier by:

* Allowing to generate file for any system available not just quicklisp package (dependencies should be on quicklisp though)
* Allowing to generate a complete formula, not just dependencies
* Having a cli interface that can be used in CI services to generate formulas automatically.

At the moment several assumptions were made:

* We can use [Buildapp][Buildapp] or [Deploy][deploy] make executable.
* Formula is generated for the system available for ASDF.
* All dependencies should be available via quicklisp.
* System source code should live on github all releases should be tagged with vX.Y.Z scheme.
* System should have description, version, homepage fields defined. Project at GitHub should have a tag corresponding to the
  current system version. GitHub build a tar.gz archive with sources of the tagged revision and this way a formula,
  created by cl-brewer, will be able to fetch sources, corresponding to the version.
* [Buildapp][buildapp] calls `main` function. By default `main` function is searched in the
  package with the same name as system name but can be overridden with option.
* When using [Deploy][deploy], you don't have to define an entry point. Also, Deploy is able to
  create formulas for applications which build some dynamic libraries.

Here is an [example formula](https://github.com/can3p/homebrew-cl-journal/blob/master/cl-journal.rb)
")


(defsection @installation (:title "Installation")
  """
```
brew tap 40ants/soft
brew install cl-brewer
```

Or you can install it using Roswell:

```
# install roswell and sbcl before
$ ros install 40ants/cl-brewer
```

If you want to install it to use from the REPL, then you can install this library from Quicklisp,
but you want to receive updates quickly, then install it from Ultralisp.org:

```
(ql-dist:install-dist "http://dist.ultralisp.org/"
                      :prompt nil)
(ql:quickload :cl-brewer)
```
""")


(defsection @usage (:title "Usage")
  "
Just run:

```
cl-brewer <your-system-name>
```

This will emit `<your-system-name>.rb` file in current folder.
")


(defsection @building-cl-brewer (:title "Building a formula for cl-brewer")
  "
To kickstart a cl-brewer and to create a formula for itself, load it in the REPL and do like that:

```
CL-USER> (cl-brewer:create-formula :cl-brewer)
#<BUILDAPP-FORMULA \"cl-brewer\" depends on 23 systems>

CL-USER> (cl-brewer:save-formula * \"cl-brewer\"
                                 :preload (list \"quicklisp-starter\"))
Downloading \"https://github.com/40ants/cl-brewer/archive/v0.5.5.tar.gz\" (Unknown size)
NIL
```

However, in most cases you can just install cl-brewer from the Homebrew. In this case,
you can update `cl-brewer's` formula with this command:

```
qlot exec cl-brewer \
          --preload quicklisp-starter \
          cl-brewer
```
")


(defsection @local-install (:title "Installing From Local Formula")
  "
How to install cl-brewer (or any other project) from a local formula?

Replace url line in a formula:

```
url \"https://github.com/40ants/cl-brewer/archive/v0.5.6.tar.gz\"
```

with two lines like this:

```
url File.dirname(__FILE__), :using => :git
version \"0.5.6-rc1\"
```
    
Next, do this in the shell:

```
HOMEBREW_NO_AUTO_UPDATE=1 brew install --debug --verbose ./*.rb
```

it should build and install `cl-brewer`.
")


(defsection @contribute (:title "Contribute")
  "
If you are interested in using this project but your application has different requirements,
please open an issue or make a pull request. Contributions are welcome!
")


(defsection @license (:title "License")
  "
All code is public domain except parts that were taken from [quicklisp-homebrew-roundup](https://github.com/benesch/quicklisp-homebrew-roundup) which is under MIT License.
")




(defautodoc @api (:system "cl-brewer"))

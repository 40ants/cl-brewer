# Homebrew formula builder for (command line) common lisp applications

Currently there is now easy way to distribute common lisp applications.
One promising way is to use [roswell](https://github.com/roswell/roswell) project
however it might be to complicated for the users how just want to install
an application and are not really interested in having one more package manager
for that.

In Mac OS X world the most popular solution is to use brew package manager and all
we need is to be able to generate formula that will handle installation proccess.
Since homebrew guidelines are not really fond of using third-party managers to
get dependencies, we need to generate list manually and feed it to brew.

cl-brewer uses sbcl (default for buildapp) and targets command line applications
written in common lisp. GUI applications haven't been tested and might require additional
changes.

This application is based on the awesome [quicklisp-homebrew-roundup](https://github.com/benesch/quicklisp-homebrew-roundup)
but has a purpose to make a process simplier by:

* Allowing to generate file for any system available not just quicklisp package (dependencies should be on quicklisp though)
* Allowing to generate a complete formula, not just dependencies
* Having a cli interface that can be used in CI services to generate formulas automatically.

At the moment several assumptions were made:

* We can use `buildapp to make executable`
* Formula is generated for the system available for asdf
* All dependencies should be available via quicklisp
* System source code should live on github all releases should be tagged with vX.Y.Z scheme
* System should have description, version, homepage fields defined
* Buildapp calls main function. By default namespace is default to system name but can be overridden with option

Here is an [example formula](https://github.com/can3p/homebrew-cl-journal/blob/master/cl-journal.rb)

## Install

```
brew tap svetlyak40wt/cl-brewer https://github.com/svetlyak40wt/cl-brewer
```

Or you can install it using Roswell:

```
# install roswell and sbcl before
$ ros install can3p/cl-brewer
```


## Use

```
$ cl-brewer <your-system-name> # will emit <your-system-name>.rb file in current folder
```

## Building a formula for cl-brewer

To kickstart a cl-brewer and to create a formula for itself, load it in the REPL and do like that:

```
CL-USER> (cl-brewer:create-formula :cl-brewer)
#<BUILDAPP-FORMULA "cl-brewer" depends on 23 systems>

CL-USER> (cl-brewer:save-formula * "cl-brewer"
                                 :entry-point "cl-brewer::buildapp-main"
                                 :preload (list "quicklisp-starter"))

CL-USER> (cl-brewer:save-formula * "cl-brewer"
                                 :entry-point "cl-brewer::buildapp-main"
                                 :preload (list "quicklisp-starter"))
Downloading "https://github.com/svetlyak40wt/cl-brewer/archive/v0.5.5.tar.gz" (Unknown size)
NIL
```

However, in most cases you can just install cl-brewer from the Homebrew.

## Contribute

If you are interested in using this project but your application has different requirements,
please open an issue or make a pull request. Contributions are welcome!

## License

All code is public domain except parts that were taken from quicklisp-homebrew-roundup which is under MIT License

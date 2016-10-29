# Homebrew formula builder for common lisp applications

Currently there is now easy way to distribute common lisp applications.
One promising way is to use [https://github.com/roswell/roswell](roswell) project
however it might be to complicated for the users how just want to install
an application and are not really interested in having one more package manager
for that.

In Mac OS X world the most popular solution is to use brew package manager and all
we need is to be able to generate formula that will handle installation proccess.
Since homebrew guidelines are not really fond of using third-party managers to
get dependencies, we need to generate list manually and feed it to brew.

This application is based on the awesome [https://github.com/benesch/quicklisp-homebrew-roundup](quicklisp-homebrew-roundup)
but has a purpose to make a process simplier by:

* Allowing to generate file for any system available not just quicklisp package (dependencies should be on quicklisp though)
* Allowing to generate a complete formula, not just dependencies
* Having a cli interface that can be used in CI services to generate formulas automatically.

At the moment there two assumptions:

* We can use `buildapp to make executable`
* Buildapp calls main function. By default namespace is default to system name but can be overridden with option

## Install

```
# install roswell and sbcl before
$ ros install can3p/cl-brewer
```


## Use

```
$ cl-brewer <your-system-name> # will emit <your-system-name>.rb file in current folder
```

## License

All code is public domain except parts that were taken from quicklisp-homebrew-roundup which is under MIT License

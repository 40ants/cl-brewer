# Changelog

## 0.5.2

* Added an entry point for buildapp.

## 0.5.1

* Kickstarting quicklisp cliend when building cl-brewer.

## 0.5.0

* Now cl-brewer can work with package-inferred asdf systems.
* Previously, some archives were included more than once because
  they contains multiple systems. Now this is fixed.
* Added option `--compress-core`. It reduces size of a simple
  "hello world" from 52M to 13M.
* Function `cl-brewer::create-formula` was made external and now
  can accept system name as a symbol.
* Function `cl-brewer::save-formula` was made external.
* ASDF option `defsystem-depends-on` was supported.

## 0.4.0

* 2016-12-17 Provide a way to skip some systems if not found by quicklisp. Right now it's only sb-introspect

## 0.3.0

* 2016-11-10 A bit of doc strings
* 2016-11-08 Cli interface
* 2016-11-08 Code was split into multiple files

## 0.2.0

* 2016-11-04 Initial working functionality

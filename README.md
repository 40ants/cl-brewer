<a id="x-28CL-BREWER-DOCS-2FINDEX-3A-40README-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

# cl-brewer - Homebrew formula builder for Common Lisp applications.

<a id="cl-brewer-asdf-system-details"></a>

## CL-BREWER ASDF System Details

* Version: 0.10.0

* Description: Homebrew formula builder for Common Lisp applications.

* Licence: Public Domain

* Author: Dmitry Petrov <dpetroff@gmail.com>

* Maintainer: Alexander Artemenko <svetlyak.40wt@gmail.com>

* Homepage: [https://40ants.com/cl-brewer/][1887]

* Bug tracker: [https://github.com/40ants/cl-brewer/issues][557c]

* Source control: [GIT][df7d]

* Depends on: [alexandria][8236], [cl-plus-ssl-osx-fix][0ce0], [command-line-arguments][bdb0], [ironclad][90b9], [quicklisp][9c78], [trivial-download][923b]

[![](https://github-actions.40ants.com/40ants/cl-brewer/matrix.svg?only=ci.run-tests)][a54e]

![](http://quickdocs.org/badge/cl-brewer.svg)

<a id="x-28CL-BREWER-DOCS-2FINDEX-3A-3A-40ABOUT-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## About

Currently there is now easy way to distribute common lisp applications.
One promising way is to use [roswell][795a] project
however it might be to complicated for the users how just want to install
an application and are not really interested in having one more package manager
for that.

In Mac `OS` X world the most popular solution is to use brew package manager and all
we need is to be able to generate formula that will handle installation proccess.
Since homebrew guidelines are not really fond of using third-party managers to
get dependencies, we need to generate list manually and feed it to brew.

cl-brewer uses `SBCL` and targets command line applications written in Common Lisp.
`GUI` applications haven't been tested and might require additional changes.

This application is based on the awesome [quicklisp-homebrew-roundup][f9c6]
but has a purpose to make a process simplier by:

* Allowing to generate file for any system available not just quicklisp package (dependencies should be on quicklisp though)

* Allowing to generate a complete formula, not just dependencies

* Having a cli interface that can be used in `CI` services to generate formulas automatically.

At the moment several assumptions were made:

* We can use [Buildapp][ebe7] or [Deploy][eaea] make executable.

* Formula is generated for the system available for `ASDF`.

* All dependencies should be available via quicklisp.

* System source code should live on github all releases should be tagged with vX.Y.Z scheme.

* System should have description, version, homepage fields defined. Project at GitHub should have a tag corresponding to the
  current system version. GitHub build a tar.gz archive with sources of the tagged revision and this way a formula,
  created by cl-brewer, will be able to fetch sources, corresponding to the version.

* [Buildapp][ebe7] calls `main` function. By default `main` function is searched in the
  package with the same name as system name but can be overridden with option.

* When using [Deploy][eaea], you don't have to define an entry point. Also, Deploy is able to
  create formulas for applications which build some dynamic libraries.

Here is an [example formula][cccc]

<a id="x-28CL-BREWER-DOCS-2FINDEX-3A-3A-40INSTALLATION-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Installation

```
brew tap 40ants/soft
brew install cl-brewer
```
Or you can install it using Roswell:

```
# install roswell and sbcl before
$ ros install 40ants/cl-brewer
```
If you want to install it to use from the `REPL`, then you can install this library from Quicklisp,
but you want to receive updates quickly, then install it from Ultralisp.org:

```
(ql-dist:install-dist "http://dist.ultralisp.org/"
                      :prompt nil)
(ql:quickload :cl-brewer)
```
<a id="x-28CL-BREWER-DOCS-2FINDEX-3A-3A-40USAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Usage

Just run:

```
cl-brewer <your-system-name>
```
This will emit `<your-system-name>.rb` file in current folder.

<a id="x-28CL-BREWER-DOCS-2FINDEX-3A-3A-40BUILDING-CL-BREWER-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Building a formula for cl-brewer

To kickstart a cl-brewer and to create a formula for itself, load it in the `REPL` and do like that:

```
CL-USER> (cl-brewer:create-formula :cl-brewer)
#<BUILDAPP-FORMULA "cl-brewer" depends on 23 systems>

CL-USER> (cl-brewer:save-formula * "cl-brewer"
                                 :preload (list "quicklisp-starter"))
Downloading "https://github.com/40ants/cl-brewer/archive/v0.5.5.tar.gz" (Unknown size)
NIL
```
However, in most cases you can just install cl-brewer from the Homebrew. In this case,
you can update `cl-brewer's` formula with this command:

```
qlot exec cl-brewer 
          --preload quicklisp-starter 
          cl-brewer
```
<a id="x-28CL-BREWER-DOCS-2FINDEX-3A-3A-40LOCAL-INSTALL-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Installing From Local Formula

How to install cl-brewer (or any other project) from a local formula?

Replace url line in a formula:

```
url "https://github.com/40ants/cl-brewer/archive/v0.5.6.tar.gz"
```
with two lines like this:

```
url File.dirname(__FILE__), :using => :git
version "0.5.6-rc1"
```
Next, do this in the shell:

```
HOMEBREW_NO_AUTO_UPDATE=1 brew install --debug --verbose ./*.rb
```
it should build and install `cl-brewer`.

<a id="x-28CL-BREWER-DOCS-2FINDEX-3A-3A-40CONTRIBUTE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Contribute

If you are interested in using this project but your application has different requirements,
please open an issue or make a pull request. Contributions are welcome!

<a id="x-28CL-BREWER-DOCS-2FINDEX-3A-3A-40LICENSE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## License

All code is public domain except parts that were taken from [quicklisp-homebrew-roundup][f9c6] which is under `MIT` License.

<a id="x-28CL-BREWER-DOCS-2FINDEX-3A-3A-40API-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## API

<a id="x-28CL-BREWER-DOCS-2FINDEX-3A-3A-40CL-BREWER-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### CL-BREWER

<a id="x-28-23A-28-289-29-20BASE-CHAR-20-2E-20-22CL-BREWER-22-29-20PACKAGE-29"></a>

#### [package](34a5) `cl-brewer`

<a id="x-28CL-BREWER-DOCS-2FINDEX-3A-3A-7C-40CL-BREWER-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28CL-BREWER-DOCS-2FINDEX-3A-3A-40CL-BREWER-24FORMULA-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### FORMULA

<a id="x-28CL-BREWER-3AFORMULA-20CLASS-29"></a>

###### [class](05e1) `cl-brewer:formula` ()

Base class for Homebrew formula definition.

**Readers**

<a id="x-28CL-BREWER-2FFORMULA-3AINCLUDED-SYSTEMS-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-BREWER-3AFORMULA-29-29"></a>

###### [reader](560c) `cl-brewer/formula:included-systems` (formula) (:included-systems)

<a id="x-28CL-BREWER-2FFORMULA-3AMISSING-SYSTEMS-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-BREWER-3AFORMULA-29-29"></a>

###### [reader](b0d4) `cl-brewer/formula:missing-systems` (formula) (:missing-systems)

<a id="x-28CL-BREWER-2FFORMULA-3AROOT-SYSTEM-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20CL-BREWER-3AFORMULA-29-29"></a>

###### [reader](0e6e) `cl-brewer/formula:root-system` (formula) (:root-system)

**Accessors**

<a id="x-28CL-BREWER-2FFORMULA-3AINCLUDED-SYSTEMS-20-2840ANTS-DOC-2FLOCATIVES-3AACCESSOR-20CL-BREWER-3AFORMULA-29-29"></a>

###### [accessor](560c) `cl-brewer/formula:included-systems` (formula) (:included-systems)

<a id="x-28CL-BREWER-2FFORMULA-3AMISSING-SYSTEMS-20-2840ANTS-DOC-2FLOCATIVES-3AACCESSOR-20CL-BREWER-3AFORMULA-29-29"></a>

###### [accessor](b0d4) `cl-brewer/formula:missing-systems` (formula) (:missing-systems)

<a id="x-28CL-BREWER-2FFORMULA-3AROOT-SYSTEM-20-2840ANTS-DOC-2FLOCATIVES-3AACCESSOR-20CL-BREWER-3AFORMULA-29-29"></a>

###### [accessor](0e6e) `cl-brewer/formula:root-system` (formula) (:root-system)

<a id="x-28CL-BREWER-DOCS-2FINDEX-3A-3A-7C-40CL-BREWER-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Generics

<a id="x-28CL-BREWER-3ACREATE-FORMULA-20GENERIC-FUNCTION-29"></a>

##### [generic-function](d4c3) `cl-brewer:create-formula` system

Create <formula> object based on asdf:system with a list of all dependencies

<a id="x-28CL-BREWER-3AGET-IMPLICIT-DEPENDENCIES-20GENERIC-FUNCTION-29"></a>

##### [generic-function](97c6) `cl-brewer:get-implicit-dependencies` system-name

Some systems, like cl-unicode have implicit dependencies in their asdf methods:
[https://github.com/edicl/cl-unicode/blob/8073fc5634c9d4802888ac03abf11dfe383e16fa/cl-unicode.asd#L67-L70][44a6]
use this method to provide information about such dependencies.

System name is a keyword and method should return a one keyword or a list of keywords with names of systems.
Each returned system should be possible to find with ql-dist:find-system.

<a id="x-28CL-BREWER-DOCS-2FINDEX-3A-3A-7C-40CL-BREWER-3FFunctions-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Functions

<a id="x-28CL-BREWER-3ASAVE-FORMULA-20FUNCTION-29"></a>

##### [function](d55a) `cl-brewer:save-formula` formula name &key entry-point preload

Saves Homebrew formula definition into the file with given `NAME`.

If `ENTRY-POINT` argument was given, then it might be used as entry-point,
but some formula classes like [`cl-brewer/deploy/formula:deploy-formula`][de5a]
might ignore this argument.

`PRELOAD` argument if given, should be a list of strings with
`ASDF` system names to be preloaded before cl-brewer will build a binary.

<a id="x-28CL-BREWER-DOCS-2FINDEX-3A-3A-40CL-BREWER-2FBUILDAPP-2FFORMULA-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### CL-BREWER/BUILDAPP/FORMULA

<a id="x-28-23A-28-2826-29-20BASE-CHAR-20-2E-20-22CL-BREWER-2FBUILDAPP-2FFORMULA-22-29-20PACKAGE-29"></a>

#### [package](bcf0) `cl-brewer/buildapp/formula`

<a id="x-28CL-BREWER-DOCS-2FINDEX-3A-3A-7C-40CL-BREWER-2FBUILDAPP-2FFORMULA-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28CL-BREWER-DOCS-2FINDEX-3A-3A-40CL-BREWER-2FBUILDAPP-2FFORMULA-24BUILDAPP-FORMULA-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### BUILDAPP-FORMULA

<a id="x-28CL-BREWER-2FBUILDAPP-2FFORMULA-3ABUILDAPP-FORMULA-20CLASS-29"></a>

###### [class](4b86) `cl-brewer/buildapp/formula:buildapp-formula` (formula)

This formula class uses [Buildapp][ebe7] to build a binary.

<a id="x-28CL-BREWER-DOCS-2FINDEX-3A-3A-40CL-BREWER-2FDEPLOY-2FFORMULA-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### CL-BREWER/DEPLOY/FORMULA

<a id="x-28-23A-28-2824-29-20BASE-CHAR-20-2E-20-22CL-BREWER-2FDEPLOY-2FFORMULA-22-29-20PACKAGE-29"></a>

#### [package](c93e) `cl-brewer/deploy/formula`

<a id="x-28CL-BREWER-DOCS-2FINDEX-3A-3A-7C-40CL-BREWER-2FDEPLOY-2FFORMULA-3FClasses-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Classes

<a id="x-28CL-BREWER-DOCS-2FINDEX-3A-3A-40CL-BREWER-2FDEPLOY-2FFORMULA-24DEPLOY-FORMULA-3FCLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

##### DEPLOY-FORMULA

<a id="x-28CL-BREWER-2FDEPLOY-2FFORMULA-3ADEPLOY-FORMULA-20CLASS-29"></a>

###### [class](f092) `cl-brewer/deploy/formula:deploy-formula` (formula)

This formula class uses [Deploy][eaea] to build a binary.

The core difference from [`cl-brewer/buildapp/formula:buildapp-formula`][854c] is that
this type of formula also builds and distributes all necessary dynamic libraries.

<a id="x-28CL-BREWER-DOCS-2FINDEX-3A-3A-40CL-BREWER-2FFORMULA-3FPACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### CL-BREWER/FORMULA

<a id="x-28-23A-28-2817-29-20BASE-CHAR-20-2E-20-22CL-BREWER-2FFORMULA-22-29-20PACKAGE-29"></a>

#### [package](c184) `cl-brewer/formula`

<a id="x-28CL-BREWER-DOCS-2FINDEX-3A-3A-7C-40CL-BREWER-2FFORMULA-3FGenerics-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Generics

<a id="x-28CL-BREWER-2FFORMULA-3AINCLUDED-SYSTEMS-20GENERIC-FUNCTION-29"></a>

##### [generic-function] `cl-brewer/formula:included-systems` object

<a id="x-28CL-BREWER-2FFORMULA-3AMISSING-SYSTEMS-20GENERIC-FUNCTION-29"></a>

##### [generic-function] `cl-brewer/formula:missing-systems` object

<a id="x-28CL-BREWER-2FFORMULA-3AROOT-SYSTEM-20GENERIC-FUNCTION-29"></a>

##### [generic-function] `cl-brewer/formula:root-system` object

<a id="x-28CL-BREWER-DOCS-2FINDEX-3A-3A-7C-40CL-BREWER-2FFORMULA-3FMacros-SECTION-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

#### Macros

<a id="x-28CL-BREWER-2FFORMULA-3ADEFINE-QUESSER-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29"></a>

##### [macro](b050) `cl-brewer/formula:define-quesser` name (asdf-system) &body body

Use this macro to define a function to guess a formula class.

The function should accept a one argument - an `ASDF` system and
return a symbol denoting a class derived from [`formula`][4e9e] class.

If guesser does not know how to create a formula for the system,
then it should return a `NIL` value.


[1887]: https://40ants.com/cl-brewer/
[854c]: https://40ants.com/cl-brewer/#x-28CL-BREWER-2FBUILDAPP-2FFORMULA-3ABUILDAPP-FORMULA-20CLASS-29
[de5a]: https://40ants.com/cl-brewer/#x-28CL-BREWER-2FDEPLOY-2FFORMULA-3ADEPLOY-FORMULA-20CLASS-29
[4e9e]: https://40ants.com/cl-brewer/#x-28CL-BREWER-3AFORMULA-20CLASS-29
[df7d]: https://github.com/40ants/cl-brewer
[a54e]: https://github.com/40ants/cl-brewer/actions
[bcf0]: https://github.com/40ants/cl-brewer/blob/ea36d2a643088451f19d01d6ce2a3ede2362ff0b/src/buildapp/formula.lisp#L1
[4b86]: https://github.com/40ants/cl-brewer/blob/ea36d2a643088451f19d01d6ce2a3ede2362ff0b/src/buildapp/formula.lisp#L13
[34a5]: https://github.com/40ants/cl-brewer/blob/ea36d2a643088451f19d01d6ce2a3ede2362ff0b/src/core.lisp#L1
[c93e]: https://github.com/40ants/cl-brewer/blob/ea36d2a643088451f19d01d6ce2a3ede2362ff0b/src/deploy/formula.lisp#L1
[f092]: https://github.com/40ants/cl-brewer/blob/ea36d2a643088451f19d01d6ce2a3ede2362ff0b/src/deploy/formula.lisp#L14
[d55a]: https://github.com/40ants/cl-brewer/blob/ea36d2a643088451f19d01d6ce2a3ede2362ff0b/src/formula-impl.lisp#L127
[c184]: https://github.com/40ants/cl-brewer/blob/ea36d2a643088451f19d01d6ce2a3ede2362ff0b/src/formula.lisp#L1
[b050]: https://github.com/40ants/cl-brewer/blob/ea36d2a643088451f19d01d6ce2a3ede2362ff0b/src/formula.lisp#L147
[05e1]: https://github.com/40ants/cl-brewer/blob/ea36d2a643088451f19d01d6ce2a3ede2362ff0b/src/formula.lisp#L21
[0e6e]: https://github.com/40ants/cl-brewer/blob/ea36d2a643088451f19d01d6ce2a3ede2362ff0b/src/formula.lisp#L22
[b0d4]: https://github.com/40ants/cl-brewer/blob/ea36d2a643088451f19d01d6ce2a3ede2362ff0b/src/formula.lisp#L25
[560c]: https://github.com/40ants/cl-brewer/blob/ea36d2a643088451f19d01d6ce2a3ede2362ff0b/src/formula.lisp#L28
[d4c3]: https://github.com/40ants/cl-brewer/blob/ea36d2a643088451f19d01d6ce2a3ede2362ff0b/src/formula.lisp#L34
[97c6]: https://github.com/40ants/cl-brewer/blob/ea36d2a643088451f19d01d6ce2a3ede2362ff0b/src/formula.lisp#L38
[557c]: https://github.com/40ants/cl-brewer/issues
[f9c6]: https://github.com/benesch/quicklisp-homebrew-roundup
[cccc]: https://github.com/can3p/homebrew-cl-journal/blob/master/cl-journal.rb
[44a6]: https://github.com/edicl/cl-unicode/blob/8073fc5634c9d4802888ac03abf11dfe383e16fa/cl-unicode.asd#L67-L70
[795a]: https://github.com/roswell/roswell
[8236]: https://quickdocs.org/alexandria
[0ce0]: https://quickdocs.org/cl-plus-ssl-osx-fix
[bdb0]: https://quickdocs.org/command-line-arguments
[90b9]: https://quickdocs.org/ironclad
[9c78]: https://quickdocs.org/quicklisp
[923b]: https://quickdocs.org/trivial-download
[eaea]: https://shinmera.github.io/deploy/
[ebe7]: https://xach.com/lisp/buildapp/

* * *
###### [generated by [40ANTS-DOC](https://40ants.com/doc/)]

<a id="x-28CL-BREWER-DOCS-2FCHANGELOG-3A-40CHANGELOG-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

# ChangeLog

<a id="x-28CL-BREWER-DOCS-2FCHANGELOG-3A-3A-7C0-2E11-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.11.0 (2023-07-26)

* Now `CL_SOURCE_REGISTRY` includes current dir non-recursively and all dependencies
  are placed in the `_brew_resources` folder instead of `lib`.

* Also, all warnings are muffled on compilation.

* And `--formula-name` option was added. This way you'll be able to override filename
  of the formula.

<a id="x-28CL-BREWER-DOCS-2FCHANGELOG-3A-3A-7C0-2E10-2E5-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.10.5 (2023-07-25)

* Separated system with hooks into it's own `ASDF` system, solved a fiew other build errors.

<a id="x-28CL-BREWER-DOCS-2FCHANGELOG-3A-3A-7C0-2E10-2E4-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.10.4 (2023-07-24)

* Cleanup and use newer 40ants-asdf-system for proper `--version` handling.

<a id="x-28CL-BREWER-DOCS-2FCHANGELOG-3A-3A-7C0-2E10-2E3-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.10.3 (2023-07-23)

* Fixed arguments parsing.

<a id="x-28CL-BREWER-DOCS-2FCHANGELOG-3A-3A-7C0-2E10-2E2-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.10.2 (2023-07-23)

* Fixed how cl-brewer's version number is stored in the binary.

<a id="x-28CL-BREWER-DOCS-2FCHANGELOG-3A-3A-7C0-2E10-2E1-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.10.1 (2023-07-23)

* Fixed build commands escaping for Deploy formula.

<a id="x-28CL-BREWER-DOCS-2FCHANGELOG-3A-3A-7C0-2E10-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.10.0 (2023-07-23)

* Library was refactored to use package-inferred system style. Also a new style documentation was added.

<a id="x-28CL-BREWER-DOCS-2FCHANGELOG-3A-3A-7C0-2E9-2E1-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.9.1 (2023-07-14)

* Fixed an error happened in case if program does not have any dynamic libs to install into the Cellar.

<a id="x-28CL-BREWER-DOCS-2FCHANGELOG-3A-3A-7C0-2E9-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.9.0 (2023-07-14)

Now when using Deploy, dynamic libraries copied into the libexec
folder inside Cellar and not symlinked into the /ope/homebrew/bin folder.

Also, dynamic libraries provided by other Homebrew formulas are not
copied into the Cellar, instead these formulas are listed as a dependency
of the formula, created by cl-homebrew.

<a id="x-28CL-BREWER-DOCS-2FCHANGELOG-3A-3A-7C0-2E8-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.8.0 (2019-09-12)

* Now cl-brewer suppress debug output of the Deploy's startup process.

* Added a `--version` command line option.

* Fixed creation of dist, local-projects and tmp in the current directory.

<a id="x-28CL-BREWER-DOCS-2FCHANGELOG-3A-3A-7C0-2E7-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.7.0 (2019-09-11)

* Added a generic-function [`cl-brewer:get-implicit-dependencies`][dda5] which can be used to
specify additional dependencies for systems when asdf is not possible to figure them out.

It can be used like that:

```
(defmethod cl-brewer:get-implicit-dependencies ((system-name (eql :cl-unicode)))
    :flexi-streams)
```
It can return a single keyword or a list of keywords.

<a id="x-28CL-BREWER-DOCS-2FCHANGELOG-3A-3A-7C0-2E6-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.6.0 (2019-07-30)

* Now cl-brewer can be installed from the Homebrew tap as a binary.

<a id="x-28CL-BREWER-DOCS-2FCHANGELOG-3A-3A-7C0-2E5-2E6-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.5.6 (2019-07-22)

* Fixed arguments list in buildapp's entrypoint.

<a id="x-28CL-BREWER-DOCS-2FCHANGELOG-3A-3A-7C0-2E5-2E5-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.5.5 (2019-07-22)

* Fixed a bug in preloading code on buildapp.

<a id="x-28CL-BREWER-DOCS-2FCHANGELOG-3A-3A-7C0-2E5-2E4-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.5.4 (2019-07-22)

* Added support for `--preload` option.
It allows to embedd Quicklisp client when building a binary for a homebrew.

<a id="x-28CL-BREWER-DOCS-2FCHANGELOG-3A-3A-7C0-2E5-2E3-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.5.3 (2019-07-22)

* More diagnostics to understand why does not work a binary built by homebrew.

<a id="x-28CL-BREWER-DOCS-2FCHANGELOG-3A-3A-7C0-2E5-2E2-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.5.2 (2019-07-22)

* Added an entry point for buildapp.

<a id="x-28CL-BREWER-DOCS-2FCHANGELOG-3A-3A-7C0-2E5-2E1-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.5.1 (2019-07-21)

* Kickstarting quicklisp client when building cl-brewer.

<a id="x-28CL-BREWER-DOCS-2FCHANGELOG-3A-3A-7C0-2E5-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.5.0 (2019-07-21)

* Now cl-brewer can work with package-inferred asdf systems.

* Previously, some archives were included more than once because
  they contains multiple systems. Now this is fixed.

* Added option `--compress-core`. It reduces size of a simple
  "hello world" from `52M` to `13M`.

* Function `cl-brewer::create-formula` was made external and now
  can accept system name as a symbol.

* Function `cl-brewer::save-formula` was made external.

* `ASDF` option `defsystem-depends-on` was supported.

<a id="x-28CL-BREWER-DOCS-2FCHANGELOG-3A-3A-7C0-2E4-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.4.0 (2016-12-17)

* Provide a way to skip some systems if not found by quicklisp. Right now it's only sb-introspect

<a id="x-28CL-BREWER-DOCS-2FCHANGELOG-3A-3A-7C0-2E3-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.3.0 (2016-11-10)

* A bit of doc strings.

* Cli interface.

* Code was split into multiple files.

<a id="x-28CL-BREWER-DOCS-2FCHANGELOG-3A-3A-7C0-2E2-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.2.0 (2016-11-04)

Initial working functionality.


[dda5]: https://40ants.com/cl-brewer/#x-28CL-BREWER-3AGET-IMPLICIT-DEPENDENCIES-20GENERIC-FUNCTION-29

* * *
###### [generated by [40ANTS-DOC](https://40ants.com/doc/)]

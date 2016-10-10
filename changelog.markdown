# 2010-11-18 Dr. Alistair Ward <regexchar@functionalley.eu>

## 0.9.0.0
* First version of the package.

## 0.9.0.1
* Reacted to major-number changes in the packages "**ToolShed-0.10.0.0**" & "**RegExDot-0.10.0.0**".
* Created **src/RegExChar/** (& then modified module-names accordingly), & sub-directory "**data/**".
* Pacified **hlint**.
* Qualified identifiers used in error-messages.

## 0.9.0.2
* Ported to ghc-7.0.1:
	+ Replaced the deprecated module "**Test.QuickCheck.Batch**", & labelled properties.
* Added the cabal-flag "**threaded**", to account for the requirement to link with the threaded runtime when using a threaded version of the library package "**RegExDot**".

## 0.9.0.3
* Parallelised processing where executable "**grecce**" is requested to read multiple input-data files.
* Extracted matching-operation from the module "**Main**", into new module "**Grecce.Grep**".
* Migrated modules required only by the executable "**grecce**", from the directory "**RegExChar**" into the new directory "**Grecce**".
* Replaced use of @threaded@-flag in source-code, with Cabal CPP-macro.
* Added *.spec*-file.


## 0.9.0.4
* Added default values of command-line flags, to usage-message.
* Reacted to migration of **RegExDot.Options** to **ToolShed.Options**.
* Used new module "**ToolShed.Package**".

## 0.9.0.5
* Fixed negated reference to unparenthesised CPP-macro "**MIN_VERSION**".
* Protected reference in module "**Grecce/QC/QuickChecks**", to `Test.QuickCheck.verboseCheck`, using CPP-statement.

## 0.9.0.6
* Amended RPM *.spec*-file, to install under directory "**/usr**", rather than "**/usr/local**".
* Renamed package from "**RegExChar**" to "**regexchar**", for compatibility with Debian's *.deb*-format.
* Added directory-structure required to build *.deb*-package.

## 0.9.0.7
* Redefined man-page to be an "**extra-source-file**" in the *.cabal*-file & the *.spec*-file.
* Added manually controlled flag "**llvm**" to the *.cabal*-file.
* Replaced references to the module "**ToolShed.Package**" with "**Distribution.Package**".
* Changed identifier for type-parameters, to better reflect its role.
* Removed dependency on archaic module "**ToolShed.Arithmetic**".
* Reacted to the creation of module "**ToolShed.Defaultable**".
* Uploaded to [Hackage](http://hackage.haskell.org/package/regexchar).

## 0.9.0.8
* Amended the *.cabal*-file to more correctly specify dependency on package "**toolshed**".
* Used new module "**ToolShed.Pair**" from package toolshed-0.12.0.0.
* Relocated modules "**Grecce.Assert**", "**Grecce.Performance**" and "**Grecce.QC**", into new layer "**Test**", in module-hierarchy.
* Guarded flag "**eager-blackholing**" in the *cabal*-file.

## 0.9.0.9
* Recoded module "**RegExChar.RegExOptsChar**" to the interface `Data.Array.IArray.IArray`, rather than the data-type `Data.Array.Array`.
* Replaced `(+ 1)` and `(- 1)` with the faster calls `succ` and `pred`, in modules "**RegExChar.RegExOptsChar**", "**Grecce.Test.Performance.ExtendedRegExTestsPositive**" and "**Grecce.Grep**".
* Used `Paths_regexchar.version` in **Main**, rather than hard-coding it.
* Reacted to new module-hierarchy and addition of method `ToolShed.SelfValidate.getErrors`, in package "**toolshed-0.13.0.0**", and used it to improved error-reporting in @instance Read RegExChar.ExtendedRegExChar.ExtendedRegExChar@.
* Replaced use of `Control.Monad` in **RegExChar.MetaChar** and **RegExChar.ExtendedRegExChar**, with `Control.Applicative`.
* Replaced `System` with `System.Environment` and `System.Exit`.
* Removed dependency on **haskell98**.

## 0.9.0.10
* Fully qualified symbols imported from package "**regexdot**".
* Used new constant `RegExDot.Anchor.unanchored`.
* Added details to any failure to parse the command-line arguments.
* Used new test-modules from package "**toolshed-0.14.0.0**".

## 0.9.0.11
* Refactored module "**Grecce.Test.QC.QuickChecks**".
* Replaced calls to `error` from inside the IO-monad, with `Control.Monad.fail`.
* Trapped command-line arguments to which garbage has been appended.
* Added files to build *.deb* to the *.cabal*-file.
* Tested with **haskell-platform-2013.2.0.0**.
* Replaced preprocessor-directives with **build-depends** constraints in the *.cabal*-file.

## 0.9.0.12
* Either replaced instances of `(<$>)` with `fmap` to avoid ambiguity between modules "**Control.Applicative**" & "**Prelude**" which (from package "**base-4.8**") also exports this symbol, or hid the symbol when importing the module "**Prelude**".

## 0.9.0.13
* Added the compiler to the output returned for the command-line option "**version**".
* Changed flag "**threaded**" in the *.cabal*-file to **manual**.
* Added "**Default-language**"-specification to the *.cabal*-file.
* Added file "**README.markdown**".
* Converted this file to markdown-format.
* Replaced `System.Exit.exitWith System.Exit.ExitSuccess` with `System.Exit.exitSuccess`.
* Moved the entry-point to the test-suite from module "**Main.hs**" to "**Test.hs**", both to integrate with **cabal** & to minimise the dependencies of the executable.
* Partitioned the source-files into directories "**src-lib**", "**src-exe**", & "**src-test**", & referenced them individually from the *.cabal*-file to avoid repeated compilation.
* Used **CPP** to control the import of symbols from **Control.Applicative**.

## 0.9.0.14
* Corrected the markdown-syntax in this file.
* Reverted to calling "**error**" rather than "**Control.Monad.fail**", since the **String**-argument for the latter is discarded in **Monad**-implementations other than **IO**.
* Uploaded to [GitHub](https://github.com/functionalley/RegExChar.git).
* Simplified file **src-test/Main.hs**.
* Added **prop_read** to modules; **Grecce.Test.QC.{RepeatableMetaChar,ExtendedRegExChar}**.
* Replaced calls to **error** with **[]**, in implementation of **Read**, in module **RegExChar.ExtendedRegExChar**.
* Added file **.travis.yml** to control testing by <https://docs.travis-ci.com>.
* Added file **.ghci**.
* Replaced use of module **ToolShed.Defaultable** with **Data.Default**.
* Reimplemented **RegExChar.MetaChar.deconstruct**, **QuickCheck.ExtendedRegExChar.deconstruct**, **QuickCheck.RepeatableMetaChar.deconstruct** using record-syntax.
* Tested with **ghc-8.0.1**.
## 0.9.0.15

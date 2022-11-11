# Changelog for Posit Numbers

# posit-3.2.0.2

  * Added `FlexableContexts` back in to Posit.hs, a build error occured on GHC-9.2 that didn't occur with GHC-9.0 or GHC-8.10

# posit-3.2.0.1

  * Refactored `IntN` Type Family to be a closed type family instead of an associated type family
  * Refactored `IntN` constraints to use `ConstraintKinds` and made that to be a Super Class of `PositC` to improve the encapsulation the Constraints of the internal implementation
  * Refactored `PositC` to make use of `ConstrainedClassMethods` vastly reducing code duplication
  * Eliminated the `FlexableContexts` Language Extension from Posit.hs Interface, since the `InN` constraints no longer bleed into that file
  * Added test of Heegner numbers (almost integers)
  * Added test of various Gamma Function approximations
  * Improved function names in the Orphan Instance for `Storable` ( `Word128` )
  * Improved documentation

## posit-3.2.0.0

  * Posit Standard 3.2 [Posit Standard] (https://posithub.org/docs/posit_standard.pdf)
  * LiquidHaskell support: stack build --flag posit:do-liquid
  * To run the test suite: stack test --flag posit:do-test
  * To play around: stack repl --flag posit:do-test


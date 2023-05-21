# Changelog for Posit Numbers

# posit-2022.0.1.0

  * Added Random and Uniform Instances
  * `Uniform` provides a uniform distribution over all possible posits, sampling the entire projective real line
  * `Random` provides a uniform distribution between 0 and 1, sampling the real numbers in that range
  * Example: take 10 (randomRs (1,100) (mkStdGen 2023)) :: [Posit64]
  * Added `hypot2`, `hypot3`, `hypot4`, to the `AltFloating` class

# posit-2022.0.0.1

  * Added `PositF` constraint synonym to simplify the usage of `(PositC es, PositC (Next es))`, as is needed for `Floating` instances
  * More coverage in the test suite for multiple types

# posit-2022

  * Added Types (P8, P16, P32, P64, P128, P256) for the Posit Standard 2022 encoding, exponent size = 2, and with nBytes = 2^es
  * Refactored `Floating` to step up in resolution and then calculate a function, and then round it down to the the lower resolution
  * Added polymorphic `Posit es` approximations for the `Floating` class
  * Moved functions used in the test suite to the Test.Algorithms module, to eliminate the `do-test` flag
  * Since the test flag has been removed the test can be run by: stack test
  * Please forgive the lack of camelCase in some of the Floating functions... I think it reads better this time
  * The Weigh test can be run as a benchmark: stack bench

# posit-3.2.0.5

  * Bug fix for `mkIntRep` to resolve an overflow issue with the fractional part when it rounds up, in anticipation of the 2022 Standard release

# posit-3.2.0.4

  * No more Orphan Instances for Storable!
  * Figured out how to resolve the orphan instances problem with `newtype`, `DerivingVia` and `UndecidableInstances`.
  * Added a "weigh" based test to verify the proper size of each Posit type
  * Added a WeighPosit test; command to run: stack test posit:test-posit-weigh
  * Added NFData instance.
  * New GitHub Snapshot of Liquid Haskell makes it work with GHC 9.0.2!

# posit-3.2.0.3

  * Made the following changes in anticipation of adding the 2022 Posit Standard:
      * Made the `IntN` type family non-Injective, and added more visable type applications to help the compiler select the proper types
      * Corrected some bad uses of `nBytes @es`, with `2^(exponentSize @es)`, in order to be more general
      * Chagned `maxPosRat` to match the more general form as described in "Posit Arithmetic" (John L Gustafson, 10 October 2017)
      * Changed `lnOf2` to be a long decimal value, in order to be more general
  * Changed Borwein's algorithm, with quintic convergence, to check for a fixed point of both `a` and `s`
  * Added Borwein's Quadradic 1985
  * Added Borewein's Cubic

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


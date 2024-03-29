# Changelog for Posit Numbers

# posit-2022.2.0.0

  * Updated divide (/) to have one less round operation, previous default was `a * recip b`, divide two rational numbers then round `viaRational2 (/) a b`, it sort of fuses the old default, reducing error
  * New flag for Fused Operations applied automatically via rewrite rules `--flag posit:do-rewrite`
  * Test for Fused Operations `stack test posit:test-posit-fusedRewrite`, with no fusing
  * Test for Fused Operations `stack test posit:test-posit-fusedRewrite --flag posit:do-rewrite`, to execute with the rewrite rules for fusing
  * Added several `fms` or `fma` functions to `asin`, `acos`, `asinh`, `acosh`. No apparent change in the elementary functions.
  * User defined rewrite rules seem to work before but break in `ghc-9.6.3`.  I really hope it gets fixed.  See: (How Cool is Rulz!)[https://discourse.haskell.org/t/how-cool-is-rulz/7738/9]
  * Added new `approxEq` function to the `AltFloating` class
  * Implemented `approxEq` by converting to the next lower `ES` and then using the normal `(==)` function, using a new `Prev es` type family
  * Added new `Prev es` type family to the `PositF es` Constraint Synonym
  * Changed naming in `AltFloating` to free up the name space and not use greek letters, `eps` is now `machEps`, `psi` is now `goldenRatio`
  * Removed `gamma`, `sinc`, `expm1` from `AltFloating` making way for updates to the Posit Standard in the future for special functions

# posit-2022.1.0.0

  * Optimized the `Show` instance to properly handle the tapered precision
  * Changes to round better
  * Improved accuracy of `exp`
  * Changed titles of accuracy charts to be more consistant
  * Added test to confirm `read.show == id` to perhaps optimise the textual representation, and verify that the implementation is sufficent
  * Test round trip with command to run: `stack test posit:test-posit-readShowId`
  * Added catagory to .cabal file

# posit-2022.0.1.4

  * `atan 1 :: Posit256` is very slow, added a few precice constants to speed it up
  * Improved accuracy of `sin` and `cos` at the extreme positive and negative values

# posit-2022.0.1.3

  * Much thanks to @svlc sugesting to link to a raw image rather than an html, and that finaly made the images display on Hackage

# posit-2022.0.1.2

  * Another attempt to get the images working

# posit-2022.0.1.1

  * Fixed loss of precision bug in some of the Floating instances (exp,sin,cos)
  * Added a Chart test; command to run: stack test posit:test-posit-functions
  * Added test results to the README.md file

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


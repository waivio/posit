
--------------------------------------------------------------------------------------------
--   Posit Numbers
--   Copyright   :  (C) 2022 Nathan Waivio
--   License     :  BSD3
--   Maintainer  :  Nathan Waivio <nathan.waivio@gmail.com>
--   Stability   :  Stable
--   Portability :  Portable
--
-- | Library implementing standard Posit Numbers (Posit Standard version
--   3.2.0.0, with some improvements) a fixed width word size of
--   2^es bytes.
-- 
---------------------------------------------------------------------------------------------


{-# LANGUAGE GADTs #-} --   For our main type Posit (es :: ES)
{-# LANGUAGE DataKinds #-}  --   For our ES kind and the constructors Z, I, II, III, IV, V for exponent size type
{-# LANGUAGE KindSignatures #-}  --   For defining the type of kind ES that indexes the GADT
{-# LANGUAGE ViewPatterns #-}  --   To decode the posit in the pattern
{-# LANGUAGE BangPatterns #-}  --   Added Strictness for some fixed point algorithms
{-# LANGUAGE PatternSynonyms #-}  --   for a nice NaR interface
{-# LANGUAGE FlexibleInstances #-} --   To make instances for each specific type [Posit8 .. Posit256]
{-# LANGUAGE TypeApplications #-} --   To apply types: @Type, it seems to select the specific class instance, when GHC is not able to reason about things, commenting this out shows an interesting interface
{-# LANGUAGE MultiParamTypeClasses #-}  --   To convert between Posit Types
{-# LANGUAGE ScopedTypeVariables #-} --   To reduce some code duplication
{-# LANGUAGE UndecidableInstances #-}  --   To reduce some code duplication, I think the code is decidable but GHC is not smart enough ;), like there being only 1 instance that is polymorphic and works for all of my types.
{-# LANGUAGE CPP #-} --   To remove Storable instances to remove noise when performing analysis of Core
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}  --   Turn off noise
{-# OPTIONS_GHC -Wno-type-defaults #-}  --   Turn off noise
{-# OPTIONS_GHC -Wno-unused-top-binds #-}  --   Turn off noise


-- ----
--  Posit numbers implementing:
--
--    * Show
--    * Eq
--    * Ord  -- compare as an integer representation
--    * Num  -- Addition, subtraction, multiplication, and other operations
--    * Enum  -- Successor and Predecessor
--    * Fractional  -- division, divide by zero is Not a Real (NaR) number
--    * Real
--    * Bounded
--    * FusedOps  -- dot product and others
--    * Convertible  -- Conversions between different posit formats
--    * AltShow
--    * Read
--    * Storable  -- Formats for binary data, for computation and data interchange
--    * RealFrac
--    * RealFloat
--    * Floating  -- Mathematical functions such as logarithm, exponential, trigonometric, and hyperbolic functions. Warning! May induce trance.
--
-- ----

module Posit
(Posit(),
 -- * Main Exported Types
 Posit8, -- |An 8-bit Posit number with 'es' ~ 'Z'
 Posit16, -- |An 16-bit Posit number with 'es' ~ 'I'
 Posit32, -- |An 32-bit Posit number with 'es' ~ 'II'
 Posit64, -- |An 64-bit Posit number with 'es' ~ 'III'
 Posit128, -- |An 128-bit Posit number with 'es' ~ 'IV'
 Posit256, -- |An 256-bit Posit number with 'es' ~ 'V'
 
 -- * Patterns for Matching Exported Types
 pattern NaR,  -- |A pattern for Exception handling when a value is Not a Real number (NaR).
 pattern R,  -- |A pattern for the non-Exceptional case, yielding a Rational, will make a total function when paired with NaR, if the Rational implementation is total.
 
 -- * Fused Operation Interface defined by the Posit Standard
 FusedOps(..),
 
 -- * Posits are Convertable between different Posit representations
 Convertible(..),
 
#ifndef O_NO_SHOW
 -- * Additional functions to show the Posit in different formats
 AltShow(..),
#endif
 
 -- * Additional Special Functions
 AltFloating(..),
 
 -- * Functions to lift functions of Integers or Rationals to operate on Posit Types
 viaIntegral,
 viaRational,
 viaRational2,
 viaRational3,
 viaRational4,
 viaRational6,
 viaRational8,
 
#ifdef O_TEST
 -- * Alternative algorithms for test purposes
 funExp,
 funExp2,
 funExpTaylor,
 funLogTaylor,
 funExpTuma,
 funGammaSeriesFused,
 funGammaRamanujan,
 funGammaCalc,
 funGammaNemes,
 funGammaYang,
 funGammaChen,
 funGammaXminus1,
 funLogTuma,
 funLogDomainReduction,
 funPi1,
 funPi2,
 funPi3,
 funPi4,
 funPsiSha1,
 funPsiSha2,
 funPsiSha3
#endif

 ) where


import Prelude hiding (rem)

-- Imports for Show and Read Instances
import Data.Scientific (scientificP
                       ,fromRationalRepetendUnlimited
                       ,formatScientific
                       ,FPFormat(Generic)) -- Used to print/show and read the rational value

import Text.Read (Lexeme(Ident)
                 ,readPrec
                 ,readListPrec
                 ,(+++)
                 ,pfail
                 ,readListPrecDefault
                 ,lexP
                 ,lift
                 ,parens) -- Used to read a Posit value

-- Imports for Vectorization Class Instances
import Data.Foldable (toList)  -- Used for fused operations on foldable/lists

-- Imports for Storable Instance
import Foreign.Storable (Storable, sizeOf, alignment, peek, poke)  -- Used for Storable Instances of Posit
import Foreign.Ptr (Ptr, castPtr)  -- Used for dealing with Pointers for the Posit Storable Instance


-- would like to:
-- import Posit.Internal.ElementaryFunctions
-- Perhaps on the chopping block if we are moving to ElementaryFunctions
-- Imports for implementing the Transcendental Functions
import GHC.Natural (Natural) -- Import the Natural Numbers ℕ (u+2115) for some of the Transcendental Functions
import Data.Ratio ((%))  -- Import the Rational Numbers ℚ (u+211A), ℚ can get arbitrarily close to Real numbers ℝ (u+211D), used for some of the Transcendental Functions

import Debug.Trace (trace) -- temporary for debug purposes


-- =====================================================================
-- ===                  Posit Implementation                         ===
-- =====================================================================

-- The machine implementation of the Posit encoding/decoding
import Posit.Internal.PositC  -- The main internal implementation details


-- |Base GADT rapper type, that uses the Exponent Size kind to index the various implementations
data Posit (es :: ES) where
     Posit :: PositC es => !(IntN es) -> Posit es

-- |Not a Real Number, the Posit is like a Maybe type, it's either a real number or not
pattern NaR :: PositC es => Posit es
pattern NaR <- (Posit (decode -> Nothing)) where
  NaR = Posit unReal
--

--
-- |A Real or at least Rational Number, rounded to the nearest Posit Rational representation
pattern R :: PositC es => Rational -> Posit es
pattern R r <- (Posit (decode -> Just r)) where
  R r = Posit (encode $ Just r)
--

-- Posit functions are complete if the following two patterns are completely defined.
{-# COMPLETE NaR, R #-}

-- Concrete types exported for use.
type Posit8 = Posit Z
type Posit16 = Posit I
type Posit32 = Posit II
type Posit64 = Posit III
type Posit128 = Posit IV
type Posit256 = Posit V

#ifndef O_NO_SHOW
-- Show
--
instance PositC es => Show (Posit es) where
  show NaR = "NaR"
  show (R r) = formatScientific Generic (Just $ decimalPrec @es) (fst.fromRationalRepetendUnlimited $ r)
--
#endif



-- Two Posit Numbers are Equal if their Finite Precision Integer representation is Equal
--
-- All things equal I would rather write it like this:
instance PositC es => Eq (Posit es) where
  (Posit int1) == (Posit int2) = int1 == int2
--



-- Two Posit Numbers are ordered by their Finite Precision Integer representation
--
-- Ordinarily I would only like one instance to cover them all
instance PositC es => Ord (Posit es) where
  compare (Posit int1) (Posit int2) = compare int1 int2
--



-- Num
--
-- I'm num trying to get this definition:
instance PositC es => Num (Posit es) where
  -- Addition
  (+) = viaRational2 (+)
  -- Multiplication
  (*) = viaRational2 (*)
  -- 'abs', Absolute Value, it's like a magnitude of sorts, abs of a posit is the same as abs of the integer representation
  abs = viaIntegral abs
  -- 'signum' it is a kind of an representation of directionality, the sign of a number for instance
  signum = viaRational signum
  -- 'fromInteger' rounds the integer into the closest posit number
  fromInteger int = Posit $ encode (Just $ fromInteger int)
  -- 'negate', Negates the sign of the directionality. negate of a posit is the same as negate of the integer representation
  negate = viaIntegral negate
--

-- deriving via Integral Class, for the Integral representation of the posit
viaIntegral :: PositC es => (IntN es -> IntN es) -> Posit es -> Posit es
viaIntegral f (Posit int) = Posit $ f int
--



-- Enum-ish, A Posit has a Successor and Predecessor so its an ordinal number, as per Posit standard next, prior
-- The Posit Standard requires 2's complement integer overflow to be ignored
instance PositC es => Enum (Posit es) where
  -- succ (Posit int) = Posit (int + 1)
  succ = viaIntegral (+1)
  -- succ = viaIntegral succ  -- Non-compliant, runtime error pred NaR, and worse it is Int64 for types of greater precision, probably because of Preludes gross abomination of toEnum/fromEnum
  -- pred (Posit int) = Posit (int - 1)
  pred = viaIntegral (subtract 1)
  -- pred = viaIntegral pred  -- Non-compliant, runtime error pred NaR, and worse it is Int64 for types of greater precision, probably because of Preludes gross abomination of toEnum/fromEnum
  -- enumFrom :: Posit es -> [Posit es]
  enumFrom n = enumFromTo n maxBound
  enumFromTo n m
    | n == m = [n]
    | n < m = n : enumFromTo (succ n) m
    | otherwise = []
  -- enumFromThen n m :: Posit es -> Posit es -> [Posit es]
  enumFromThen NaR _ = [NaR]
  enumFromThen _ NaR = [NaR]
  enumFromThen n m = n : go n
    where
      step = m - n
      go :: Posit es -> [Posit es]
      go NaR = [NaR]
      go !l = case compare step 0 of
                LT -> let !n' = l + step  -- rounding occurs here, because the next comparison needs it, it wouldn't make sense otherwise...
                      in if n' - l > step
                         then []
                         else n' : go n'
                EQ -> [n, m]
                GT -> let !n' = l + step
                      in if n' - l < step
                         then []  -- with tapered resolution this algorithm can reach a fixed point where the next value is equal to the previous value
                         else n' : go n'
  enumFromThenTo NaR  _   _  = [NaR]
  enumFromThenTo  _  NaR  _  = [NaR]
  enumFromThenTo  _   _  NaR = [NaR]
  enumFromThenTo  e1  e2  e3 = takeWhile predicate (enumFromThen e1 e2)
    where
      mid = (e2 - e1) / 2
      predicate | e2 >= e1  = (<= e3 + mid)
                | otherwise = (>= e3 + mid)
--



-- Fractional Instances; (Num => Fractional)
--
-- How the Frac do I get this definition:
instance PositC es => Fractional (Posit es) where
  fromRational = R
 
  recip 0 = NaR
  recip p = viaRational recip p
--

-- Rational Instances; Num & Ord Instanced => Real
--
-- I for real want this definition:
instance PositC es => Real (Posit es) where
  toRational NaR = error "Your input is Not a Real or Rational (NaR) number, please try again!"
  toRational (R r) = r
--

-- Implementing instances via Rational Data Type's instance,
-- The function checks for NaR, to protect against the runtime error 'toRational' would generate if called with a NaR value
-- Unary::Arity NaR guarded pass through with wrapping and unwrapping use of a Rational function
viaRational :: PositC es => (Rational -> Rational) -> Posit es -> Posit es
viaRational _ NaR = NaR
viaRational f (R r) = fromRational $ f r

-- Binary NaR guarded pass through with wrapping and unwrapping use of a Rational function
viaRational2 :: PositC es => (Rational -> Rational -> Rational) -> Posit es -> Posit es -> Posit es
viaRational2 _ NaR  _  = NaR
viaRational2 _  _  NaR = NaR
viaRational2 f (R r1) (R r2) = R $ r1 `f` r2

-- Ternary NaR guarded pass through with wrapping and unwrapping use of a Rational function
viaRational3 :: PositC es => (Rational -> Rational -> Rational -> Rational) -> Posit es -> Posit es -> Posit es -> Posit es
viaRational3 _ NaR  _   _  = NaR
viaRational3 _  _  NaR  _  = NaR
viaRational3 _  _   _  NaR = NaR
viaRational3 f (R r1) (R r2) (R r3) = R $ f r1 r2 r3

-- Quaternary NaR guarded pass through with wrapping and unwrapping use of a Rational function
viaRational4 :: PositC es => (Rational -> Rational -> Rational -> Rational -> Rational) -> Posit es -> Posit es -> Posit es -> Posit es -> Posit es
viaRational4 _ NaR  _   _   _  = NaR
viaRational4 _  _  NaR  _   _  = NaR
viaRational4 _  _   _  NaR  _  = NaR
viaRational4 _  _   _   _  NaR = NaR
viaRational4 f (R r0) (R r1) (R r2) (R r3) = R $ f r0 r1 r2 r3

-- Senary NaR guarded pass through with wrapping and unwrapping use of a Rational function
viaRational6 :: PositC es => (Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational) -> Posit es -> Posit es -> Posit es -> Posit es -> Posit es -> Posit es -> Posit es
viaRational6 _ NaR  _   _   _   _   _  = NaR
viaRational6 _  _  NaR  _   _   _   _  = NaR
viaRational6 _  _   _  NaR  _   _   _  = NaR
viaRational6 _  _   _   _  NaR  _   _  = NaR
viaRational6 _  _   _   _   _  NaR  _  = NaR
viaRational6 _  _   _   _   _   _  NaR = NaR
viaRational6 f (R a1) (R a2) (R a3) (R b1) (R b2) (R b3) = R $ f a1 a2 a3 b1 b2 b3

-- Octonary NaR guarded pass through with wrapping and unwrapping use of a Rational function
viaRational8 :: PositC es => (Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational) -> Posit es -> Posit es -> Posit es -> Posit es -> Posit es -> Posit es -> Posit es -> Posit es -> Posit es
viaRational8 _ NaR  _   _   _   _   _   _   _  = NaR
viaRational8 _  _  NaR  _   _   _   _   _   _  = NaR
viaRational8 _  _   _  NaR  _   _   _   _   _  = NaR
viaRational8 _  _   _   _  NaR  _   _   _   _  = NaR
viaRational8 _  _   _   _   _  NaR  _   _   _  = NaR
viaRational8 _  _   _   _   _   _  NaR  _   _  = NaR
viaRational8 _  _   _   _   _   _   _  NaR  _  = NaR
viaRational8 _  _   _   _   _   _   _   _  NaR = NaR
viaRational8 f (R a0) (R a1) (R a2) (R a3) (R b0) (R b1) (R b2) (R b3) = R $ f a0 a1 a2 a3 b0 b1 b2 b3



-- Bounded, bounded to what?!? To the ℝ! NaR is out of bounds!!!
--
-- I'm bound to want this definition:
instance PositC es => Bounded (Posit es) where
  -- 'minBound' the most negative number represented
  minBound = Posit mostNegVal
  -- 'maxBound' the most positive number represented
  maxBound = Posit mostPosVal
--


-- =====================================================================
-- ===                    Fused Operations                           ===
-- =====================================================================

-- |A class that delays the rounding operation until the end for some operations
class Num a => FusedOps a where
  -- |Fused Multiply Add: (a * b) + c
  fma :: a -> a -> a -> a
  -- |Fused Add Multiply: (a + b) * c
  fam :: a -> a -> a -> a
  -- |Fused Multiply Multiply Subtract: (a * b) - (c * d)
  fmms :: a -> a -> a -> a -> a
  -- |Fused Sum of 3 values: a + b + c
  fsum3 :: a -> a -> a -> a
  -- |Fused Sum of 4 values: a + b + c + d
  fsum4 :: a -> a -> a -> a -> a
  -- |Fused Sum of a List of Posits
  fsumL :: Foldable t => t a -> a
  -- |Fused Dot Product of 3 element vector: (a1 * b1) + (a2 * b2) + (a3 * b3)
  fdot3 :: a -> a -> a -> a -> a -> a -> a
  -- |Fused Dot Product of 4 element vector: (a0 * b0) + (a1 * b1) + (a2 * b2) + (a3 * b3)
  fdot4 :: a -> a -> a -> a -> a -> a -> a -> a -> a
  -- |Fused Dot Product of Two Lists
  fdotL :: Foldable t => t a -> t a -> a
  -- |Fused Subtract Multiply: a - (b * c)
  fsm :: a -> a -> a -> a
 


-- Rational Instance
instance FusedOps Rational where
  fsm a b c = a - (b * c)
  fma a b c = (a * b) + c
  fam a b c = (a + b) * c
  fmms a b c d = (a * b) - (c * d)
  fsum3 a b c = a + b + c
  fsum4 a b c d = a + b + c + d
  fsumL (toList -> l) = go l 0
    where
      go [] acc = acc
      go (x : xs) acc = go xs (acc + x)
  fdot3 a1 a2 a3 b1 b2 b3 = (a1 * b1) + (a2 * b2) + (a3 * b3)
  fdot4 a0 a1 a2 a3 b0 b1 b2 b3 = (a0 * b0) + (a1 * b1) + (a2 * b2) + (a3 * b3)
  fdotL (toList -> l1) (toList -> l2) = go l1 l2 0
    where
      go [] [] acc = acc
      go []  _  _  = error "Lists not the same length"
      go _  []  _  = error "Lists not the same length"
      go (b : bs) (c : cs) acc = go bs cs (fma b c acc)
--

--
instance PositC es => FusedOps (Posit es) where
  -- Fused Subtract Multiply
  fsm = viaRational3 fsm
  -- Fuse Multiply Add
  fma = viaRational3 fma
  -- Fuse Add Multiply
  fam = viaRational3 fam
  -- Fuse Multiply Multiply Subtract
  fmms = viaRational4 fmms
  -- Fuse Sum of 3 Posits
  fsum3 = viaRational3 fsum3
  -- Fuse Sum of 4 Posits
  fsum4 = viaRational4 fsum4
  -- Fuse Sum of a List
  fsumL (toList -> l) = Posit $ encode (Just $ go l 0)
    where
      go :: [Posit es] -> Rational -> Rational
      go [] !acc = acc
      go ((Posit int) : xs) !acc = case decode int of
                                     Nothing -> error "Posit List contains NaR"
                                     Just r -> go xs (acc + r)
  -- Fuse Dot Product of a 3-Vector
  fdot3 = viaRational6 fdot3
  -- Fuse Dot Product of a 4-Vector
  fdot4 = viaRational8 fdot4
  -- Fuse Dot Product of two Lists
  fdotL (toList -> l1) (toList -> l2) = Posit $ encode (Just $ go l1 l2 0)
    where
      go [] [] !acc = acc
      go []  _   _  = error "Lists not the same length"
      go _  []   _  = error "Lists not the same length"
      go ((Posit int1) : bs) ((Posit int2) : cs) !acc = case decode int1 of
                                                          Nothing -> error "First Posit List contains NaR"
                                                          Just r1 -> case decode int2 of
                                                                       Nothing -> error "Second Posit List contains NaR"
                                                                       Just r2 -> go bs cs (acc + (r1 * r2))
--




-- =====================================================================
-- ===                  Conversion Between Posits Types              ===
-- =====================================================================

-- |A Convertible class that will cast or 'convert' between two different Posit es types
class Convertible a b where
  convert :: a -> b

instance (PositC es1, PositC es2) => Convertible (Posit es1) (Posit es2) where
  convert NaR = NaR
  convert (R r) = R r
--


#ifndef O_NO_SHOW
-- =====================================================================
-- ===                Alternative Show Formats                       ===
-- =====================================================================

-- |A Alternative to the typical 'Show' class to assist in displaying the Posit es type in different formats
class AltShow a where
  -- |Display the Posit in its Binary Representation
  displayBinary :: a -> String
  -- |Display the Posit in its Integral Representation
  displayIntegral :: a -> String
  -- |Display the Posit as a Rational
  displayRational :: a -> String
  -- |Display the Posit as a Decimal until the Repetend occurs
  displayDecimal :: a -> String
--

--
instance PositC es => AltShow (Posit es) where
  displayBinary (Posit int) = displayBin int
 
  displayIntegral (Posit int) = show int
 
  displayRational = viaShowable id
 
  displayDecimal = viaShowable (fst.fromRationalRepetendUnlimited)
--

viaShowable :: (Show a, PositC es) => (Rational -> a) -> Posit es -> String
viaShowable _ NaR = "NaR"
viaShowable f (R r) = show $ f r
#endif

#ifndef O_NO_READ
-- =====================================================================
-- ===                         Read Posit                            ===
-- =====================================================================

--
instance PositC es => Read (Posit es) where
  readPrec =
    parens $ do
      x <- lexP
      case x of
        Ident "NaR" -> return NaR
        _ -> pfail
      +++
      do
        s <- lift scientificP
        return $ R (toRational s)
 
  readListPrec = readListPrecDefault
--
#endif


-- =====================================================================
-- ===                  Storable Instances                           ===
-- =====================================================================
--
#ifndef O_NO_STORABLE
--
instance PositC es => Storable (Posit es) where
  sizeOf _ = fromIntegral $ nBytes @es
  alignment _ = fromIntegral $ nBytes @es
  peek ptr = do
    int <- peek (castPtr ptr :: Ptr (IntN es))
    return $ Posit int
  poke ptr (Posit int) = do
    poke (castPtr ptr :: Ptr (IntN es)) int
--
#endif


-- =====================================================================
-- ===                        Real Frac                              ===
-- =====================================================================

--
instance PositC es => RealFrac (Posit es) where
  -- properFraction :: Integral b => a -> (b, a)
  properFraction = viaRationalErrTrunkation "NaR value is not a RealFrac" properFraction
--

viaRationalErrTrunkation :: PositC es => String -> (Rational -> (a, Rational)) -> Posit es -> (a, Posit es)
viaRationalErrTrunkation err _ NaR = error err
viaRationalErrTrunkation _ f (R r) =
  let (int, r') = f r
  in (int, R r')

-- =====================================================================
-- ===                         Real Float                            ===
-- =====================================================================
--
instance (Floating (Posit es), PositC es) => RealFloat (Posit es) where
  isIEEE _ = False
  isDenormalized _ = False
  isNegativeZero _ = False
 
  isNaN NaR = True
  isNaN  _  = False
 
  isInfinite NaR = True
  isInfinite _ = False
 
  -- 'atan2' of y x is the argument "arg function" (also called phase or angle) of the complex number x + i y.
  -- angle from an x basis vector to some other vector
  --
  --     Y
  --     ^
  --     |    ^ (x,y)
  --     |   /
  --     |  / <-  alpha (radians)
  --     | /                      \
  --      /                        |
  --      -----------------------------------> X
  --
  --
  atan2 NaR  _  = NaR
  atan2  _  NaR = NaR
  atan2 y x
    | x == 0 && y == 0 = NaR
    | x > 0             = atan (y/x)
    | x < 0  && y >= 0  = atan (y/x) + pi
    | x < 0  && y  < 0  = atan (y/x) - pi
    | x == 0 && y  > 0  = pi / 2
    | x == 0 && y  < 0  = negate $ pi / 2
    | otherwise = error "What!?!?!" -- The case where x == 0 && y == 0
 
  floatRadix _ = 2
  floatDigits _ = undefined
  floatRange _ = (negate maxExponent, maxExponent)
    where
      maxExponent = fromIntegral $ (nBytes @es) * ((nBits @es) - 2)
  decodeFloat = undefined
  encodeFloat = undefined
--



-- =====================================================================
-- ===                         Floating                              ===
-- =====================================================================


instance Floating Posit8 where
  pi = convert (pi :: Posit256) :: Posit8
  exp x = convert (exp (convert x) :: Posit256) :: Posit8
  log x = convert (log (convert x) :: Posit256) :: Posit8
  x ** y = convert $ (convert x :: Posit256) ** (convert y :: Posit256) :: Posit8
  sin x = convert (sin (convert x) :: Posit256) :: Posit8
  cos x = convert (cos (convert x) :: Posit256) :: Posit8
  asin x = convert (asin (convert x) :: Posit256) :: Posit8
  acos x = convert (acos (convert x) :: Posit256) :: Posit8
  atan x = convert (atan (convert x) :: Posit256) :: Posit8
  sinh x = convert (sinh (convert x) :: Posit256) :: Posit8
  cosh x = convert (cosh (convert x) :: Posit256) :: Posit8
  asinh x = convert (asinh (convert x) :: Posit256) :: Posit8
  acosh x = convert (acosh (convert x) :: Posit256) :: Posit8
  atanh x = convert (atanh (convert x) :: Posit256) :: Posit8

instance Floating Posit16 where
  pi = convert (pi :: Posit256) :: Posit16
  exp x = convert (exp (convert x) :: Posit256) :: Posit16
  log x = convert (log (convert x) :: Posit256) :: Posit16
  x ** y = convert $ (convert x :: Posit256) ** (convert y :: Posit256) :: Posit16
  sin x = convert (sin (convert x) :: Posit256) :: Posit16
  cos x = convert (cos (convert x) :: Posit256) :: Posit16
  asin x = convert (asin (convert x) :: Posit256) :: Posit16
  acos x = convert (acos (convert x) :: Posit256) :: Posit16
  atan x = convert (atan (convert x) :: Posit256) :: Posit16
  sinh x = convert (sinh (convert x) :: Posit256) :: Posit16
  cosh x = convert (cosh (convert x) :: Posit256) :: Posit16
  asinh x = convert (asinh (convert x) :: Posit256) :: Posit16
  acosh x = convert (acosh (convert x) :: Posit256) :: Posit16
  atanh x = convert (atanh (convert x) :: Posit256) :: Posit16

instance Floating Posit32 where
  pi = convert (pi :: Posit256) :: Posit32
  exp x = convert (exp (convert x) :: Posit256) :: Posit32
  log x = convert (log (convert x) :: Posit256) :: Posit32
  x ** y = convert $ (convert x :: Posit256) ** (convert y :: Posit256) :: Posit32
  sin x = convert (sin (convert x) :: Posit256) :: Posit32
  cos x = convert (cos (convert x) :: Posit256) :: Posit32
  asin x = convert (asin (convert x) :: Posit256) :: Posit32
  acos x = convert (acos (convert x) :: Posit256) :: Posit32
  atan x = convert (atan (convert x) :: Posit256) :: Posit32
  sinh x = convert (sinh (convert x) :: Posit256) :: Posit32
  cosh x = convert (cosh (convert x) :: Posit256) :: Posit32
  asinh x = convert (asinh (convert x) :: Posit256) :: Posit32
  acosh x = convert (acosh (convert x) :: Posit256) :: Posit32
  atanh x = convert (atanh (convert x) :: Posit256) :: Posit32

instance Floating Posit64 where
  pi = convert (pi :: Posit256) :: Posit64
  exp x = convert (exp (convert x) :: Posit256) :: Posit64
  log x = convert (log (convert x) :: Posit256) :: Posit64
  x ** y = convert $ (convert x :: Posit256) ** (convert y :: Posit256) :: Posit64
  sin x = convert (sin (convert x) :: Posit256) :: Posit64
  cos x = convert (cos (convert x) :: Posit256) :: Posit64
  asin x = convert (asin (convert x) :: Posit256) :: Posit64
  acos x = convert (acos (convert x) :: Posit256) :: Posit64
  atan x = convert (atan (convert x) :: Posit256) :: Posit64
  sinh x = convert (sinh (convert x) :: Posit256) :: Posit64
  cosh x = convert (cosh (convert x) :: Posit256) :: Posit64
  asinh x = convert (asinh (convert x) :: Posit256) :: Posit64
  acosh x = convert (acosh (convert x) :: Posit256) :: Posit64
  atanh x = convert (atanh (convert x) :: Posit256) :: Posit64

instance Floating Posit128 where
  pi = convert (pi :: Posit256) :: Posit128
  exp x = convert (exp (convert x) :: Posit256) :: Posit128
  log x = convert (log (convert x) :: Posit256) :: Posit128
  x ** y = convert $ (convert x :: Posit256) ** (convert y :: Posit256) :: Posit128
  sin x = convert (sin (convert x) :: Posit256) :: Posit128
  cos x = convert (cos (convert x) :: Posit256) :: Posit128
  asin x = convert (asin (convert x) :: Posit256) :: Posit128
  acos x = convert (acos (convert x) :: Posit256) :: Posit128
  atan x = convert (atan (convert x) :: Posit256) :: Posit128
  sinh x = convert (sinh (convert x) :: Posit256) :: Posit128
  cosh x = convert (cosh (convert x) :: Posit256) :: Posit128
  asinh x = convert (asinh (convert x) :: Posit256) :: Posit128
  acosh x = convert (acosh (convert x) :: Posit256) :: Posit128
  atanh x = convert (atanh (convert x) :: Posit256) :: Posit128

instance Floating Posit256 where
  pi = 3.141592653589793238462643383279502884197169399375105820974944592307816406286 :: Posit256
  exp = funExp
  log = funLogDomainReduction funLogTaylor
  (**) = funPow
  sin = funSin
  cos = funCos
  asin = funAsin
  acos = funAcos
  atan = funAtan
  sinh = funSinh
  cosh = funCosh
  asinh = funAsinh
  acosh = funAcosh
  atanh = funAtanh





class AltFloating p where
  phi :: p
  gamma :: p -> p
  sinc :: p -> p
  expm1 :: p -> p

instance AltFloating Posit8 where
  phi = convert (phi :: Posit256) :: Posit8
  gamma x = convert (gamma (convert x) :: Posit256) :: Posit8
  sinc x = convert (sinc (convert x) :: Posit256) :: Posit8
  expm1 x =
    let b = atanh $ x / 2
    in (2 * b) / (1 - b)

instance AltFloating Posit16 where
  phi = convert (phi :: Posit256) :: Posit16
  gamma x = convert (gamma (convert x) :: Posit256) :: Posit16
  sinc x = convert (sinc (convert x) :: Posit256) :: Posit16
  expm1 x =
    let b = atanh $ x / 2
    in (2 * b) / (1 - b)

instance AltFloating Posit32 where
  phi = convert (phi :: Posit256) :: Posit32
  gamma x = convert (gamma (convert x) :: Posit256) :: Posit32
  sinc x = convert (sinc (convert x) :: Posit256) :: Posit32
  expm1 x =
    let b = atanh $ x / 2
    in (2 * b) / (1 - b)

instance AltFloating Posit64 where
  phi = convert (phi :: Posit256) :: Posit64
  gamma x = convert (gamma (convert x) :: Posit256) :: Posit64
  sinc x = convert (sinc (convert x) :: Posit256) :: Posit64
  expm1 x =
    let b = atanh $ x / 2
    in (2 * b) / (1 - b)

instance AltFloating Posit128 where
  phi = convert (phi :: Posit256) :: Posit128
  gamma x = convert (gamma (convert x) :: Posit256) :: Posit128
  sinc x = convert (sinc (convert x) :: Posit256) :: Posit128
  expm1 x =
    let b = atanh $ x / 2
    in (2 * b) / (1 - b)

instance AltFloating Posit256 where
  phi = funPhi 1.6
  gamma = funGammaSeries
  sinc = funSinc
  expm1 x =
    let b = atanh $ x / 2
    in (2 * b) / (1 - b)


-- | 'phi' fixed point recursive algorithm,
funPhi :: Posit256 -> Posit256
funPhi  px@(Posit x)
    | x == x' = Posit x
    | otherwise = funPhi (Posit x')
      where
        (Posit x') = (px^2 + 2*px) / (px^2 + 1)
        -- LiquidHaskell is telling me this is unsafe if px is imaginary
        -- lucky for us Posit256 is not imaginary


-- calculate atan(1/2^n)
-- sum k=0 to k=inf of the terms, iterate until a fixed point is reached
funArcTan :: Natural -> Posit256
funArcTan 0 = pi / 4
funArcTan n
  | n <= 122 = go 0 0
  | otherwise = z  -- at small z... (atan z) == z "small angle approximation"
    where
      go !k !acc
        | acc == (acc + term k) = acc
        | otherwise = go (k+1) (acc + term k)
      term :: Integer -> Posit256
      term k = ((-1)^k * z^(2 * k + 1)) / fromIntegral (2 * k + 1)
      z = 1 / 2^n  -- recip $ 2^n :: Posit256 -- inv2PowN

-- seems pretty close to 1 ULP with the input of 0.7813
funAtan :: Posit256 -> Posit256
funAtan NaR = NaR
funAtan x
  | abs x < 1/2^122 = x  -- small angle approximaiton, found emperically
  | x < 0 = negate.funAtan $ negate x  -- if negative turn it positive, it reduces the other domain reductions by half, found from Universal CORDIC
  | x > 1 = pi/2 - funAtan (recip x)  -- if larger than one use the complementary angle, found from Universal CORDIC
  | x > twoMsqrt3 = pi/6 + funAtan ((sqrt 3 * x - 1)/(sqrt 3 + x))  -- another domain reduction, using an identity, found from https://mathonweb.com/help_ebook/html/algorithms.htm
  | otherwise = funArcTanTaylor x
--

twoMsqrt3 :: Posit256
twoMsqrt3 = 2 - sqrt 3

--
funArcTanTaylor :: Posit256 -> Posit256
funArcTanTaylor x = go 0 0
  where
    go !k !acc
      | acc == (acc + term k) = acc
      | otherwise = go (k+1) (acc + term k)
    term :: Integer -> Posit256
    term k = ((-1)^k * x^(2 * k + 1)) / fromIntegral (2 * k + 1)
--

--
funAsin :: Posit256 -> Posit256
funAsin NaR = NaR
funAsin x
  | abs x > 1 = NaR
  | x == 1 = pi/2
  | x == -1 = -pi/2
  | otherwise = funAtan w
    where
      w = x / sqrt (1 - x^2)
--

--
funAcos :: Posit256 -> Posit256
funAcos NaR = NaR
funAcos x
  | abs x > 1 = NaR
  | x < 0 = pi + funAtan invw
  | x == 0 = pi/2
  | x > 0 = funAtan invw
  | otherwise = error "Prove it covers for Rational Numbers."
    where
      invw = sqrt (1 - x^2) / x
--

-- fI2PN = (1 /) . (2 ^)
funInv2PowN :: Natural -> Posit256
funInv2PowN n = 1 / 2^n


-- calculate atanh(1/2^n)
-- sum k=0 to k=inf of the terms, iterate until a fixed point is reached
funArcHypTan :: Natural -> Posit256
funArcHypTan 0 = NaR
funArcHypTan n
  | n <= 122 = go 0 0
  | otherwise = z  -- at small z... (atan z) == z "small angle approximation"
    where
      go !k !acc
        | acc == (acc + term k) = acc
        | otherwise = go (k+1) (acc + term k)
      term :: Integer -> Posit256
      term k = (z^(2 * k + 1)) / fromIntegral (2 * k + 1)
      z = 1 / 2^n


fac :: Natural -> Natural
fac 0 = 1
fac n = n * fac (n - 1)

--
funAsinh :: Posit256 -> Posit256
funAsinh NaR = NaR
funAsinh x = log $ x + sqrt (x^2 + 1)
--

--
funAcosh :: Posit256 -> Posit256
funAcosh NaR = NaR
funAcosh x
  | x < 1 = NaR
  | otherwise = log $ x + sqrt (x^2 - 1)
--

--
funAtanh :: Posit256 -> Posit256
funAtanh NaR = NaR
funAtanh x
  | abs x >= 1 = NaR
  | x < 0 = negate.funAtanh.negate $ x  -- make use of odd parity to only calculate the positive part
  | otherwise = 0.5 * log ((1+t) / (1-t)) - (fromIntegral ex / 2) * lnOf2
    where
      (ex, sig) = (int * fromIntegral (nBytes @V) + fromIntegral nat + 1, fromRational rat / 2)
      (_,int,nat,rat) = (posit2TupPosit @V).toRational $ x' -- sign should always be positive
      x' = 1 - x
      t = (2 - sig - x') / (2 + sig - x')
--

--
funAtanhTaylor :: Posit256 -> Posit256
funAtanhTaylor NaR = NaR
funAtanhTaylor x
  | abs x >= 1 = NaR
  | abs x < 1/2^122 = x  -- small angle approximaiton, found emperically
  | x < 0 = negate.funAtanhTaylor.negate $ x
  | otherwise = go 0 0
    where
      go !k !acc
        | acc == (acc + term k) = acc
        | otherwise = go (k+1) (acc + term k)
      term :: Integer -> Posit256
      term k = (x^(2 * k + 1)) / fromIntegral (2 * k + 1)
--

--
funSin :: Posit256 -> Posit256
funSin NaR = NaR
funSin 0 = 0
funSin x = funSin' $ x / (2*pi)
--
-- funSin' is sine normalized by 2*pi
funSin' :: Posit256 -> Posit256
funSin' x
  | x == 0 = 0
  | x == 0.25 = 1
  | x == 0.5 = 0
  | x == 0.75 = -1
  | x == 1 = 0
  | x < 0 = negate.funSin'.negate $ x
  | x > 1 =
    let (_,rem) = properFraction x
    in funSin' rem
  | x > 0.75 && x < 1 = negate.funSin' $ 1 - x -- reduce domain by quadrant symmetry
  | x > 0.5 && x < 0.75 = negate.funSin' $ x - 0.5
  | x > 0.25 && x < 0.5 = funSin' $ 0.5 - x
  | x > 0.125 && x < 0.25 = funCosTuma $ 2*pi * (0.25 - x) -- reduce domain and use cofunction
  | otherwise = funSinTuma $ 2*pi * x
--

-- Taylor series expansion and fixed point algorithm, most accurate near zero
funSinTaylor :: Posit256 -> Posit256
funSinTaylor NaR = NaR
funSinTaylor z = go 0 0
  where
    go :: Natural -> Posit256 -> Posit256
    go !k !acc
      | acc == (acc + term k) = acc
      | otherwise = go (k+1) (acc + term k)
    term :: Natural -> Posit256
    term k = (-1)^k * z^(2*k+1) / (fromIntegral.fac $ 2*k+1)
--

--
funSinTuma :: Posit256 -> Posit256
funSinTuma NaR = NaR
funSinTuma z = go 19 1
  where
    go :: Natural -> Posit256 -> Posit256
    go 1 !acc = z * acc
    go !k !acc = go (k-1) (1 - (z^2 / fromIntegral ((2*k-2)*(2*k-1))) * acc)
--

--
funCos :: Posit256 -> Posit256
funCos NaR = NaR
funCos 0 = 1
funCos x = funCos' $ x / (2*pi)
--
-- funCos' is cosine normalized for 2*pi
funCos' :: Posit256 -> Posit256
funCos' NaR = NaR
funCos' x
  | x == 0 = 1
  | x == 0.25 = 0
  | x == 0.5 = -1
  | x == 0.75 = 0
  | x == 1 = 1
  | x < 0 = funCos'.negate $ x  -- reduce domain by symmetry across 0 to turn x positive
  | x > 1 = -- reduce domain by using perodicity
    let (_,rem) = properFraction x
    in funCos' rem
  | x > 0.75 && x < 1 = funCos' $ 1 - x  -- reduce domain by quadrant symmetry
  | x > 0.5 && x < 0.75 = negate.funCos' $ x - 0.5
  | x > 0.25 && x < 0.5 = negate.funCos' $ 0.5 - x
  | x > 0.125 && x < 0.25 = funSinTuma $ 2*pi * (0.25 - x) -- reduce domain and use cofunction
  | otherwise = funCosTuma $ 2*pi * x --
--

-- Taylor series expansion and fixed point algorithm, most accurate near zero
funCosTaylor :: Posit256 -> Posit256
funCosTaylor NaR = NaR
funCosTaylor z = go 0 0
  where
    go :: Natural -> Posit256 -> Posit256
    go !k !acc
      | acc == (acc + term k) = acc
      | otherwise = go (k+1) (acc + term k)
    term :: Natural -> Posit256
    term k = (-1)^k * z^(2*k) / (fromIntegral.fac $ 2*k)
--

--
funCosTuma :: Posit256 -> Posit256
funCosTuma NaR = NaR
funCosTuma z = go 19 1
  where
    go :: Natural -> Posit256 -> Posit256
    go 1 !acc = acc
    go !k !acc = go (k-1) (1 - (z^2 / fromIntegral ((2*k-3)*(2*k-2))) * acc)
--

-- ~16 ULP for 42
funSinh :: Posit256 -> Posit256
funSinh NaR = NaR
funSinh x = (exp x - exp (negate x))/2
--

-- ~2 ULP for 42
funSinhTaylor :: Posit256 -> Posit256
funSinhTaylor NaR = NaR
funSinhTaylor z = go 0 0
  where
    go :: Natural -> Posit256 -> Posit256
    go !k !acc
      | acc == (acc + term k) = acc
      | otherwise = go (k+1) (acc + term k)
    term :: Natural -> Posit256
    term k = z^(2*k+1) / (fromIntegral.fac $ 2*k+1)
--

--
funSinhTuma :: Posit256 -> Posit256
funSinhTuma NaR = NaR
funSinhTuma 0 = 0
funSinhTuma z | z < 0 = negate.funSinhTuma.negate $ z
funSinhTuma z | z > 80 = 0.5 * funExpTuma z
funSinhTuma z = go 256 1
  where
    go :: Natural -> Posit256 -> Posit256
    go 1 !acc = z * acc
    go !k !acc = go (k-1) (1 + (z^2 / fromIntegral ((2*k-2) * (2*k-1))) * acc)
--

-- ~17 ULP for 42
funCosh :: Posit256 -> Posit256
funCosh NaR = NaR
funCosh x = (exp x + exp (negate x))/2
--

-- ~3 ULP for 42
funCoshTaylor :: Posit256 -> Posit256
funCoshTaylor NaR = NaR
funCoshTaylor z = go 0 0
  where
    go :: Natural -> Posit256 -> Posit256
    go !k !acc
      | acc == (acc + term k) = acc
      | otherwise = go (k+1) (acc + term k)
    term :: Natural -> Posit256
    term k = z^(2*k) / (fromIntegral.fac $ 2*k)
--

--
funCoshTuma :: Posit256 -> Posit256
funCoshTuma NaR = NaR
funCoshTuma 0 = 1
funCoshTuma z | z < 0 = funCoshTuma.negate $ z
funCoshTuma z | z > 3 = 0.5 * (funExpTuma z + funExpTuma (negate z))
funCoshTuma z = go 20 1
  where
    go :: Natural -> Posit256 -> Posit256
    go 1 !acc = acc
    go !k !acc = go (k-1) (1 + (z^2 / fromIntegral ((2*k-3)*(2*k-2)))*acc)
--


--
funLog :: Posit256 -> Posit256
funLog x = funLog2 x * lnOf2
--

--
-- Use the constant, for performance
lnOf2 :: Posit256
lnOf2 = Posit 28670435363615573179632300308403400109260626501925370561166468529302554498548
--

--
-- Some series don't converge reliably, this one does
funLnOf2 :: Posit256
funLnOf2 = go 1 0
  where
    go :: Natural -> Posit256 -> Posit256
    go !k !acc
      | acc == (acc + term k) = acc
      | otherwise = go (k+1) (acc + term k)
    term :: Natural -> Posit256
    term k = 1 / fromIntegral (2^k * k)
--

--
funLog2 :: Posit256 -> Posit256
funLog2 NaR = NaR
funLog2 z
  | z <= 0 = NaR -- includes the NaR case
  | otherwise = go (fromInteger ex) 1 sig  -- domain reduction
    where
      go :: Posit256 -> Posit256 -> Posit256 -> Posit256
      go !acc !mak !sig' -- fixed point iteration, y is [1,2) :: Posit256
        | sig == 1 = acc
        | acc == (acc + mak * 2^^(negate.fst.term $ sig')) = acc  -- stop when fixed point is reached
        | otherwise = go (acc + mak * 2^^(negate.fst.term $ sig')) (mak * 2^^(negate.fst.term $ sig')) (snd.term $ sig')
      term = findSquaring 0  -- returns (m,s') m the number of times to square, and the new significand
      (ex, sig) = (int * fromIntegral (nBytes @V) + fromIntegral nat, fromRational rat)
      (_,int,nat,rat) = (posit2TupPosit @V).toRational $ z -- sign should always be positive
      findSquaring m s
        | s >= 2 && s < 4 = (m, s/2)
        | otherwise = findSquaring (m+1) (s^2)
--


--  Gauss–Legendre algorithm, Seems only accurate to 2-3 ULP, but really slow
funPi1 :: Posit256
funPi1 = go 0 3 1 (recip.sqrt $ 2) (recip 4) 1
  where
    go :: Posit256 -> Posit256 -> Posit256 -> Posit256 -> Posit256 -> Posit256 -> Posit256
    go !prev !next !a !b !t !p
      | prev == next = next
      | otherwise =
        let a' = (a + b) / 2
            b' = sqrt $ a * b
            t' = t - p * (a - ((a + b) / 2))^2
            p' = 2 * p
        in go next ((a' + b')^2 / (4 * t')) a' b' t' p'
--

#ifndef O_NO_SHOW
--  Borwein's algorithm, with quintic convergence,
--  gets to 7 ULP in 4 iterations, but really slow due to expensive function evaluations
--  quite unstable and will not converge if sqrt is not accurate, which means log must be accurate
funPi2 :: Posit256
funPi2 = recip $ go 0 0 0.5 (5 / phi^3)
  where
    go :: Posit256 -> Natural -> Posit256 -> Posit256 -> Posit256
    go !prev !n !a !s
      | prev == a = a
      | otherwise =
        let x = 5 / s - 1
            y = (x - 1)^2 + 7
            z = (0.5 * x * (y + sqrt (y^2 - 4 * x^3)))**(1/5)
            a' = s^2 * a - (5^n * ((s^2 - 5)/2 + sqrt (s * (s^2 - 2*s + 5))))
            s' = 25 / ((z + x/z + 1)^2 * s)
        in go a (n+1) (trace (show a') a') s'
--
#endif


-- Bailey–Borwein–Plouffe (BBP) formula, to 1-2 ULP, and blazing fast, converges in 60 iterations
funPi3 :: Posit256
funPi3 = go 0 0
  where
    go :: Integer -> Posit256 -> Posit256
    go !k !acc
      | acc == acc + term k = acc
      | otherwise = go (k+1) (acc + term k)
    term :: Integer -> Posit256
    term k = fromRational $ (1 % 16^k) * ((120 * k^2 + 151 * k + 47) % (512 * k^4 + 1024 * k^3 + 712 * k^2 + 194 * k + 15))
--


-- Fabrice Bellard improvement on the BBP, 2-3 ULP, even faster, converges in 25 iterations, really fast
funPi4 :: Posit256
funPi4 = (1/2^6) * go 0 0
  where
    go :: Integer -> Posit256 -> Posit256
    go !k !acc
      | acc == acc + term k = acc
      | otherwise = go (k+1) (acc + term k)
    term :: Integer -> Posit256
    term k = fromRational $ ((-1)^k % (2^(10*k))) * ((1 % (10 * k + 9)) - (2^2 % (10 * k + 7)) - (2^2 % (10 * k + 5)) - (2^6 % (10 * k + 3)) + (2^8 % (10 * k + 1)) - (1 % (4 * k + 3)) - (2^5 % (4 * k + 1)))
--



--
-- looks to be about 4 ULP accurate at -100, right on the money at -1000
funExp :: Posit256 -> Posit256
funExp x = funExp2 funExpTaylor (x / lnOf2)
--

--
--
funExp2 :: (Posit256 -> Posit256) -> Posit256 -> Posit256
funExp2 _ NaR = NaR
funExp2 _ 0 = 1
funExp2 f x
  | x < 0 = recip.funExp2 f.negate $ x  -- always calculate the positive method
  | otherwise = case properFraction x of
                  (int,rem) -> fromIntegral (2^int) * f (lnOf2 * rem)



--
-- calculate exp, its most accurate near zero
-- sum k=0 to k=inf of the terms, iterate until a fixed point is reached
funExpTaylor :: Posit256 -> Posit256
funExpTaylor NaR = NaR
funExpTaylor 0 = 1
funExpTaylor z = go 0 0
  where
    go :: Natural -> Posit256 -> Posit256
    go !k !acc
      | acc == (acc + term k) = acc  -- if x == x + dx then terminate and return x
      | otherwise = go (k+1) (acc + term k)
    term :: Natural -> Posit256
    term k = (z^k) / (fromIntegral.fac $ k)
--

--
-- calculate exp, its most accurate near zero
-- use the Nested Series of Jan J Tuma
funExpTuma :: Posit256 -> Posit256
funExpTuma NaR = NaR
funExpTuma 0 = 1
funExpTuma z = go 57 1 -- was 66
  where
    go :: Natural -> Posit256 -> Posit256
    go !k !acc
      | k == 0 = acc
      | otherwise = go (k-1) (1 + (z / fromIntegral k) * acc)
--

--
--
funPow :: Posit256 -> Posit256 -> Posit256
NaR `funPow` _ = NaR
_ `funPow` NaR = NaR
funPow 0 y
  | y < 0 = NaR -- NaR: Divide by Zero
  | y == 0 = NaR -- NaR: Indeterminate
  | y > 0 = 0
funPow x y
  | y < 0 = recip $ funPow x (negate y)
  | x < 0 = -- NaR if y is not an integer
    let (int,rem) = properFraction y
    in if rem == 0
       then x^^int
       else NaR -- NaR: Imaginary Number
  | otherwise = exp $ y * log x
--

-- Looks like 1 ULP for 0.7813
funSinc :: Posit256 -> Posit256
funSinc NaR = NaR
funSinc 0 = 1  -- Why the hell not!
funSinc theta = sin theta / theta
--

-- Interestingly enough, wikipedia defines two alternative solutions
-- for the Shannon Wavelet, eventhough there are infinite solutions
-- where the functions are equal, they are not equal.  It a class of 
-- functions with the charicteristic of being a band pass filter in the 
-- frequency space.
-- Shannon wavelet
funPsiSha1 :: Posit256 -> Posit256
funPsiSha1 NaR = NaR
funPsiSha1 t = 2 * funSinc (2 * t) - funSinc t
--

-- Shannon wavelet
funPsiSha2 :: Posit256 -> Posit256
funPsiSha2 NaR = NaR
funPsiSha2 t = funSinc (t/2) * cos (3*pi*t/2)
--

-- Shannon wavelet, same as funPsiSha1 but with a factor of pi, with the
-- Law: funPsiSha1.(pi*) === funPsiSha3
-- or : funPsiSha1 === funpsiSha3.(/pi)
-- Posit256 seems to hold to a few ULP
funPsiSha3 :: Posit256 -> Posit256
funPsiSha3 NaR = NaR
funPsiSha3 0 = 1  -- Why the hell not!
funPsiSha3 t =
  let pit = pi * t
      invpit = recip pit 
  in invpit * (sin (2 * pit) - sin pit)
--



-- Using the CORDIC domain reduction and some approximation function
funLogDomainReduction :: (Posit256 -> Posit256) -> Posit256 -> Posit256
funLogDomainReduction _ NaR = NaR
funLogDomainReduction _ 1 = 0
funLogDomainReduction f x
  | x <= 0 = NaR
  | otherwise = f sig + (fromIntegral ex * lnOf2)
    where
      (ex, sig) = (int * fromIntegral (nBytes @V) + fromIntegral nat + 1, fromRational rat / 2) -- move significand range from 1,2 to 0.5,1
      (_,int,nat,rat) = (posit2TupPosit @V).toRational $ x -- sign should always be positive
     
 

-- natural log with log phi acurate to 9 ULP
funLogTaylor :: Posit256 -> Posit256
funLogTaylor NaR = NaR
funLogTaylor 1 = 0
funLogTaylor x | x <= 0 = NaR
funLogTaylor x
  | x <= 2 = go 1 0
  | otherwise = error "The funLogTaylor algorithm is being used improperly"
    where
      go :: Natural -> Posit256 -> Posit256
      go !k !acc
        | acc == (acc + term k) = acc
        | otherwise = go (k + 1) (acc + term k)
      term :: Natural -> Posit256
      term k = (-1)^(k+1) * (x - 1)^k / fromIntegral k
     



-- natural log the Jan J Tuma way
funLogTuma :: Posit256 -> Posit256
funLogTuma NaR = NaR
funLogTuma 1 = 0  -- domain reduced input is [0.5,1) and/or , where funLogTuma 1 = 0
funLogTuma x | x <= 0 = NaR  -- zero and less than zero is NaR
funLogTuma x
  = go 242 1
    where
      xM1 = x - 1  -- now [-0.5, 0)
      go :: Natural -> Posit256 -> Posit256
      go !k !acc
        | k == 0 = xM1 * acc
        | otherwise = go (k-1) (recip (fromIntegral k) - xM1 * acc)


funGammaRamanujan :: Posit256 -> Posit256
funGammaRamanujan z = sqrt pi * (x / exp 1)**x * (8*x^3 + 4*x^2 + x + (1/30))**(1/6)
  where
    x = z - 1

--
a001163 :: [Integer] -- Numerator
a001163 = [1, 1, -139, -571, 163879, 5246819, -534703531, -4483131259, 432261921612371, 6232523202521089, -25834629665134204969, -1579029138854919086429, 746590869962651602203151, 1511513601028097903631961, -8849272268392873147705987190261, -142801712490607530608130701097701]
a001164 :: [Integer]  -- Denominator
a001164 = [12, 288, 51840, 2488320, 209018880, 75246796800, 902961561600, 86684309913600, 514904800886784000, 86504006548979712000, 13494625021640835072000, 9716130015581401251840000, 116593560186976815022080000, 2798245444487443560529920000, 299692087104605205332754432000000, 57540880724084199423888850944000000]

funGammaSeries :: Posit256 -> Posit256
funGammaSeries z = sqrt(2 * pi) * (z**(z - 0.5)) * exp (negate z) * (1 + series)
  where
    series :: Posit256
    series = sum $ zipWith (*) [fromRational (a % b) | (a,b) <- zip a001163 a001164] [recip $ z^n |  n <- [1..len]]  -- zipWith (\x y -> ) a001163 a001164
    lenA = length a001163
    lenB = length a001164
    len = if lenA == lenB
            then lenA
            else error "Seiries Numerator and Denominator do not have the same length."

funGammaSeriesFused :: Posit256 -> Posit256
funGammaSeriesFused z = sqrt(2 * pi) * (z**(z - 0.5)) * exp (negate z) * (1 + series)
  where
    series :: Posit256
    series = fsumL $ zipWith (*) [fromRational (a % b) | (a,b) <- zip a001163 a001164] [recip $ z^n |  n <- [1..len]]  -- zipWith (\x y -> ) a001163 a001164
    lenA = length a001163
    lenB = length a001164
    len = if lenA == lenB
            then lenA
            else error "Seiries Numerator and Denominator do not have the same length."
--

funGammaCalc :: Posit256 -> Posit256
funGammaCalc z = sqrt (2*pi / z) * ((z / exp 1) * sqrt (z * sinh (recip z) + recip (810 * z^6)))**z


funGammaNemes :: Posit256 -> Posit256
funGammaNemes z = sqrt (2*pi / z) * (recip (exp 1) * (z + recip (12 * z - recip (10 * z))))**z

funGammaYang :: Posit256 -> Posit256
funGammaYang z = sqrt (2 * pi * x) * (x / exp 1)**x * (x * sinh (recip x))**(x/2) * exp (fromRational (7 % 324) * recip (x^3 * (35 * x^2 + 33)))
  where
    x = z - 1

funGammaChen :: Posit256 -> Posit256
funGammaChen z = sqrt (2 * pi * x) * (x / exp 1)**x * (1 + recip (12*x^3 + (24/7)*x - 0.5))**(x^2 + fromRational (53 % 210))
  where
    x = z - 1

funGammaXminus1 :: Posit256 -> Posit256
funGammaXminus1 x = go (x - 1)
  where
    go z = sqrt (2 * pi) * exp z ** (negate z) * z ** (z + 0.5)


--------------------------------------------------------------------------------------------
--   Posit Numbers
--   Copyright   :  (C) 2022-2023 Nathan Waivio
--   License     :  BSD3
--   Maintainer  :  Nathan Waivio <nathan.waivio@gmail.com>
--   Stability   :  Stable
--   Portability :  Portable
--
-- | Library implementing standard Posit Numbers both Posit Standard version
--   3.2 and 2022, with some improvements.  Posit is the interface, PositC 
--   provides the implemetation.  2's Complement Fixed Point Integers,
--   and Rational numbers, are used throughout, as well as Integers & Naturals.
--   Encode and Decode are indexed through a Type Family.
-- 
---------------------------------------------------------------------------------------------


{-# LANGUAGE GADTs #-} --   For our main type Posit (es :: ES)
{-# LANGUAGE DataKinds #-}  --   For our ES kind and the constructors Z, I, II, III, IV, V for exponent size type, post-pended with the version.
{-# LANGUAGE KindSignatures #-}  --   For defining the type of kind ES that indexes the GADT
{-# LANGUAGE ViewPatterns #-}  --   To decode the posit in the pattern
{-# LANGUAGE BangPatterns #-}  --   Added Strictness for some fixed point algorithms
{-# LANGUAGE PatternSynonyms #-}  --   for a nice NaR interface
{-# LANGUAGE FlexibleInstances #-} --   To make instances for each specific type [Posit8 .. Posit256], and [P8 .. P256]
{-# LANGUAGE FlexibleContexts #-} --   If anybody knows what's this for let me know...
{-# LANGUAGE TypeApplications #-} --   To apply types: @Type, it seems to select the specific class instance, when GHC is not able to reason about things, commenting this out shows an interesting interface
{-# LANGUAGE MultiParamTypeClasses #-}  --   To convert between Posit Types, via Rational
{-# LANGUAGE ScopedTypeVariables #-} --   To reduce some code duplication, this is important
{-# LANGUAGE UndecidableInstances #-}  --   To reduce some code duplication, I think the code is decidable but GHC is not smart enough ;), like there being only 1 instance that is polymorphic and works for all of my types.
{-# LANGUAGE CPP #-} --   To remove Storable instances to remove noise when performing analysis of Core
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}  --   Turn off noise
{-# OPTIONS_GHC -Wno-type-defaults #-}  --   Turn off noise
{-# OPTIONS_GHC -Wno-unused-top-binds #-}  --   Turn off noise
{-# LANGUAGE AllowAmbiguousTypes #-}

-- ----
--  Posit numbers implementing:
--
--    * Show
--    * Eq  -- equality via an integer representation
--    * Ord  -- compare via an integer representation
--    * Num  -- Addition, subtraction, multiplication, and other operations most via Rational, negate is via an integer representation
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
 Posit8, -- |A Posit-3.2 8-bit Posit number with 'exponentSize' = '0', and 1 byte wide
 Posit16, -- |A Posit-3.2 16-bit Posit number with 'exponentSize' = '1', and 2 bytes wide
 Posit32, -- |A Posit-3.2 32-bit Posit number with 'exponentSize' = '2', and 4 bytes wide
 Posit64, -- |A Posit-3.2 64-bit Posit number with 'exponentSize' = '3', and 8 bytes wide
 Posit128, -- |A Posit-3.2 128-bit Posit number with 'exponentSize' = '4', and 16 bytes wide
 Posit256, -- |A Posit-3.2 256-bit Posit number with 'exponentSize' = '5', and 32 bytes wide
 P8, -- |A Posit-2022 8-bit Posit number with 'exponentSize' = '2', and 1 byte wide
 P16, -- |A Posit-2022 16-bit Posit number with 'exponentSize' = '2', and 2 bytes wide
 P32, -- |A Posit-2022 32-bit Posit number with 'exponentSize' = '2', and 4 bytes wide
 P64, -- |A Posit-2022 64-bit Posit number with 'exponentSize' = '2', and 8 bytes wide
 P128, -- |A Posit-2022 128-bit Posit number with 'exponentSize' = '2', and 16 bytes wide
 P256, -- |A Posit-2022 256-bit Posit number with 'exponentSize' = '2', and 32 bytes wide
 
 -- * A Complete Pair of Patterns for Matching Exported Types
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
 viaRational8
 
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

#ifndef O_NO_STORABLE_RANDOM
-- Imports for Storable Instance
import Foreign.Storable (Storable, sizeOf, alignment, peek, poke)  -- Used for Storable Instances of Posit
import Foreign.Ptr (Ptr, castPtr)  -- Used for dealing with Pointers for the Posit Storable Instance


import System.Random (Random(random,randomR))
import System.Random.Stateful (Uniform, uniform, uniformM)

import Data.Bits (shiftL, (.&.), (.|.))
#endif

-- would like to:
-- import Posit.Internal.ElementaryFunctions
-- Perhaps on the chopping block if we are moving to ElementaryFunctions
-- Imports for implementing the Transcendental Functions
import GHC.Natural (Natural) -- Import the Natural Numbers ℕ (u+2115) for some of the Transcendental Functions
import Data.Ratio ((%))  -- Import the Rational Numbers ℚ (u+211A), ℚ can get arbitrarily close to Real numbers ℝ (u+211D), used for some of the Transcendental Functions

-- for NFData instance
import Control.DeepSeq (NFData, rnf)

-- import Debug.Trace (trace) -- temporary for debug purposes


-- =====================================================================
-- ===                  Posit Implementation                         ===
-- =====================================================================

-- The machine implementation of the Posit encoding/decoding
import Posit.Internal.PositC  -- The main internal implementation details


-- |Base GADT rapper type, that uses the Exponent Size kind to index the various implementations
data Posit (es :: ES) where
     Posit :: PositC es => !(IntN es) -> Posit es

-- |NFData Instance
instance NFData (Posit es) where
  rnf (Posit _) = ()

-- |Not a Real Number, the Posit is like a Maybe type, it's either a real number or not
pattern NaR :: forall es. PositC es => Posit es
pattern NaR <- (Posit (decode @es -> Nothing)) where
  NaR = Posit (unReal @es)
--

--
-- |A Real or at least Rational Number, rounded to the nearest Posit Rational representation
pattern R :: forall es. PositC es => Rational -> Posit es
pattern R r <- (Posit (decode @es -> Just r)) where
  R r = Posit (encode @es $ Just r)
--

-- Posit functions are complete if the following two patterns are completely defined.
{-# COMPLETE NaR, R #-}

-- Concrete 3.2 types exported for use.
type Posit8 = Posit Z_3_2
type Posit16 = Posit I_3_2
type Posit32 = Posit II_3_2
type Posit64 = Posit III_3_2
type Posit128 = Posit IV_3_2
type Posit256 = Posit V_3_2

-- Concrete 2022 types exported for use.
type P8 = Posit Z_2022
type P16 = Posit I_2022
type P32 = Posit II_2022
type P64 = Posit III_2022
type P128 = Posit IV_2022
type P256 = Posit V_2022

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
  fromInteger int = R $ fromInteger int
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
  -- succ (Posit int) = Posit (int + 1)  -- Successor
  succ = viaIntegral (+1)  -- Posit Standard `next`
  -- succ = viaIntegral succ  -- Non-compliant, runtime error pred NaR, and worse it is Int64 for types of greater precision, probably because of Preludes gross abomination of toEnum/fromEnum
  -- pred (Posit int) = Posit (int - 1)  -- Predicessor
  pred = viaIntegral (subtract 1)  -- Posit Standard `prior`
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
  minBound = Posit (mostNegVal @es)
  -- 'maxBound' the most positive number represented
  maxBound = Posit (mostPosVal @es)
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
  fsumL (toList -> l) = Posit $ encode @es (Just $ go l 0)
    where
      go :: [Posit es] -> Rational -> Rational
      go [] !acc = acc
      go ((Posit int) : xs) !acc = case decode @es int of
                                     Nothing -> error "Posit List contains NaR"
                                     Just r -> go xs (acc + r)
  -- Fuse Dot Product of a 3-Vector
  fdot3 = viaRational6 fdot3
  -- Fuse Dot Product of a 4-Vector
  fdot4 = viaRational8 fdot4
  -- Fuse Dot Product of two Lists
  fdotL (toList -> l1) (toList -> l2) = Posit $ encode @es (Just $ go l1 l2 0)
    where
      go [] [] !acc = acc
      go []  _   _  = error "Lists not the same length"
      go _  []   _  = error "Lists not the same length"
      go ((Posit int1) : bs) ((Posit int2) : cs) !acc = case decode @es int1 of
                                                          Nothing -> error "First Posit List contains NaR"
                                                          Just r1 -> case decode @es int2 of
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
  displayBinary (Posit int) = displayBin @es int
 
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


#ifndef O_NO_STORABLE_RANDOM
-- =====================================================================
-- ===                  Storable Instances                           ===
-- =====================================================================
--
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

-- | Random instance for the Posit Sampling of R [0,1), this is for the
-- real numbers, not on the projective real numbers, for projective
-- real numbers, use Uniform.
instance forall es. PositC es => Random (Posit es) where
-- First we take a uniform distributed random posit, then we mask out
-- the sign, 2 bits of regime, and the exponent, then we write in the
-- sign, regime and exponent of 1.0, to get a posit [1,2) then subtract
-- 1.0 to adjust the range to [0,1).  This approach is credited to a
-- coorispondance between Shin Yee Chung and John L. Gustafson titled:
-- "random number generators for posit" in the Unum Computing Google Group
  random g = case uniform g of
               (Posit int :: Posit es, g') -> (Posit ((int .&. maskFraction @es) .|. patt) - 1.0, g')
    where
     (Posit patt) = 1.0 :: Posit es

  randomR (lo,hi) g
    | lo > hi = randomR (hi,lo) g
    | otherwise = case random g of
                    (p,g') -> let scaled_p = (hi - lo) * p + lo
                              in (scaled_p, g')


-- | Uniform instance for the Posit Sampling of the projective real line
instance PositC es => Uniform (Posit es) where
  uniformM g = do
    int <- uniformM g
    return $ Posit int


maskFraction :: forall es. PositC es => IntN es
maskFraction =
  let twoRegimeBits = 2 -- regimeBitSize set to 2, the good range is [1,2)
      sreSize = signBitSize @es + twoRegimeBits + exponentSize @es
  in (1 `shiftL` fromIntegral (nBits @es - sreSize) - 1)

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

instance PositF es => Floating (Posit es) where
  pi = approx_pi
  exp = hiRezNext approx_exp
  log = hiRezNext approx_log
  x ** y = hiRezNext2 approx_pow x y
  sin = hiRezNext approx_sin
  cos = hiRezNext approx_cos
  asin = hiRezNext approx_asin
  acos = hiRezNext approx_acos
  atan = hiRezNext approx_atan
  sinh = hiRezNext approx_sinh
  cosh = hiRezNext approx_cosh
  asinh = hiRezNext approx_asinh
  acosh = hiRezNext approx_acosh
  atanh = hiRezNext approx_atanh



-- Functions to step up and down in Resolution of the trancendental
-- functions so that we get properly rounded results upto 128-bits
-- Note: 256-bit resolution will not have ulp accuracy
hiRezNext :: forall es. PositF es => (Posit (Next es) -> Posit (Next es)) -> Posit es -> Posit es
hiRezNext f x = convert (f (convert x) :: Posit (Next es)) :: Posit es

hiRezMax :: forall es. (PositC es, PositC (Max es)) => (Posit (Max es) -> Posit (Max es)) -> Posit es -> Posit es
hiRezMax f x = convert (f (convert x) :: Posit (Max es)) :: Posit es

hiRezNext2 :: forall es. PositF es => (Posit (Next es) -> Posit (Next es) -> Posit (Next es)) -> Posit es -> Posit es -> Posit es
hiRezNext2 f x y = convert (f (convert x :: Posit (Next es)) (convert y :: Posit (Next es)) ) :: Posit es

hiRezMax2 :: forall es. (PositC es, PositC (Max es)) => (Posit (Max es) -> Posit (Max es) -> Posit (Max es)) -> Posit es -> Posit es -> Posit es
hiRezMax2 f x y = convert (f (convert x :: Posit (Max es)) (convert y :: Posit (Max es)) ) :: Posit es


-- =====================================================================
--            Approximations of Trancendental Funcitons
-- =====================================================================

approx_pi :: PositC es => Posit es
approx_pi = 3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679821480865132823066470938446


approx_exp :: PositC es => Posit es -> Posit es     -- Comment by Abigale Emily:  xcddfffff
approx_exp x = approx_2exp taylor_approx_exp (x / lnOf2)


approx_log :: PositC es => Posit es -> Posit es
approx_log = funLogDomainReduction funLogTaylor -- lnOf2 * approx_log2 x  -- the commented out was slightly less accurate


approx_pow :: (PositC es) => Posit es -> Posit es -> Posit es
NaR `approx_pow` _ = NaR
_ `approx_pow` NaR = NaR
approx_pow 0 y
  | y < 0 = NaR -- NaR: Divide by Zero
  | y == 0 = NaR -- NaR: Indeterminate
  | y > 0 = 0
approx_pow x y
  | y < 0 = recip $ approx_pow x (negate y)
  | x < 0 = -- NaR if y is not an integer
    let (int,rem) = properFraction y
    in if rem == 0
       then x^^int
       else NaR -- NaR: Imaginary Number
  | otherwise = approx_exp $ y * approx_log x


approx_sin :: forall es. PositC es => Posit es -> Posit es
approx_sin  NaR = NaR
approx_sin 0 = 0
approx_sin x = normalizedSine $ x / (2*approx_pi)


approx_cos :: PositC es => Posit es -> Posit es
approx_cos NaR = NaR
approx_cos 0 = 1
approx_cos x = normalizedCosine $ x / (2*approx_pi)


approx_asin :: PositC es => Posit es -> Posit es
approx_asin NaR = NaR
approx_asin x
  | abs x > 1 = NaR
  | x == 1 = approx_pi/2
  | x == -1 = -approx_pi/2
  | otherwise = approx_atan w
    where
      w = x / approx_sqrt (1 - x^2)


approx_acos :: PositC es => Posit es -> Posit es
approx_acos NaR = NaR
approx_acos x
  | abs x > 1 = NaR
  | x < 0 = approx_pi + approx_atan invw
  | x == 0 = approx_pi/2
  | x > 0 = approx_atan invw
  | otherwise = error "Prove it covers for Rational Numbers."
    where
      invw = approx_sqrt (1 - x^2) / x


approx_atan :: PositC es => Posit es -> Posit es
approx_atan NaR = NaR
approx_atan x
  | abs x < 1/2^122 = x  -- small angle approximaiton, found emperically
  | x < 0 = negate.approx_atan $ negate x  -- if negative turn it positive, it reduces the other domain reductions by half, found from Universal CORDIC
  | x > 1 = approx_pi/2 - approx_atan (recip x)  -- if larger than one use the complementary angle, found from Universal CORDIC
  | x > twoMsqrt3 = approx_pi/6 + approx_atan ((approx_sqrt 3 * x - 1)/(approx_sqrt 3 + x))  -- another domain reduction, using an identity, found from https://mathonweb.com/help_ebook/html/algorithms.htm
  | otherwise = taylor_approx_atan x


approx_sinh :: PositC es => Posit es -> Posit es
approx_sinh NaR = NaR
approx_sinh x = (approx_exp x - approx_exp (negate x))/2


approx_cosh :: PositC es => Posit es -> Posit es
approx_cosh NaR = NaR
approx_cosh x = (approx_exp x + approx_exp (negate x))/2


approx_asinh :: PositC es => Posit es -> Posit es
approx_asinh NaR = NaR
approx_asinh x = approx_log $ x + approx_sqrt (x^2 + 1)


approx_acosh :: PositC es => Posit es -> Posit es
approx_acosh NaR = NaR
approx_acosh x
  | x < 1 = NaR
  | otherwise = approx_log $ x + approx_sqrt (x^2 - 1)


approx_atanh :: forall es. PositC es => Posit es -> Posit es
approx_atanh NaR = NaR
approx_atanh x
  | abs x >= 1 = NaR
  | x < 0 = negate.approx_atanh.negate $ x  -- make use of odd parity to only calculate the positive part
  | otherwise = 0.5 * approx_log ((1+t) / (1-t)) - (fromIntegral ex / 2) * lnOf2
    where
      (ex, sig) = (int * fromIntegral (2^(exponentSize @es)) + fromIntegral nat + 1, fromRational rat / 2)
      (_,int,nat,rat) = (posit2TupPosit @es).toRational $ x' -- sign should always be positive
      x' = 1 - x
      t = (2 - sig - x') / (2 + sig - x')



-- =====================================================================
--     Normalized Functions or Alternative Bases
-- =====================================================================

-- normalizedSine is sine normalized by 2*pi
normalizedSine :: PositC es => Posit es -> Posit es
normalizedSine NaR = NaR
normalizedSine x
  | x == 0 = 0
  | x == 0.25 = 1
  | x == 0.5 = 0
  | x == 0.75 = -1
  | x == 1 = 0
  | x < 0 = negate.normalizedSine.negate $ x
  | x > 1 =
    let (_,rem) = properFraction x
    in normalizedSine rem
  | x > 0.75 && x < 1 = negate.normalizedSine $ 1 - x -- reduce domain by quadrant symmetry
  | x > 0.5 && x < 0.75 = negate.normalizedSine $ x - 0.5
  | x > 0.25 && x < 0.5 = normalizedSine $ 0.5 - x
  | x > 0.125 && x < 0.25 = tuma_approx_cos $ 2*approx_pi * (0.25 - x) -- reduce domain and use cofunction
  | otherwise = tuma_approx_sin $ 2*approx_pi * x


-- normalizedCosine is cosine normalized for 2*pi
normalizedCosine :: PositC es => Posit es -> Posit es
normalizedCosine NaR = NaR
normalizedCosine x
  | x == 0 = 1
  | x == 0.25 = 0
  | x == 0.5 = -1
  | x == 0.75 = 0
  | x == 1 = 1
  | x < 0 = normalizedCosine.negate $ x  -- reduce domain by symmetry across 0 to turn x positive
  | x > 1 = -- reduce domain by using perodicity
    let (_,rem) = properFraction x
    in normalizedCosine rem
  | x > 0.75 && x < 1 = normalizedCosine $ 1 - x  -- reduce domain by quadrant symmetry
  | x > 0.5 && x < 0.75 = negate.normalizedCosine $ x - 0.5
  | x > 0.25 && x < 0.5 = negate.normalizedCosine $ 0.5 - x
  | x > 0.125 && x < 0.25 = tuma_approx_sin $ 2*approx_pi * (0.25 - x) -- reduce domain and use cofunction
  | otherwise = tuma_approx_cos $ 2*approx_pi * x --


-- Approximation of 2^x Domain Reduction
approx_2exp :: PositC es => (Posit es -> Posit es) -> Posit es -> Posit es
approx_2exp _ NaR = NaR
approx_2exp _ 0 = 1
approx_2exp f x
  | x < 0 = recip.approx_2exp f.negate $ x  -- always calculate the positive method
  | otherwise = case properFraction x of
                  (int,rem) -> fromIntegral (2^int) * f (lnOf2 * rem)




-- Using the CORDIC domain reduction and some approximation function of log
funLogDomainReduction :: forall es. PositC es => (Posit es -> Posit es) -> Posit es -> Posit es
funLogDomainReduction _ NaR = NaR
funLogDomainReduction _ 1 = 0
funLogDomainReduction f x
  | x <= 0 = NaR
  | otherwise = f sig + (fromIntegral ex * lnOf2)
    where
      (ex, sig) = (int * fromIntegral (2^(exponentSize @es)) + fromIntegral nat + 1, fromRational rat / 2) -- move significand range from 1,2 to 0.5,1
      (_,int,nat,rat) = (posit2TupPosit @es).toRational $ x -- sign should always be positive
     
 

-- natural log with log phi acurate to 9 ULP
funLogTaylor :: forall es. PositC es => Posit es -> Posit es
funLogTaylor NaR = NaR
funLogTaylor 1 = 0
funLogTaylor x | x <= 0 = NaR
funLogTaylor x
  | x <= 2 = go 1 0
  | otherwise = error "The funLogTaylor algorithm is being used improperly"
    where
      go :: Natural -> Posit es -> Posit es
      go !k !acc
        | acc == (acc + term k) = acc
        | otherwise = go (k + 1) (acc + term k)
      term :: Natural -> Posit es
      term k = (-1)^(k+1) * (x - 1)^k / fromIntegral k
     



-- =====================================================================
--       Taylor Series Fixed Point Approximations
-- =====================================================================

--
taylor_approx_atan :: forall es. PositC es => Posit es -> Posit es
taylor_approx_atan NaR = NaR
taylor_approx_atan x = go 0 0
  where
    go !k !acc
      | acc == (acc + term k) = acc
      | otherwise = go (k+1) (acc + term k)
    term :: Integer -> Posit es
    term k = ((-1)^k * x^(2 * k + 1)) / fromIntegral (2 * k + 1)
--


-- calculate exp, its most accurate near zero
-- sum k=0 to k=inf of the terms, iterate until a fixed point is reached
taylor_approx_exp :: forall es. PositC es => Posit es -> Posit es
taylor_approx_exp NaR = NaR
taylor_approx_exp 0 = 1
taylor_approx_exp z = go 0 0
  where
    go :: Natural -> Posit es -> Posit es
    go !k !acc
      | acc == (acc + term k) = acc  -- if x == x + dx then terminate and return x
      | otherwise = go (k+1) (acc + term k)
    term :: Natural -> Posit es
    term k = (z^k) / (fromIntegral.fac $ k)
--


-- =====================================================================
--  High Order Taylor Series transformed to Horner's Method
--     from Jan J Tuma's "Handbook of Numerical Calculations in Engineering" 
-- =====================================================================

--
tuma_approx_cos :: forall es. PositC es => Posit es -> Posit es
tuma_approx_cos NaR = NaR
tuma_approx_cos z = go 19 1  -- TODO can the order be selected based on the word size?
  where
    go :: Natural -> Posit es -> Posit es
    go 1 !acc = acc
    go !k !acc = go (k-1) (1 - (z^2 / fromIntegral ((2*k-3)*(2*k-2))) * acc)
--

--
tuma_approx_sin :: forall es. PositC es => Posit es -> Posit es
tuma_approx_sin NaR = NaR
tuma_approx_sin z = go 19 1  -- TODO can the order be selected based on the word size?
  where
    go :: Natural -> Posit es -> Posit es
    go 1 !acc = z * acc
    go !k !acc = go (k-1) (1 - (z^2 / fromIntegral ((2*k-2)*(2*k-1))) * acc)
--



-- =========================================================
--           Alternate Floating of a Posit es
-- =========================================================

class AltFloating p where
  eps :: p
  phi :: p
  gamma :: p -> p
  sinc :: p -> p
  expm1 :: p -> p
  hypot2 :: p -> p -> p
  hypot3 :: p -> p -> p -> p
  hypot4 :: p -> p -> p -> p -> p

--
instance PositF es => AltFloating (Posit es) where
  phi = 1.6180339887498948482045868343656381177203091798057628621354486227052604628189024497072072041893911374847540880753868917521266338   -- approx_phi 1.6
  eps = succ 1.0 - 1.0
  gamma = approx_gamma
  sinc = approx_sinc
  expm1 x =
    let b = approx_atanh $ x / 2
    in (2 * b) / (1 - b)
  hypot2 a b = let a' :: Posit (Next es) = convert a
                   b' :: Posit (Next es) = convert b
               in convert (approx_sqrt $ a'^2 + b'^2) :: Posit es
  hypot3 a b c = let a' :: Posit (Next es) = convert a
                     b' :: Posit (Next es) = convert b
                     c' :: Posit (Next es) = convert c
                 in convert (approx_sqrt $ fsum3 (a'^2) (b'^2) (c'^2)) :: Posit es
  hypot4 a b c d = let a' :: Posit (Next es) = convert a
                       b' :: Posit (Next es) = convert b
                       c' :: Posit (Next es) = convert c
                       d' :: Posit (Next es) = convert d
                   in convert (approx_sqrt $ fsum4 (a'^2) (b'^2) (c'^2) (d'^2)) :: Posit es






approx_gamma :: forall es. PositC es => Posit es -> Posit es
approx_gamma z = approx_sqrt(2 * approx_pi) * (z `approx_pow` (z - 0.5)) * approx_exp (negate z) * (1 + series)
  where
    series :: Posit es
    series = sum $ zipWith (*) [fromRational (a % b) | (a,b) <- zip a001163 a001164] [recip $ z^n |  n <- [1..len]]  -- zipWith (\x y -> ) a001163 a001164
    lenA = length a001163
    lenB = length a001164
    len = if lenA == lenB
            then lenA
            else error "Seiries Numerator and Denominator do not have the same length."
--


-- Looks like 1 ULP for 0.7813
approx_sinc :: PositC es => Posit es -> Posit es
approx_sinc NaR = NaR
approx_sinc 0 = 1  -- Why the hell not!
approx_sinc theta = approx_sin theta / theta
--



-- =====================================================================
--    Useful Constants
-- =====================================================================

--
-- Use the constant, for performance
lnOf2 :: PositC es => Posit es
lnOf2 = 0.6931471805599453094172321214581765680755001343602552541206800094933936219696947156058633269964186875420014810205706857336855202
--

--
a001163 :: [Integer] -- Numerator
a001163 = [1, 1, -139, -571, 163879, 5246819, -534703531, -4483131259, 432261921612371, 6232523202521089, -25834629665134204969, -1579029138854919086429, 746590869962651602203151, 1511513601028097903631961, -8849272268392873147705987190261, -142801712490607530608130701097701]
a001164 :: [Integer]  -- Denominator
a001164 = [12, 288, 51840, 2488320, 209018880, 75246796800, 902961561600, 86684309913600, 514904800886784000, 86504006548979712000, 13494625021640835072000, 9716130015581401251840000, 116593560186976815022080000, 2798245444487443560529920000, 299692087104605205332754432000000, 57540880724084199423888850944000000]
--

twoMsqrt3 :: PositC es => Posit es
twoMsqrt3 = 2 - approx_sqrt 3



-- =====================================================================
--    Helper Funcitons
-- =====================================================================

-- Factorial Function of type Natural
fac :: Natural -> Natural
fac 0 = 1
fac n = n * fac (n - 1)
--

approx_sqrt :: PositC es => Posit es -> Posit es
approx_sqrt x = approx_pow x 0.5




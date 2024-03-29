
--------------------------------------------------------------------------------------------
--
--   Copyright   :  (C) 2022-2023 Nathan Waivio
--   License     :  BSD3
--   Maintainer  :  Nathan Waivio <nathan.waivio@gmail.com>
--   Stability   :  Stable
--   Portability :  Portable
--
-- | Library implementing standard 'Posit-3.2', and 'Posit-2022' numbers, as defined by
--   the Posit Working Group 23 June 2018, and in 2022 respectively.
-- 
-- 
---------------------------------------------------------------------------------------------


{-# LANGUAGE TypeFamilyDependencies #-} -- For the associated bidirectional type family that the Posit library is based on
{-# LANGUAGE DataKinds #-}  -- For our ES kind and the constructors Z, I, II, III, IV, V, for exponent size type
{-# LANGUAGE TypeApplications #-}  -- The most excellent syntax @Int256
{-# LANGUAGE AllowAmbiguousTypes #-} -- The Haskell/GHC Type checker seems to have trouble things in the PositC class
{-# LANGUAGE ScopedTypeVariables #-} -- To reduce some code duplication
{-# LANGUAGE FlexibleContexts #-} -- To reduce some code duplication by claiming the type family provides some constraints, that GHC can't do without fully evaluating the type family
{-# LANGUAGE ConstrainedClassMethods #-} -- Allows constraints on class methods so default implementations of methods with Type Families can be implemented
{-# LANGUAGE ConstraintKinds #-}  -- Simplify all of the constraints into a combinded constraint for the super class constraint
{-# LANGUAGE DerivingVia #-}  -- To Derive instances for newtypes to eliminate Orphan Instances
{-# LANGUAGE UndecidableInstances #-}  -- For deriving DoubleWord
{-# LANGUAGE CPP #-} -- To remove Storable instances to remove noise when performing analysis of Core
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}  -- Turn off noise
{-# OPTIONS_GHC -Wno-type-defaults #-}  -- Turn off noise

-- ----
--  |Posit Class, implementing:
--
--   * PositC
-- ----

module Posit.Internal.PositC
(PositC(..),
 ES(..),
 PositF(),
 IntN,
 FixedWidthInteger(),
 Max,
 Next,
 Prev
 ) where

import Prelude hiding (exponent,significand)

#ifndef O_NO_STORABLE_RANDOM
-- Imports for Storable Instance of Data.DoubleWord
import Foreign.Storable (Storable, sizeOf, alignment, peek, poke)  -- Used for Storable Instances of Data.DoubleWord
import Foreign.Ptr (Ptr, plusPtr, castPtr)  -- Used for dealing with Pointers for the Data.DoubleWord Storable Instance

import System.Random.Stateful (Uniform, uniformM)
#endif

-- Machine Integers and Operations
{-@ embed Int128 * as int @-}
{-@ embed Int256 * as int @-}
import Data.Int (Int8,Int16,Int32,Int64)  -- Import standard Int sizes
import Data.DoubleWord (Word128,Int128,Int256,fromHiAndLo,hiWord,loWord,DoubleWord,BinaryWord) -- Import large Int sizes
import Data.Word (Word64)
import Data.Bits (Bits(..), shiftL, shiftR, testBit, (.&.), FiniteBits)


-- Import Naturals and Rationals
{-@ embed Natural * as int @-}
import GHC.Natural (Natural) -- Import the Natural Numbers ℕ (u+2115)
{-@ embed Ratio * as real @-}
{-@ embed Rational * as real @-}
import Data.Ratio ((%))  -- Import the Rational Numbers ℚ (u+211A), ℚ can get arbitrarily close to Real numbers ℝ (u+211D)


-- | The Exponent Size 'ES' kind, the constructor for the Type is a Roman Numeral.
data ES = Z_3_2
        | I_3_2
        | II_3_2
        | III_3_2
        | IV_3_2
        | V_3_2
        | Z_2022
        | I_2022
        | II_2022
        | III_2022
        | IV_2022
        | V_2022

-- | Type of the Finite Precision Representation, in our case Int8, 
-- Int16, Int32, Int64, Int128, Int256.
{-@ embed IntN * as int @-}
type family IntN (es :: ES)
  where
    IntN Z_3_2   = Int8
    IntN I_3_2   = Int16
    IntN II_3_2  = Int32
    IntN III_3_2 = Int64
#ifdef O_NO_STORABLE_RANDOM
    IntN IV_3_2  = Int128
    IntN V_3_2   = Int256
#else
    IntN IV_3_2  = Int128_Storable_Random
    IntN V_3_2   = Int256_Storable_Random
#endif
    IntN Z_2022   = Int8
    IntN I_2022   = Int16
    IntN II_2022  = Int32
    IntN III_2022 = Int64
#ifdef O_NO_STORABLE_RANDOM
    IntN IV_2022  = Int128
    IntN V_2022   = Int256
#else
    IntN IV_2022  = Int128_Storable_Random
    IntN V_2022   = Int256_Storable_Random

-- | New Type Wrappers to resolve Orphan Instance Issue
newtype Int128_Storable_Random = Int128_Storable_Random Int128
  deriving (Bits,Bounded,Enum,Real,Integral,Eq,Ord,Num,Read,Show,DoubleWord,BinaryWord,FiniteBits)
    via Int128
newtype Int256_Storable_Random = Int256_Storable_Random Int256
  deriving (Bits,Bounded,Enum,Real,Integral,Eq,Ord,Num,Read,Show,DoubleWord,BinaryWord,FiniteBits)
    via Int256
newtype Word128_Storable_Random = Word128_Storable_Random Word128
  deriving (Bits,Bounded,Enum,Real,Integral,Eq,Ord,Num,Read,Show,DoubleWord,BinaryWord,FiniteBits)
    via Word128
#endif

-- | Type Max of Kind ES
type family Max (es :: ES)
  where
    Max Z_3_2    = V_3_2
    Max I_3_2    = V_3_2
    Max II_3_2   = V_3_2
    Max III_3_2  = V_3_2
    Max IV_3_2   = V_3_2
    Max V_3_2    = V_3_2
    Max Z_2022   = V_2022
    Max I_2022   = V_2022
    Max II_2022  = V_2022
    Max III_2022 = V_2022
    Max IV_2022  = V_2022
    Max V_2022   = V_2022

type family Next (es :: ES)
  where
    Next Z_3_2    = I_3_2
    Next I_3_2    = II_3_2
    Next II_3_2   = III_3_2
    Next III_3_2  = IV_3_2
    Next IV_3_2   = V_3_2
    Next V_3_2    = V_3_2
    Next Z_2022   = I_2022
    Next I_2022   = II_2022
    Next II_2022  = III_2022
    Next III_2022 = IV_2022
    Next IV_2022  = V_2022
    Next V_2022   = V_2022

type family Prev (es :: ES)
  where
    Prev Z_3_2 = Z_3_2
    Prev I_3_2 = Z_3_2
    Prev II_3_2 = I_3_2
    Prev III_3_2 = II_3_2
    Prev IV_3_2 = III_3_2
    Prev V_3_2 = IV_3_2
    Prev Z_2022 = Z_2022
    Prev I_2022 = Z_2022
    Prev II_2022 = I_2022
    Prev III_2022 = II_2022
    Prev IV_2022 = III_2022
    Prev V_2022 = IV_2022

-- | The 'FixedWidthInteger' is a Constraint Synonym that contains all
-- of the constraints provided by the 'IntN' Type Family.  It is a super
-- class for the Posit Class.
type FixedWidthInteger a = 
  (Bits a
  ,Bounded a
  ,Enum a
  ,Integral a
  ,Eq a
  ,Ord a
  ,Num a
  ,Read a
  ,Show a
#ifndef O_NO_STORABLE_RANDOM
  ,Storable a
  ,Uniform a
#endif
  )


-- | The 'Posit' class is an approximation of ℝ, it is like a sampling 
-- on the Projective Real line ℙ(ℝ) with Maybe ℚ as the internal type.
-- The 'es' is an index that controlls the log2 word size of the Posit's
-- fininte precision representation.
class (FixedWidthInteger (IntN es)) => PositC (es :: ES) where
  
  -- | Transform to/from the Infinite Precision Representation
  encode :: Maybe Rational -> IntN es  -- ^ Maybe you have some Rational Number and you want to encode it as some integer with a finite integer log2 word size.
  encode Nothing = unReal @es
  encode (Just 0) = 0
  encode (Just r)
    | r > maxPosRat @es = mostPosVal @es
    | r < minNegRat @es = mostNegVal @es
    | r > 0 && r < minPosRat @es = leastPosVal @es
    | r < 0 && r > maxNegRat @es = leastNegVal @es
    | otherwise = buildIntRep @es r
  
  decode :: IntN es -> Maybe Rational  -- ^ You have an integer with a finite integer log2 word size decode it and Maybe it is Rational
  decode int
    | int == unReal @es = Nothing
    | int == 0 = Just 0
    | otherwise =
      let sgn = int < 0
          int' = if sgn
                 then negate int
                 else int
          (regime,nR) = regime2Integer @es int'
          exponent = exponent2Nat @es nR int'  -- if no e or some bits missing, then they are considered zero
          rat = fraction2Posit @es nR int'  -- if no fraction or some bits missing, then the missing bits are zero, making the significand p=1
      in tupPosit2Posit @es (sgn,regime,exponent,rat)
  
  
  -- | Exponent Size based on the Posit Exponent kind ES, Posit-2022 sets the default to 2.
  exponentSize :: Natural  -- ^ The exponent size, 'es' is a Natural number
  exponentSize = 2
  
  -- | Various other size definitions used in the Posit format with their default definitions
  nBytes :: Natural  -- ^ 'nBytes' the number of bytes of the Posit Representation
  nBytes = 2^(exponentSize @es)
  
  nBits :: Natural  -- ^ 'nBits' the number of bits of the Posit Representation
  nBits = 8 * (nBytes @es)
  
  signBitSize :: Natural  -- ^ 'signBitSize' the size of the sign bit
  signBitSize = 1
  
  uSeed :: Natural  -- ^ 'uSeed' scaling factor for the regime of the Posit Representation
  uSeed = 2^2^(exponentSize @es)
  
  -- | Integer Representation of common bounds
  unReal :: IntN es  -- ^ 'unReal' is something that is not Real, the integer value that is not a Real number
  unReal = minBound @(IntN es)
  
  mostPosVal :: IntN es
  mostPosVal = maxBound @(IntN es)
  
  leastPosVal :: IntN es
  leastPosVal = 1
  
  leastNegVal :: IntN es
  leastNegVal = -1
  
  mostNegVal :: IntN es
  mostNegVal = negate (mostPosVal @es)
  
  -- Rational Value of common bounds
  maxPosRat :: Rational
  maxPosRat = (fromIntegral (uSeed @es)^(nBits @es - 2)) % 1
  minPosRat :: Rational
  minPosRat = recip (maxPosRat @es)
  maxNegRat :: Rational
  maxNegRat = negate (minPosRat @es)
  minNegRat :: Rational
  minNegRat = negate (maxPosRat @es)
  
  -- Functions to support encode and decode
  
  -- log base uSeed
  -- After calculating the regime the rational should be in the range [1,uSeed), it starts with (0,rational)
  log_uSeed :: (Integer, Rational) -> (Integer, Rational)
  log_uSeed (regime,r)
    | r < 1 = log_uSeed @es (regime-1,r * fromRational (toInteger (uSeed @es) % 1))
    | r >= fromRational (toInteger (uSeed @es) % 1) = log_uSeed @es (regime+1,r * fromRational (1 % toInteger (uSeed @es)))
    | otherwise = (regime,r)
  
  getRegime :: Rational -> (Integer, Rational)
  getRegime r = log_uSeed @es (0,r)
  
  posit2TupPosit :: Rational -> (Bool, Integer, Natural, Rational)
  posit2TupPosit r =
    let (sgn,r') = getSign r -- returns the sign and a positive rational
        (regime,r'') = getRegime @es r' -- returns the regime and a rational between uSeed^-1 to uSeed^1
        (exponent,significand) = getExponent r'' -- returns the exponent and a rational between [1,2), the significand
    in (sgn,regime,exponent,significand)
  
  buildIntRep :: Rational -> IntN es
  buildIntRep r =
    let (signBit,regime,exponent,significand) = posit2TupPosit @es r
        intRep = mkIntRep @es regime exponent significand
    in if signBit
       then negate intRep
       else intRep
  
  mkIntRep :: Integer -> Natural -> Rational -> IntN es
  mkIntRep regime exponent significand =
    let (regime', offset) = formRegime @es regime  -- offset is the number of binary digits remaining after the regime is formed
        (exponent', offset') = formExponent @es exponent offset  -- offset' is the number of binary digits remaining after the exponent is formed
        fraction = formFraction @es significand offset'
    in regime' + exponent' + fraction  --  Previously bad code...
    -- Was previously Bitwise OR'd (regime' .|. exponent' .|. fraction), but that failed when an overflow occurs in the fraction:
    -- (R @V_3_2 (6546781215792283740026379393655198304433284092086129578966582736192267592809066457889108741457440782093636999212155773298525238592782299216095867171579 % 6546781215792283740026379393655198304433284092086129578966582736192267592809349109766540184651808314301773368255120142018434513091770786106657055178752))
  
  formRegime :: Integer -> (IntN es, Integer)
  formRegime power
    | 0 <= power =
      let offset = (fromIntegral (nBits @es - 1) - power - 1)
      in (fromIntegral (2^(power + 1) - 1) `shiftL` fromInteger offset, offset - 1)
    | otherwise =
      let offset = (fromIntegral (nBits @es - 1) - abs power - 1)
      in (1 `shiftL` fromInteger offset, offset)
  
  formExponent :: Natural -> Integer -> (IntN es, Integer)
  formExponent power offset =
    let offset' = offset - fromIntegral (exponentSize @es)
        result = fromIntegral power `shiftR` negate (fromInteger offset')
    in if offset' >= 0
       then (fromIntegral power `shiftL` fromInteger offset', offset')
       else if testBit (fromIntegral power :: Int) (pred.fromIntegral.negate $ offset')  -- What is "Twilight Zone" Posit Number?
            then (succ result, offset')
            else (result, offset')  -- rounding case
  
  formFraction :: Rational -> Integer -> IntN es
  formFraction r offset =
    let numFractionBits = offset
        fractionSize = 2^numFractionBits
        normFraction = round $ (r - 1) * fractionSize  -- "posit - 1" is changing it from the significand to the fraction: [1,2) -> [0,1)
    in if | numFractionBits >= 1 -> fromInteger normFraction
          | numFractionBits == 0 -> if 2 * (r - 1) > 1 -- rounding case -- Previously was 0, this should be "banker's rounding" per Gustafson's ch3
                                    then 1
                                    else 0
          | otherwise -> 0
  
  tupPosit2Posit :: (Bool,Integer,Natural,Rational) -> Maybe Rational
  tupPosit2Posit (sgn,regime,exponent,rat) = -- s = isNeg posit == True
    let pow2 = toRational (uSeed @es)^^regime * 2^exponent
        scale = if sgn
                then negate pow2
                else pow2
    in Just $ scale * rat
  
  regime2Integer :: IntN es -> (Integer, Int)
  regime2Integer posit =
    let regimeFormat = findRegimeFormat @es posit
        regimeCount = countRegimeBits @es regimeFormat posit
        regime = calcRegimeInt regimeFormat regimeCount
    in (regime, regimeCount + 1) -- a rational representation of the regime and the regimeCount plus rBar which is the numBitsRegime
  
  -- will return the format of the regime, either HI or LO; it could get refactored in the future
  -- True means a 1 is the first bit in the regime
  findRegimeFormat :: IntN es -> Bool
  findRegimeFormat posit = testBit posit (fromIntegral (nBits @es) - 1 - fromIntegral (signBitSize @es))
  
  countRegimeBits :: Bool -> IntN es -> Int
  countRegimeBits format posit = go (fromIntegral (nBits @es) - 1 - fromIntegral (signBitSize @es)) 0
    where
      go (-1) acc = acc
      go index acc
        | xnor format (testBit posit index)  = go (index - 1) (acc + 1)
        | otherwise = acc
  
  -- knowing the number of the regime bits, and the sign bit we can extract
  -- the exponent.  We mask to the left of the exponent to remove the sign and regime, and
  -- then shift to the right to remove the fraction.
  exponent2Nat :: Int -> IntN es -> Natural
  exponent2Nat numBitsRegime posit =
    let bitsRemaining = fromIntegral (nBits @es) - numBitsRegime - fromIntegral (signBitSize @es)
        signNRegimeMask = 2^bitsRemaining - 1
        int = posit .&. signNRegimeMask
        nBitsToTheRight = fromIntegral (nBits @es) - numBitsRegime - fromIntegral (signBitSize @es) - fromIntegral (exponentSize @es)
    in if bitsRemaining <=0
       then 0
       else if nBitsToTheRight < 0
            then fromIntegral $ int `shiftL` negate nBitsToTheRight
            else fromIntegral $ int `shiftR` nBitsToTheRight
  
  -- knowing the number of the regime bits, sign bit, and the number of the
  -- exponent bits we can extract the fraction.  We mask to the left of the fraction to
  -- remove the sign, regime, and exponent. If there is no fraction then the value is 1.
  fraction2Posit :: Int -> IntN es -> Rational
  fraction2Posit numBitsRegime posit =
    let offset = fromIntegral $ (signBitSize @es) + fromIntegral numBitsRegime + (exponentSize @es)
        fractionSize = fromIntegral (nBits @es) - offset
        fractionBits = posit .&. (2^fractionSize - 1)
    in if fractionSize >= 1
       then (2^fractionSize + toInteger fractionBits) % 2^fractionSize
       else 1 % 1
  
  -- prints out the IntN es value in 0b... format
  displayBin :: IntN es -> String
  displayBin int = "0b" ++ go (fromIntegral (nBits @es) - 1)
    where
      go :: Int -> String
      go 0 = if testBit int 0
             then "1"
             else "0"
      go idx = if testBit int idx
               then "1" ++ go (idx - 1)
               else "0" ++ go (idx - 1)
  
  -- decimal Precision
  decimalPrec :: IntN es -> Int
  -- decimalPrec _ = fromIntegral $ 2 * (nBytes @es) + 1  -- The Olden way, 1.37% fail to round trip
  --
  -- `1 + ` Gustafson's Decimal Accuracy (from coorispondance with Alessandro)  -- Sucessfuly round triped for both Posit32 and P32, and below
  decimalPrec posit =
    let regimeFormat = findRegimeFormat @es posit
        regimeCount = countRegimeBits @es regimeFormat posit
        fractionSize = max (fromIntegral (nBits @es) - fromIntegral (signBitSize @es) - regimeCount - fromIntegral (exponentSize @es)) 0  -- fractionSize is at least zero
    in succ.ceiling $ (fromIntegral fractionSize + 1) * log10Of2 + halflog10Of2
  --

  {-# MINIMAL exponentSize | nBytes #-}


-- =====================================================================
-- ===                    PositC Instances                           ===
-- =====================================================================
-- | Standard 3.2
instance PositC Z_3_2 where
  exponentSize = 0

instance PositC I_3_2 where
  exponentSize = 1

instance PositC II_3_2 where
  exponentSize = 2

instance PositC III_3_2 where
  exponentSize = 3

instance PositC IV_3_2 where
  exponentSize = 4

instance PositC V_3_2 where
  exponentSize = 5

-- | Standard 2022
instance PositC Z_2022 where
  nBytes = 2^0

instance PositC I_2022 where
  nBytes = 2^1

instance PositC II_2022 where
  nBytes = 2^2

instance PositC III_2022 where
  nBytes = 2^3

instance PositC IV_2022 where
  nBytes = 2^4

instance PositC V_2022 where
  nBytes = 2^5

-- | `PositF` Constraint Synonym for things that need both the word size and the next higher word size
type PositF es = (PositC es, PositC (Next es), PositC (Prev es))


-- =====================================================================
-- ===                Encode and Decode Helpers                      ===
-- =====================================================================


-- getSign finds the sign value and then returns the absolute value of the Posit
getSign :: Rational -> (Bool, Rational)
getSign r =
  let s = r <= 0
      absPosit =
        if s
        then negate r
        else r
  in (s,absPosit)  -- pretty much the same as 'abs')

-- Exponent should be an integer in the range of [0,uSeed), and also return an exponent and a rational in the range of [1,2)
getExponent :: Rational -> (Natural, Rational)
getExponent r = log_2 (0,r)

log_2 :: (Natural, Rational) -> (Natural, Rational)
log_2 (exponent,r) | r <  1 = error "Should never happen, exponent should be a natural number, i.e. positive integer."
                   | r >= (2 % 1) = log_2 (exponent+1,r * (1 % 2))
                   | otherwise = (exponent,r)


calcRegimeInt :: Bool -> Int -> Integer
calcRegimeInt format count | format = fromIntegral (count - 1)
                           | otherwise = fromIntegral $ negate count


xnor :: Bool -> Bool -> Bool
xnor a b = not $ (a || b) && not (b && a)


log10Of2 :: Double
log10Of2 = 0.3010299956639811952137388947


halflog10Of2 :: Double
halflog10Of2 = 0.1505149978319905976068694473


#ifndef O_NO_STORABLE_RANDOM
-- =====================================================================
-- ===                  Storable Instances                           ===
-- =====================================================================
--
-- Storable Instance for Word128 using the DoubleWord type class and Word128_Storable newtype
instance Storable Word128_Storable_Random where
  sizeOf _ = 16
  alignment _ = 16
  peek ptr = do
    hi <- peek $ offsetWord 0
    lo <- peek $ offsetWord 1
    return $ fromHiAndLo hi lo
      where
        offsetWord i = (castPtr ptr :: Ptr Word64) `plusPtr` (i*8)
  poke ptr int = do
    poke (offsetWord 0) (hiWord int)
    poke (offsetWord 1) (loWord int)
      where
        offsetWord i = (castPtr ptr :: Ptr Word64) `plusPtr` (i*8)

-- Storable Instance for Int128 using the DoubleWord type class and Int128_Storable newtype
instance Storable Int128_Storable_Random where
  sizeOf _ = 16
  alignment _ = 16
  peek ptr = do
    hi <- peek $ offsetInt 0
    lo <- peek $ offsetWord 1
    return $ fromHiAndLo hi lo
      where
        offsetInt i = (castPtr ptr :: Ptr Int64) `plusPtr` (i*8)
        offsetWord i = (castPtr ptr :: Ptr Word64) `plusPtr` (i*8)
  poke ptr int = do
    poke (offsetInt 0) (hiWord int)
    poke (offsetWord 1) (loWord int)
      where
        offsetInt i = (castPtr ptr :: Ptr Int64) `plusPtr` (i*8)
        offsetWord i = (castPtr ptr :: Ptr Word64) `plusPtr` (i*8)

-- Storable Instance for Int256 using the DoubleWord type class and Int256_Storable newtype
instance Storable Int256_Storable_Random where
  sizeOf _ = 32
  alignment _ = 32
  peek ptr = do
    (Int128_Storable_Random hi) <- peek $ offsetInt 0
    (Word128_Storable_Random lo) <- peek $ offsetWord 1
    return $ fromHiAndLo hi lo
      where
        offsetInt i = (castPtr ptr :: Ptr Int128_Storable_Random) `plusPtr` (i*16)
        offsetWord i = (castPtr ptr :: Ptr Word128_Storable_Random) `plusPtr` (i*16)
  poke ptr int = do
    poke (offsetInt 0) (Int128_Storable_Random $ hiWord int)
    poke (offsetWord 1) (Word128_Storable_Random $ loWord int)
      where
        offsetInt i = (castPtr ptr :: Ptr Int128_Storable_Random) `plusPtr` (i*16)
        offsetWord i = (castPtr ptr :: Ptr Word128_Storable_Random) `plusPtr` (i*16)
--

-- =====================================================================
-- ===                  Random   Instances                           ===
-- =====================================================================
--
-- Uniform Instance for Word128 using the Double Word type class and Word128_Storable newtype
instance Uniform Word128_Storable_Random where
  uniformM g = do
    hi <- uniformM g
    lo <- uniformM g
    return $ fromHiAndLo hi lo

-- Uniform Instance for Int128 using the Double Word type class and Int128_Storable newtype
instance Uniform Int128_Storable_Random where
  uniformM g = do
    hi <- uniformM g
    lo <- uniformM g
    return $ fromHiAndLo hi lo

-- Uniform Instance for Int256 using the Double Word type class and Int256_Storable newtype
instance Uniform Int256_Storable_Random where
  uniformM g = do
    Int128_Storable_Random hi <- uniformM g
    Word128_Storable_Random lo <- uniformM g
    return $ fromHiAndLo hi lo
#endif


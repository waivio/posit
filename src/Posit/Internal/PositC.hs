
--------------------------------------------------------------------------------------------
--
--   Copyright   :  (C) 2022 Nathan Waivio
--   License     :  BSD3
--   Maintainer  :  Nathan Waivio <nathan.waivio@gmail.com>
--   Stability   :  Stable
--   Portability :  Portable
--
-- | Library implementing standard 'Posit-3.2' numbers, as defined by
--   the Posit Working Group 23 June 2018.
-- 
-- 
---------------------------------------------------------------------------------------------


{-# LANGUAGE TypeFamilyDependencies #-} -- For the associated bidirectional type family that the Posit library is based on
{-# LANGUAGE DataKinds #-}  -- For our ES kind and the constructors Z, I, II, III, IV, V, for exponent size type
{-# LANGUAGE TypeApplications #-}  -- The most excellent syntax @Int256
{-# LANGUAGE AllowAmbiguousTypes #-} -- The Haskell/GHC Type checker seems to have trouble things in the PositC class
{-# LANGUAGE ScopedTypeVariables #-} -- To reduce some code duplication
{-# LANGUAGE FlexibleContexts #-} -- To reduce some code duplication by claiming the type family provides some constraints, that GHC can't do without fully evaluating the type family
{-# LANGUAGE CPP #-} -- To remove Storable instances to remove noise when performing analysis of Core
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}  -- Turn off noise
{-# OPTIONS_GHC -Wno-type-defaults #-}  -- Turn off noise

-- ----
--  |Posit Class, implementing:
--
--   * PositC
--   * Orphan Instances of Storable for Word128, Int128, Int256
-- ----

module Posit.Internal.PositC
(PositC(..),
 ES(..)
 ) where

import Prelude hiding (exponent,significand)

-- Imports for Storable Instance of Data.DoubleWord
import Foreign.Storable (Storable, sizeOf, alignment, peek, poke)  -- Used for Storable Instances of Data.DoubleWord
import Foreign.Ptr (Ptr, plusPtr, castPtr)  -- Used for dealing with Pointers for the Data.DoubleWord Storable Instance

-- Machine Integers and Operations
{-@ embed Int128 * as int @-}
{-@ embed Int256 * as int @-}
import Data.Int (Int8,Int16,Int32,Int64)  -- Import standard Int sizes
import Data.DoubleWord (Word128,Int128,Int256,fromHiAndLo,hiWord,loWord) -- Import large Int sizes
import Data.Word (Word64)
import Data.Bits ((.|.), shiftL, shift, testBit, (.&.), shiftR)

-- Import Naturals and Rationals
{-@ embed Natural * as int @-}
import GHC.Natural (Natural) -- Import the Natural Numbers ℕ (u+2115)
{-@ embed Ratio * as int @-}
import Data.Ratio (Rational, (%))  -- Import the Rational Numbers ℚ (u+211A), ℚ can get arbitrarily close to Real numbers ℝ (u+211D)


-- | The Exponent Size 'ES' kind, the constructor for the Type is a Roman Numeral.
data ES = Z
        | I
        | II
        | III
        | IV
        | V


-- | The 'Posit' class is an approximation of ℝ, it is like a sampling on the Projective Real line ℙ(ℝ) with Maybe ℚ as the internal type.
-- The 'es' is an index that controlls the log2 word size of the Posit's
-- fininte precision representation.
class PositC (es :: ES) where
  -- | Type of the Finite Precision Representation, in our case Int8, Int16, Int32, Int64, Int128, Int256. The 'es' of kind 'ES' will determine a result of 'r' such that you can determine the 'es' by the 'r'
  type IntN es = r | r -> es
 
 
  -- | Transform to/from the Infinite Precision Representation
  encode :: Maybe Rational -> IntN es  -- ^ Maybe you have some Rational Number and you want to encode it as some integer with a finite integer log2 word size.
  decode :: IntN es -> Maybe Rational  -- ^ You have an integer with a finite integer log2 word size decode it and Maybe it is Rational
 
  -- | Exponent Size based on the Posit Exponent kind ES
  exponentSize :: Natural  -- ^ The exponent size, 'es' is a Natural number
 
  -- | Various other size definitions used in the Posit format with their default definitions
  nBytes :: Natural  -- ^ 'nBytes' the number of bytes of the Posit Representation
  nBytes = 2^(exponentSize @es)
 
  nBits :: Natural  -- ^ 'nBits' the number of bits of the Posit Representation
  nBits = 8 * (nBytes @es)
 
  signBitSize :: Natural  -- ^ 'signBitSize' the size of the sign bit
  signBitSize = 1
 
  uSeed :: Natural  -- ^ 'uSeed' scaling factor for the regime of the Posit Representation
  uSeed = 2^(nBytes @es)
 
  -- | Integer Representation of common bounds
  unReal :: IntN es  -- ^ 'unReal' is something that is not Real, the integer value that is not a Real number
 
  mostPosVal :: IntN es
  leastPosVal :: IntN es
  leastNegVal :: IntN es
  mostNegVal :: IntN es
 
  -- Rational Value of common bounds
  maxPosRat :: Rational
  maxPosRat = 2^((nBytes @es) * ((nBits @es) - 2)) % 1
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
  mkIntRep :: Integer -> Natural -> Rational -> IntN es
  formRegime :: Integer -> (IntN es, Integer)
  formExponent :: Natural -> Integer -> (IntN es, Integer)
  formFraction :: Rational -> Integer -> IntN es
 
  tupPosit2Posit :: (Bool,Integer,Natural,Rational) -> Maybe Rational
  tupPosit2Posit (sgn,regime,exponent,rat) = -- s = isNeg posit == True
    let pow2 = toRational (uSeed @es)^^regime * 2^exponent
        scale = if sgn
                then negate pow2
                else pow2
    in Just $ scale * rat
 
  regime2Integer :: IntN es -> (Integer, Int)
  findRegimeFormat :: IntN es -> Bool
  countRegimeBits :: Bool -> IntN es -> Int
  exponent2Nat :: Int -> IntN es -> Natural
  fraction2Posit :: Int -> IntN es -> Rational
 
  -- prints out the IntN es value in 0b... format
  displayBin :: IntN es -> String
  -- decimal Precision
  decimalPrec :: Int
  decimalPrec = fromIntegral $ 2 * (nBytes @es) + 1



instance PositC Z where
  type IntN Z = Int8
  exponentSize = 0
 
  -- Posit Integer Rep of various values
  unReal = minBound @Int8
 
  mostPosVal = maxBound @Int8
  leastPosVal = 1
  leastNegVal = -1
  mostNegVal = negate mostPosVal
 
  encode Nothing = unReal @Z
  encode (Just 0) = 0
  encode (Just r)
    | r > maxPosRat @Z = mostPosVal @Z
    | r < minNegRat @Z = mostNegVal @Z
    | r > 0 && r < minPosRat @Z = leastPosVal @Z
    | r < 0 && r > maxNegRat @Z = leastNegVal @Z
    | otherwise = buildIntRep @Z r
 
  buildIntRep r =
    let (signBit,regime,exponent,significand) = posit2TupPosit @Z r
        intRep = mkIntRep @Z regime exponent significand
    in if signBit
       then negate intRep
       else intRep
 
  mkIntRep regime exponent significand =
    let (regime', offset) = formRegime @Z regime  -- offset is the number of binary digits remaining after the regime is formed
        (exponent', offset') = formExponent @Z exponent offset  -- offset' is the number of binary digits remaining after the exponent is formed
        fraction = formFraction @Z significand offset'
    in regime' .|. exponent' .|. fraction
 
  formRegime power
    | 0 <= power =
      let offset = (fromIntegral (nBits @Z - 1) -     power - 1)
      in (fromIntegral (2^(power + 1) - 1) `shiftL` fromInteger offset, offset - 1)
    | otherwise =
      let offset = (fromIntegral (nBits @Z - 1) - abs power - 1)
      in (1 `shiftL` fromInteger offset, offset)
 
  formExponent power offset =
    let offset' = offset - fromIntegral (exponentSize @Z)
    in (fromIntegral power `shift` fromInteger offset', offset')
 
  formFraction r offset =
    let numFractionBits = offset
        fractionSize = 2^numFractionBits
        normFraction = round $ (r - 1) * fractionSize  -- "posit - 1" is changing it from the significand to the fraction: [1,2) -> [0,1)
    in if numFractionBits >= 1
       then fromInteger normFraction
       else 0
 
  decode int
    | int == unReal @Z = Nothing
    | int == 0 = Just 0
    | otherwise =
      let sgn = int < 0
          int' = if sgn
                 then negate int
                 else int
          (regime,nR) = regime2Integer @Z int'
          exponent = exponent2Nat @Z nR int'  -- if no e or some bits missing, then they are considered zero
          rat = fraction2Posit @Z nR int'  -- if no fraction or some bits missing, then the missing bits are zero, making the significand p=1
      in tupPosit2Posit @Z (sgn,regime,exponent,rat)
 
  regime2Integer posit =
    let regimeFormat = findRegimeFormat @Z posit
        regimeCount = countRegimeBits @Z regimeFormat posit
        regime = calcRegimeInt regimeFormat regimeCount
    in (regime, regimeCount + 1) -- a rational representation of the regime and the regimeCount plus rBar which is the numBitsRegime
 
  -- will return the format of the regime, either HI or LO; it could get refactored in the future
  -- True means a 1 is the first bit in the regime
  findRegimeFormat posit = testBit posit (fromIntegral (nBits @Z) - 1 - fromIntegral (signBitSize @Z))
 
  countRegimeBits format posit = go (fromIntegral (nBits @Z) - 1 - fromIntegral (signBitSize @Z)) 0
    where
      go (-1) acc = acc
      go index acc
        | xnor format (testBit posit index)  = go (index - 1) (acc + 1)
        | otherwise = acc
 
  -- knowing the number of the regime bits, and the sign bit we can extract
  -- the exponent.  We mask to the left of the exponent to remove the sign and regime, and
  -- then shift to the right to remove the fraction.
  exponent2Nat numBitsRegime posit =
    let bitsRemaining = fromIntegral (nBits @Z) - numBitsRegime - fromIntegral (signBitSize @Z)
        signNRegimeMask = 2^bitsRemaining - 1
        int = posit .&. signNRegimeMask
        nBitsToTheRight = fromIntegral (nBits @Z) - numBitsRegime - fromIntegral (signBitSize @Z) - fromIntegral (exponentSize @Z)
    in if bitsRemaining <=0
       then 0
       else if nBitsToTheRight < 0
            then fromIntegral $ int `shiftL` negate nBitsToTheRight
            else fromIntegral $ int `shiftR` nBitsToTheRight
 
  -- knowing the number of the regime bits, sign bit, and the number of the
  -- exponent bits we can extract the fraction.  We mask to the left of the fraction to
  -- remove the sign, regime, and exponent. If there is no fraction then the value is 1.
  fraction2Posit numBitsRegime posit =
    let offset = fromIntegral $ (signBitSize @Z) + fromIntegral numBitsRegime + (exponentSize @Z)
        fractionSize = fromIntegral (nBits @Z) - offset
        fractionBits = posit .&. (2^fractionSize - 1)
    in if fractionSize >= 1
       then (2^fractionSize + toInteger fractionBits) % 2^fractionSize
       else 1 % 1
 
  displayBin int = "0b" ++ go (fromIntegral (nBits @Z) - 1)
    where
      go :: Int -> String
      go 0 = if testBit int 0
             then "1"
             else "0"
      go idx = if testBit int idx
               then "1" ++ go (idx - 1)
               else "0" ++ go (idx -1)



instance PositC I where
  type IntN I = Int16
  exponentSize = 1
 
  -- Posit Integer Rep of various values
  unReal = minBound @Int16
 
  mostPosVal = maxBound @Int16
  leastPosVal = 1
  leastNegVal = -1
  mostNegVal = negate mostPosVal
 
  encode Nothing = unReal @I
  encode (Just 0) = 0
  encode (Just r)
    | r > maxPosRat @I = mostPosVal @I
    | r < minNegRat @I = mostNegVal @I
    | r > 0 && r < minPosRat @I = leastPosVal @I
    | r < 0 && r > maxNegRat @I = leastNegVal @I
    | otherwise = buildIntRep @I r
 
  buildIntRep r =
    let (signBit,regime,exponent,significand) = posit2TupPosit @I r
        intRep = mkIntRep @I regime exponent significand
    in if signBit
       then negate intRep
       else intRep
 
  mkIntRep regime exponent significand =
    let (regime', offset) = formRegime @I regime  -- offset is the number of binary digits remaining after the regime is formed
        (exponent', offset') = formExponent @I exponent offset  -- offset' is the number of binary digits remaining after the exponent is formed
        fraction = formFraction @I significand offset'
    in regime' .|. exponent' .|. fraction
 
  formRegime power
    | 0 <= power =
      let offset = (fromIntegral (nBits @I - 1) -     power - 1)
      in (fromIntegral (2^(power + 1) - 1) `shiftL` fromInteger offset, offset - 1)
    | otherwise =
      let offset = (fromIntegral (nBits @I - 1) - abs power - 1)
      in (1 `shiftL` fromInteger offset, offset)
 
  formExponent power offset =
    let offset' = offset - fromIntegral (exponentSize @I)
    in (fromIntegral power `shift` fromInteger offset', offset')
 
  formFraction r offset =
    let numFractionBits = offset
        fractionSize = 2^numFractionBits
        normFraction = round $ (r - 1) * fractionSize  -- "posit - 1" is changing it from the significand to the fraction: [1,2) -> [0,1)
    in if numFractionBits >= 1
       then fromInteger normFraction
       else 0
 
  decode int
    | int == unReal @I = Nothing
    | int == 0 = Just 0
    | otherwise =
      let sgn = int < 0
          int' = if sgn
                 then negate int
                 else int
          (regime,nR) = regime2Integer @I int'
          exponent = exponent2Nat @I nR int'  -- if no e or some bits missing, then they are considered zero
          rat = fraction2Posit @I nR int'  -- if no fraction or some bits missing, then the missing bits are zero, making the significand p=1
      in tupPosit2Posit @I (sgn,regime,exponent,rat)
 
  regime2Integer posit =
    let regimeFormat = findRegimeFormat @I posit
        regimeCount = countRegimeBits @I regimeFormat posit
        regime = calcRegimeInt regimeFormat regimeCount
    in (regime, regimeCount + 1) -- a rational representation of the regime and the regimeCount plus rBar which is the numBitsRegime
 
  -- will return the format of the regime, either HI or LO; it could get refactored in the future
  -- True means a 1 is the first bit in the regime
  findRegimeFormat posit = testBit posit (fromIntegral (nBits @I) - 1 - fromIntegral (signBitSize @I))
 
  countRegimeBits format posit = go (fromIntegral (nBits @I) - 1 - fromIntegral (signBitSize @I)) 0
    where
      go (-1) acc = acc
      go index acc
        | xnor format (testBit posit index)  = go (index - 1) (acc + 1)
        | otherwise = acc
 
  -- knowing the number of the regime bits, and the sign bit we can extract
  -- the exponent.  We mask to the left of the exponent to remove the sign and regime, and
  -- then shift to the right to remove the fraction.
  exponent2Nat numBitsRegime posit =
    let bitsRemaining = fromIntegral (nBits @I) - numBitsRegime - fromIntegral (signBitSize @I)
        signNRegimeMask = 2^bitsRemaining - 1
        int = posit .&. signNRegimeMask
        nBitsToTheRight = fromIntegral (nBits @I) - numBitsRegime - fromIntegral (signBitSize @I) - fromIntegral (exponentSize @I)
    in if bitsRemaining <=0
       then 0
       else if nBitsToTheRight < 0
            then fromIntegral $ int `shiftL` negate nBitsToTheRight
            else fromIntegral $ int `shiftR` nBitsToTheRight
 
  -- knowing the number of the regime bits, sign bit, and the number of the
  -- exponent bits we can extract the fraction.  We mask to the left of the fraction to
  -- remove the sign, regime, and exponent. If there is no fraction then the value is 1.
  fraction2Posit numBitsRegime posit =
    let offset = fromIntegral $ (signBitSize @I) + fromIntegral numBitsRegime + (exponentSize @I)
        fractionSize = fromIntegral (nBits @I) - offset
        fractionBits = posit .&. (2^fractionSize - 1)
    in if fractionSize >= 1
       then (2^fractionSize + toInteger fractionBits) % 2^fractionSize
       else 1 % 1
 
  displayBin int = "0b" ++ go (fromIntegral (nBits @I) - 1)
    where
      go :: Int -> String
      go 0 = if testBit int 0
             then "1"
             else "0"
      go idx = if testBit int idx
               then "1" ++ go (idx - 1)
               else "0" ++ go (idx -1)



instance PositC II where
  type IntN II = Int32
  exponentSize = 2
 
  -- Posit Integer Rep of various values
  unReal = minBound @Int32
 
  mostPosVal = maxBound @Int32
  leastPosVal = 1
  leastNegVal = -1
  mostNegVal = negate mostPosVal
 
  encode Nothing = unReal @II
  encode (Just 0) = 0
  encode (Just r)
    | r > maxPosRat @II = mostPosVal @II
    | r < minNegRat @II = mostNegVal @II
    | r > 0 && r < minPosRat @II = leastPosVal @II
    | r < 0 && r > maxNegRat @II = leastNegVal @II
    | otherwise = buildIntRep @II r
 
  buildIntRep r =
    let (signBit,regime,exponent,significand) = posit2TupPosit @II r
        intRep = mkIntRep @II regime exponent significand
    in if signBit
       then negate intRep
       else intRep
 
  mkIntRep regime exponent significand =
    let (regime', offset) = formRegime @II regime  -- offset is the number of binary digits remaining after the regime is formed
        (exponent', offset') = formExponent @II exponent offset  -- offset' is the number of binary digits remaining after the exponent is formed
        fraction = formFraction @II significand offset'
    in regime' .|. exponent' .|. fraction
 
  formRegime power
    | 0 <= power =
      let offset = (fromIntegral (nBits @II - 1) -     power - 1)
      in (fromIntegral (2^(power + 1) - 1) `shiftL` fromInteger offset, offset - 1)
    | otherwise =
      let offset = (fromIntegral (nBits @II - 1) - abs power - 1)
      in (1 `shiftL` fromInteger offset, offset)
 
  formExponent power offset =
    let offset' = offset - fromIntegral (exponentSize @II)
    in (fromIntegral power `shift` fromInteger offset', offset')
 
  formFraction r offset =
    let numFractionBits = offset
        fractionSize = 2^numFractionBits
        normFraction = round $ (r - 1) * fractionSize  -- "posit - 1" is changing it from the significand to the fraction: [1,2) -> [0,1)
    in if numFractionBits >= 1
       then fromInteger normFraction
       else 0
 
  decode int
    | int == unReal @II = Nothing
    | int == 0 = Just 0
    | otherwise =
      let sgn = int < 0
          int' = if sgn
                 then negate int
                 else int
          (regime,nR) = regime2Integer @II int'
          exponent = exponent2Nat @II nR int'  -- if no e or some bits missing, then they are considered zero
          rat = fraction2Posit @II nR int'  -- if no fraction or some bits missing, then the missing bits are zero, making the significand p=1
      in tupPosit2Posit @II (sgn,regime,exponent,rat)
 
  regime2Integer posit =
    let regimeFormat = findRegimeFormat @II posit
        regimeCount = countRegimeBits @II regimeFormat posit
        regime = calcRegimeInt regimeFormat regimeCount
    in (regime, regimeCount + 1) -- a rational representation of the regime and the regimeCount plus rBar which is the numBitsRegime
 
  -- will return the format of the regime, either HI or LO; it could get refactored in the future
  -- True means a 1 is the first bit in the regime
  findRegimeFormat posit = testBit posit (fromIntegral (nBits @II) - 1 - fromIntegral (signBitSize @II))
 
  countRegimeBits format posit = go (fromIntegral (nBits @II) - 1 - fromIntegral (signBitSize @II)) 0
    where
      go (-1) acc = acc
      go index acc
        | xnor format (testBit posit index)  = go (index - 1) (acc + 1)
        | otherwise = acc
 
  -- knowing the number of the regime bits, and the sign bit we can extract
  -- the exponent.  We mask to the left of the exponent to remove the sign and regime, and
  -- then shift to the right to remove the fraction.
  exponent2Nat numBitsRegime posit =
    let bitsRemaining = fromIntegral (nBits @II) - numBitsRegime - fromIntegral (signBitSize @II)
        signNRegimeMask = 2^bitsRemaining - 1
        int = posit .&. signNRegimeMask
        nBitsToTheRight = fromIntegral (nBits @II) - numBitsRegime - fromIntegral (signBitSize @II) - fromIntegral (exponentSize @II)
    in if bitsRemaining <=0
       then 0
       else if nBitsToTheRight < 0
            then fromIntegral $ int `shiftL` negate nBitsToTheRight
            else fromIntegral $ int `shiftR` nBitsToTheRight
 
  -- knowing the number of the regime bits, sign bit, and the number of the
  -- exponent bits we can extract the fraction.  We mask to the left of the fraction to
  -- remove the sign, regime, and exponent. If there is no fraction then the value is 1.
  fraction2Posit numBitsRegime posit =
    let offset = fromIntegral $ (signBitSize @II) + fromIntegral numBitsRegime + (exponentSize @II)
        fractionSize = fromIntegral (nBits @II) - offset
        fractionBits = posit .&. (2^fractionSize - 1)
    in if fractionSize >= 1
       then (2^fractionSize + toInteger fractionBits) % 2^fractionSize
       else 1 % 1
 
  displayBin int = "0b" ++ go (fromIntegral (nBits @II) - 1)
    where
      go :: Int -> String
      go 0 = if testBit int 0
             then "1"
             else "0"
      go idx = if testBit int idx
               then "1" ++ go (idx - 1)
               else "0" ++ go (idx -1)



instance PositC III where
  type IntN III = Int64
  exponentSize = 3
 
  -- Posit Integer Rep of various values
  unReal = minBound @Int64
 
  mostPosVal = maxBound @Int64
  leastPosVal = 1
  leastNegVal = -1
  mostNegVal = negate mostPosVal
 
  encode Nothing = unReal @III
  encode (Just 0) = 0
  encode (Just r)
    | r > maxPosRat @III = mostPosVal @III
    | r < minNegRat @III = mostNegVal @III
    | r > 0 && r < minPosRat @III = leastPosVal @III
    | r < 0 && r > maxNegRat @III = leastNegVal @III
    | otherwise = buildIntRep @III r
 
  buildIntRep r =
    let (signBit,regime,exponent,significand) = posit2TupPosit @III r
        intRep = mkIntRep @III regime exponent significand
    in if signBit
       then negate intRep
       else intRep
 
  mkIntRep regime exponent significand =
    let (regime', offset) = formRegime @III regime  -- offset is the number of binary digits remaining after the regime is formed
        (exponent', offset') = formExponent @III exponent offset  -- offset' is the number of binary digits remaining after the exponent is formed
        fraction = formFraction @III significand offset'
    in regime' .|. exponent' .|. fraction
 
  formRegime power
    | 0 <= power =
      let offset = (fromIntegral (nBits @III - 1) -     power - 1)
      in (fromIntegral (2^(power + 1) - 1) `shiftL` fromInteger offset, offset - 1)
    | otherwise =
      let offset = (fromIntegral (nBits @III - 1) - abs power - 1)
      in (1 `shiftL` fromInteger offset, offset)
 
  formExponent power offset =
    let offset' = offset - fromIntegral (exponentSize @III)
    in (fromIntegral power `shift` fromInteger offset', offset')
 
  formFraction r offset =
    let numFractionBits = offset
        fractionSize = 2^numFractionBits
        normFraction = round $ (r - 1) * fractionSize  -- "posit - 1" is changing it from the significand to the fraction: [1,2) -> [0,1)
    in if numFractionBits >= 1
       then fromInteger normFraction
       else 0
 
  decode int
    | int == unReal @III = Nothing
    | int == 0 = Just 0
    | otherwise =
      let sgn = int < 0
          int' = if sgn
                 then negate int
                 else int
          (regime,nR) = regime2Integer @III int'
          exponent = exponent2Nat @III nR int'  -- if no e or some bits missing, then they are considered zero
          rat = fraction2Posit @III nR int'  -- if no fraction or some bits missing, then the missing bits are zero, making the significand p=1
      in tupPosit2Posit @III (sgn,regime,exponent,rat)
 
  regime2Integer posit =
    let regimeFormat = findRegimeFormat @III posit
        regimeCount = countRegimeBits @III regimeFormat posit
        regime = calcRegimeInt regimeFormat regimeCount
    in (regime, regimeCount + 1) -- a rational representation of the regime and the regimeCount plus rBar which is the numBitsRegime
 
  -- will return the format of the regime, either HI or LO; it could get refactored in the future
  -- True means a 1 is the first bit in the regime
  findRegimeFormat posit = testBit posit (fromIntegral (nBits @III) - 1 - fromIntegral (signBitSize @III))
 
  countRegimeBits format posit = go (fromIntegral (nBits @III) - 1 - fromIntegral (signBitSize @III)) 0
    where
      go (-1) acc = acc
      go index acc
        | xnor format (testBit posit index)  = go (index - 1) (acc + 1)
        | otherwise = acc
 
  -- knowing the number of the regime bits, and the sign bit we can extract
  -- the exponent.  We mask to the left of the exponent to remove the sign and regime, and
  -- then shift to the right to remove the fraction.
  exponent2Nat numBitsRegime posit =
    let bitsRemaining = fromIntegral (nBits @III) - numBitsRegime - fromIntegral (signBitSize @III)
        signNRegimeMask = 2^bitsRemaining - 1
        int = posit .&. signNRegimeMask
        nBitsToTheRight = fromIntegral (nBits @III) - numBitsRegime - fromIntegral (signBitSize @III) - fromIntegral (exponentSize @III)
    in if bitsRemaining <=0
       then 0
       else if nBitsToTheRight < 0
            then fromIntegral $ int `shiftL` negate nBitsToTheRight
            else fromIntegral $ int `shiftR` nBitsToTheRight
 
  -- knowing the number of the regime bits, sign bit, and the number of the
  -- exponent bits we can extract the fraction.  We mask to the left of the fraction to
  -- remove the sign, regime, and exponent. If there is no fraction then the value is 1.
  fraction2Posit numBitsRegime posit =
    let offset = fromIntegral $ (signBitSize @III) + fromIntegral numBitsRegime + (exponentSize @III)
        fractionSize = fromIntegral (nBits @III) - offset
        fractionBits = posit .&. (2^fractionSize - 1)
    in if fractionSize >= 1
       then (2^fractionSize + toInteger fractionBits) % 2^fractionSize
       else 1 % 1
 
  displayBin int = "0b" ++ go (fromIntegral (nBits @III) - 1)
    where
      go :: Int -> String
      go 0 = if testBit int 0
             then "1"
             else "0"
      go idx = if testBit int idx
               then "1" ++ go (idx - 1)
               else "0" ++ go (idx -1)



instance PositC IV where
  type IntN IV = Int128
  exponentSize = 4
 
  -- Posit Integer Rep of various values
  unReal = minBound @Int128
 
  mostPosVal = maxBound @Int128
  leastPosVal = 1
  leastNegVal = -1
  mostNegVal = negate mostPosVal
 
  encode Nothing = unReal @IV
  encode (Just 0) = 0
  encode (Just r)
    | r > maxPosRat @IV = mostPosVal @IV
    | r < minNegRat @IV = mostNegVal @IV
    | r > 0 && r < minPosRat @IV = leastPosVal @IV
    | r < 0 && r > maxNegRat @IV = leastNegVal @IV
    | otherwise = buildIntRep @IV r
 
  buildIntRep r =
    let (signBit,regime,exponent,significand) = posit2TupPosit @IV r
        intRep = mkIntRep @IV regime exponent significand
    in if signBit
       then negate intRep
       else intRep
 
  mkIntRep regime exponent significand =
    let (regime', offset) = formRegime @IV regime  -- offset is the number of binary digits remaining after the regime is formed
        (exponent', offset') = formExponent @IV exponent offset  -- offset' is the number of binary digits remaining after the exponent is formed
        fraction = formFraction @IV significand offset'
    in regime' .|. exponent' .|. fraction
 
  formRegime power
    | 0 <= power =
      let offset = (fromIntegral (nBits @IV - 1) -     power - 1)
      in (fromIntegral (2^(power + 1) - 1) `shiftL` fromInteger offset, offset - 1)
    | otherwise =
      let offset = (fromIntegral (nBits @IV - 1) - abs power - 1)
      in (1 `shiftL` fromInteger offset, offset)
 
  formExponent power offset =
    let offset' = offset - fromIntegral (exponentSize @IV)
    in (fromIntegral power `shift` fromInteger offset', offset')
 
  formFraction r offset =
    let numFractionBits = offset
        fractionSize = 2^numFractionBits
        normFraction = round $ (r - 1) * fractionSize  -- "posit - 1" is changing it from the significand to the fraction: [1,2) -> [0,1)
    in if numFractionBits >= 1
       then fromInteger normFraction
       else 0
 
  decode int
    | int == unReal @IV = Nothing
    | int == 0 = Just 0
    | otherwise =
      let sgn = int < 0
          int' = if sgn
                 then negate int
                 else int
          (regime,nR) = regime2Integer @IV int'
          exponent = exponent2Nat @IV nR int'  -- if no e or some bits missing, then they are considered zero
          rat = fraction2Posit @IV nR int'  -- if no fraction or some bits missing, then the missing bits are zero, making the significand p=1
      in tupPosit2Posit @IV (sgn,regime,exponent,rat)
 
  regime2Integer posit =
    let regimeFormat = findRegimeFormat @IV posit
        regimeCount = countRegimeBits @IV regimeFormat posit
        regime = calcRegimeInt regimeFormat regimeCount
    in (regime, regimeCount + 1) -- a rational representation of the regime and the regimeCount plus rBar which is the numBitsRegime
 
  -- will return the format of the regime, either HI or LO; it could get refactored in the future
  -- True means a 1 is the first bit in the regime
  findRegimeFormat posit = testBit posit (fromIntegral (nBits @IV) - 1 - fromIntegral (signBitSize @IV))
 
  countRegimeBits format posit = go (fromIntegral (nBits @IV) - 1 - fromIntegral (signBitSize @IV)) 0
    where
      go (-1) acc = acc
      go index acc
        | xnor format (testBit posit index)  = go (index - 1) (acc + 1)
        | otherwise = acc
 
  -- knowing the number of the regime bits, and the sign bit we can extract
  -- the exponent.  We mask to the left of the exponent to remove the sign and regime, and
  -- then shift to the right to remove the fraction.
  exponent2Nat numBitsRegime posit =
    let bitsRemaining = fromIntegral (nBits @IV) - numBitsRegime - fromIntegral (signBitSize @IV)
        signNRegimeMask = 2^bitsRemaining - 1
        int = posit .&. signNRegimeMask
        nBitsToTheRight = fromIntegral (nBits @IV) - numBitsRegime - fromIntegral (signBitSize @IV) - fromIntegral (exponentSize @IV)
    in if bitsRemaining <=0
       then 0
       else if nBitsToTheRight < 0
            then fromIntegral $ int `shiftL` negate nBitsToTheRight
            else fromIntegral $ int `shiftR` nBitsToTheRight
 
  -- knowing the number of the regime bits, sign bit, and the number of the
  -- exponent bits we can extract the fraction.  We mask to the left of the fraction to
  -- remove the sign, regime, and exponent. If there is no fraction then the value is 1.
  fraction2Posit numBitsRegime posit =
    let offset = fromIntegral $ (signBitSize @IV) + fromIntegral numBitsRegime + (exponentSize @IV)
        fractionSize = fromIntegral (nBits @IV) - offset
        fractionBits = posit .&. (2^fractionSize - 1)
    in if fractionSize >= 1
       then (2^fractionSize + toInteger fractionBits) % 2^fractionSize
       else 1 % 1
 
  displayBin int = "0b" ++ go (fromIntegral (nBits @IV) - 1)
    where
      go :: Int -> String
      go 0 = if testBit int 0
             then "1"
             else "0"
      go idx = if testBit int idx
               then "1" ++ go (idx - 1)
               else "0" ++ go (idx -1)



instance PositC V where
  type IntN V = Int256
  exponentSize = 5
 
  -- Posit Integer Rep of various values
  unReal = minBound @Int256
 
  mostPosVal = maxBound @Int256
  leastPosVal = 1
  leastNegVal = -1
  mostNegVal = negate mostPosVal
 
  encode Nothing = unReal @V
  encode (Just 0) = 0
  encode (Just r)
    | r > maxPosRat @V = mostPosVal @V
    | r < minNegRat @V = mostNegVal @V
    | r > 0 && r < minPosRat @V = leastPosVal @V
    | r < 0 && r > maxNegRat @V = leastNegVal @V
    | otherwise = buildIntRep @V r
 
  buildIntRep r =
    let (signBit,regime,exponent,significand) = posit2TupPosit @V r
        intRep = mkIntRep @V regime exponent significand
    in if signBit
       then negate intRep
       else intRep
 
  mkIntRep regime exponent significand =
    let (regime', offset) = formRegime @V regime  -- offset is the number of binary digits remaining after the regime is formed
        (exponent', offset') = formExponent @V exponent offset  -- offset' is the number of binary digits remaining after the exponent is formed
        fraction = formFraction @V significand offset'
    in regime' .|. exponent' .|. fraction
 
  formRegime power
    | 0 <= power =
      let offset = (fromIntegral (nBits @V - 1) -     power - 1)
      in (fromIntegral (2^(power + 1) - 1) `shiftL` fromInteger offset, offset - 1)
    | otherwise =
      let offset = (fromIntegral (nBits @V - 1) - abs power - 1)
      in (1 `shiftL` fromInteger offset, offset)
 
  formExponent power offset =
    let offset' = offset - fromIntegral (exponentSize @V)
    in (fromIntegral power `shift` fromInteger offset', offset')
 
  formFraction r offset =
    let numFractionBits = offset
        fractionSize = 2^numFractionBits
        normFraction = round $ (r - 1) * fractionSize  -- "posit - 1" is changing it from the significand to the fraction: [1,2) -> [0,1)
    in if numFractionBits >= 1
       then fromInteger normFraction
       else 0
 
  decode int
    | int == unReal @V = Nothing
    | int == 0 = Just 0
    | otherwise =
      let sgn = int < 0
          int' = if sgn
                 then negate int
                 else int
          (regime,nR) = regime2Integer @V int'
          exponent = exponent2Nat @V nR int'  -- if no e or some bits missing, then they are considered zero
          rat = fraction2Posit @V nR int'  -- if no fraction or some bits missing, then the missing bits are zero, making the significand p=1
      in tupPosit2Posit @V (sgn,regime,exponent,rat)
 
  regime2Integer posit =
    let regimeFormat = findRegimeFormat @V posit
        regimeCount = countRegimeBits @V regimeFormat posit
        regime = calcRegimeInt regimeFormat regimeCount
    in (regime, regimeCount + 1) -- a rational representation of the regime and the regimeCount plus rBar which is the numBitsRegime
 
  -- will return the format of the regime, either HI or LO; it could get refactored in the future
  -- True means a 1 is the first bit in the regime
  findRegimeFormat posit = testBit posit (fromIntegral (nBits @V) - 1 - fromIntegral (signBitSize @V))
 
  countRegimeBits format posit = go (fromIntegral (nBits @V) - 1 - fromIntegral (signBitSize @V)) 0
    where
      go (-1) acc = acc
      go index acc
        | xnor format (testBit posit index)  = go (index - 1) (acc + 1)
        | otherwise = acc
 
  -- knowing the number of the regime bits, and the sign bit we can extract
  -- the exponent.  We mask to the left of the exponent to remove the sign and regime, and
  -- then shift to the right to remove the fraction.
  exponent2Nat numBitsRegime posit =
    let bitsRemaining = fromIntegral (nBits @V) - numBitsRegime - fromIntegral (signBitSize @V)
        signNRegimeMask = 2^bitsRemaining - 1
        int = posit .&. signNRegimeMask
        nBitsToTheRight = fromIntegral (nBits @V) - numBitsRegime - fromIntegral (signBitSize @V) - fromIntegral (exponentSize @V)
    in if bitsRemaining <=0
       then 0
       else if nBitsToTheRight < 0
            then fromIntegral $ int `shiftL` negate nBitsToTheRight
            else fromIntegral $ int `shiftR` nBitsToTheRight
 
  -- knowing the number of the regime bits, sign bit, and the number of the
  -- exponent bits we can extract the fraction.  We mask to the left of the fraction to
  -- remove the sign, regime, and exponent. If there is no fraction then the value is 1.
  fraction2Posit numBitsRegime posit =
    let offset = fromIntegral $ (signBitSize @V) + fromIntegral numBitsRegime + (exponentSize @V)
        fractionSize = fromIntegral (nBits @V) - offset
        fractionBits = posit .&. (2^fractionSize - 1)
    in if fractionSize >= 1
       then (2^fractionSize + toInteger fractionBits) % 2^fractionSize
       else 1 % 1
 
  displayBin int = "0b" ++ go (fromIntegral (nBits @V) - 1)
    where
      go :: Int -> String
      go 0 = if testBit int 0
             then "1"
             else "0"
      go idx = if testBit int idx
               then "1" ++ go (idx - 1)
               else "0" ++ go (idx -1)


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

-- Exponent should be an integer in the range of [0,uSeed), and also return the posit [1,2)
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


#ifndef O_NO_ORPHANS
#ifndef O_NO_STORABLE
-- =====================================================================
-- ===                  Storable Instances                           ===
-- =====================================================================
--
-- Orphan Instance for Word128 using the DoubleWord type class
instance Storable Word128 where
  sizeOf _ = 16
  alignment _ = 16
  peek ptr = do
    hi <- peek $ offsetInt 0
    lo <- peek $ offsetWord 1
    return $ fromHiAndLo hi lo
      where
        offsetInt i = (castPtr ptr :: Ptr Word64) `plusPtr` (i*8)
        offsetWord i = (castPtr ptr :: Ptr Word64) `plusPtr` (i*8)
  poke ptr int = do
    poke (offsetInt 0) (hiWord int)
    poke (offsetWord 1) (loWord int)
      where
        offsetInt i = (castPtr ptr :: Ptr Word64) `plusPtr` (i*8)
        offsetWord i = (castPtr ptr :: Ptr Word64) `plusPtr` (i*8)

-- Orphan Instance for Int128 using the DoubleWord type class
instance Storable Int128 where
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

-- Orphan Instance for Int256 using the DoubleWord type class
instance Storable Int256 where
  sizeOf _ = 32
  alignment _ = 32
  peek ptr = do
    hi <- peek $ offsetInt 0
    lo <- peek $ offsetWord 1
    return $ fromHiAndLo hi lo
      where
        offsetInt i = (castPtr ptr :: Ptr Int128) `plusPtr` (i*16)
        offsetWord i = (castPtr ptr :: Ptr Word128) `plusPtr` (i*16)
  poke ptr int = do
    poke (offsetInt 0) (hiWord int)
    poke (offsetWord 1) (loWord int)
      where
        offsetInt i = (castPtr ptr :: Ptr Int128) `plusPtr` (i*16)
        offsetWord i = (castPtr ptr :: Ptr Word128) `plusPtr` (i*16)
--
#endif
#endif

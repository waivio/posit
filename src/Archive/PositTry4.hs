
--------------------------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2021 Nathan Waivio
-- License     :  BSD3
-- Maintainer  :  Nathan Waivio <nathan.waivio@gmail.com>
-- Stability   :  Stable
-- Portability :  unportable
--
-- Library implementing standard Posit Numbers
-- 
---------------------------------------------------------------------------------------------


{-# LANGUAGE TypeFamilyDependencies #-} -- For the associated bidirectional type family that the Posit library is based on
{-# LANGUAGE DataKinds #-}  -- For our ES kind and the constructors [Z,I,II,III,IV,V] for exponent size type
{-# LANGUAGE GADTs #-} -- For our highest level type Posit (es :: ES)
{-# LANGUAGE ViewPatterns #-}  -- To decode the posit in the pattern
{-# LANGUAGE FlexibleInstances #-} -- To make instances for each specific type [Posit8 .. Posit256]
{-# LANGUAGE TypeApplications #-} -- To apply types: @Type, it seems to select the specific class instance, when GHC is not able to reason about things
{-# LANGUAGE AllowAmbiguousTypes #-} -- The Haskell/GHC Type checker seems to have trouble things in the PositC class
{-# LANGUAGE MultiParamTypeClasses #-}  -- To convert between Posit Types
{-# LANGUAGE ScopedTypeVariables #-} -- To reduce some code duplication
{-# LANGUAGE FlexibleContexts #-} -- To reduce some code duplication by claiming the type family provides some constraints, that GHC can't do without fully evaluating the type family
{-# LANGUAGE UndecidableInstances #-}  -- To reduce some code duplicaiton, I think the code is decidable but GHC is not smart enough ;), like there being only 1 instance that is polymorphic and works for all of my types.
{-# LANGUAGE BangPatterns #-}  -- Added Strictness for some fixed point algorithms
{-# LANGUAGE PatternSynonyms #-}  -- for a nice NaR interface

-- ----
-- Posit numbers, implementing:
-- * Show
-- * Eq
-- * Ord
-- * Num
-- * Fractional
-- * Real
-- * Bounded
-- * FusedOps
-- * Convertable
-- * AltShow
-- * Read
-- * Storable
-- * RealFrac
-- * RealFloat
-- * Floating
-- ----

module Data.Posit
(Posit8,
 Posit16,
 Posit32,
 Posit64,
 Posit128,
 Posit256,
 pattern NaR) where


-- Imports for Show and Read Instances
import Data.Scientific (Scientific
                       ,scientificP
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

-- Imports for Storable Instance and Vectorization
import Data.Foldable (toList)  -- Used for fused operations on foldable/lists
import Foreign.Storable (Storable, sizeOf, alignment, peek, poke)  -- Used for Storable Instances of Posit
import Foreign.Ptr (Ptr, plusPtr, castPtr)  -- Used for dealing with Pointers for the Posit Storable Instance

-- 2's complement format integers:
import Data.Int (Int8,Int16,Int32,Int64)  -- Import standard Int sizes
import Data.DoubleWord (Word128,Int128,Int256,fromHiAndLo,hiWord,loWord) -- Import large Int sizes
import Data.Word (Word64)
import Data.Bits ((.|.), shiftL, shift, testBit, (.&.), shiftR)

import GHC.Natural (Natural) -- Import the Natural Numbers ℕ (u+2115)
import Data.Ratio (Rational, (%))  -- Import the Rational Numbers ℚ (u+211A), ℚ can get arbitrarily close to Real numbers ℝ (u+211D)

import Debug.Trace (trace) -- temporary for debug purposes

-- Exponent Size kind, 2^es bytes; The constructor for the Type is a Roman Numerial.
data ES = Z
        | I
        | II
        | III
        | IV
        | V

-- Base GADT rapper type, that uses the Exponent Size kind to index the various implementaitons
data Posit (es :: ES) where
     Posit :: PositC es => !(IntN es) -> Posit es

-- Not a Real Number
pattern NaR :: PositC es => Posit es
pattern NaR <- (Posit (decode -> Nothing)) where
  NaR = Posit unReal
--

-- Concrete types exported for use.
type Posit8 = Posit Z
type Posit16 = Posit I
type Posit32 = Posit II
type Posit64 = Posit III
type Posit128 = Posit IV
type Posit256 = Posit V

-- | The 'Posit' class is an approximation of ℝ, sort of an even sampling on the Projective Real line ℙ(ℝ) with Maybe ℚ as the internal type.
-- The 'es' is an index that controlls the log2 word size of the Posit's
-- fininte precision representation. 
class PositC (es :: ES) where
  -- Type of the Finite Precision Representation
  type IntN es = r | r -> es  -- The 'es' of kind 'ES' will determine a result of 'r' such that you can determine the 'es' by the 'r'
  
  -- Coerse to/from the Infinite Precision Representation
  encode :: Maybe Rational -> IntN es
  decode :: IntN es -> Maybe Rational
  
  -- Exponent Size based on the Posit Exponent kind ES
  exponentSize :: Natural
  
  -- Various other size definitions used in the Posit format with their default definitions
  -- 'nBytes' the number of bytes of the Posit Representation
  nBytes :: Natural
  nBytes = 2^(exponentSize @es)
  -- 'nBits' the number of bits of the Posit Representation
  nBits :: Natural
  nBits = 8 * (nBytes @es)
  -- 'signBitSize' the size of the sign bit
  signBitSize :: Natural
  signBitSize = 1
  -- 'uSeed' scaling factor for the regime of the Posit Representation
  uSeed :: Natural
  uSeed = 2^(nBytes @es)
  
  -- Integer Representation of common bounds
  unReal :: IntN es
  
  maxPositive :: IntN es
  minPositive :: IntN es
  maxNegative :: IntN es
  minNegative :: IntN es
  
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
    | r < fromRational (1 % 1) = log_uSeed @es (regime-1,r * fromRational (toInteger (uSeed @es) % 1))
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
    let pow2 = (toRational $ uSeed @es)^^regime * 2^exponent
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
  
  maxPositive = maxBound @Int8
  minPositive = 1
  maxNegative = -1
  minNegative = negate maxPositive
  
  encode (Nothing) = unReal @Z
  encode (Just 0) = 0
  encode (Just r)
    | r > maxPosRat @Z = maxPositive @Z
    | r < minNegRat @Z = minNegative @Z
    | r > 0 && r < minPosRat @Z = minPositive @Z
    | r < 0 && r > maxNegRat @Z = maxNegative @Z
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
            then fromIntegral $ int `shiftL` (negate nBitsToTheRight)
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
      go 0 = case (testBit int 0) of
               True -> "1"
               False -> "0"
      go idx = case (testBit int idx) of
                 True -> "1" ++ go (idx - 1)
                 False -> "0" ++ go (idx -1)



instance PositC I where
  type IntN I = Int16
  exponentSize = 1
  
  -- Posit Integer Rep of various values
  unReal = minBound @Int16
  
  maxPositive = maxBound @Int16
  minPositive = 1
  maxNegative = -1
  minNegative = negate maxPositive
  
  encode (Nothing) = unReal @I
  encode (Just 0) = 0
  encode (Just r)
    | r > maxPosRat @I = maxPositive @I
    | r < minNegRat @I = minNegative @I
    | r > 0 && r < minPosRat @I = minPositive @I
    | r < 0 && r > maxNegRat @I = maxNegative @I
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
            then fromIntegral $ int `shiftL` (negate nBitsToTheRight)
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
      go 0 = case (testBit int 0) of
               True -> "1"
               False -> "0"
      go idx = case (testBit int idx) of
                 True -> "1" ++ go (idx - 1)
                 False -> "0" ++ go (idx -1)



instance PositC II where
  type IntN II = Int32
  exponentSize = 2
  
  -- Posit Integer Rep of various values
  unReal = minBound @Int32
  
  maxPositive = maxBound @Int32
  minPositive = 1
  maxNegative = -1
  minNegative = negate maxPositive
  
  encode (Nothing) = unReal @II
  encode (Just 0) = 0
  encode (Just r)
    | r > maxPosRat @II = maxPositive @II
    | r < minNegRat @II = minNegative @II
    | r > 0 && r < minPosRat @II = minPositive @II
    | r < 0 && r > maxNegRat @II = maxNegative @II
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
            then fromIntegral $ int `shiftL` (negate nBitsToTheRight)
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
      go 0 = case (testBit int 0) of
               True -> "1"
               False -> "0"
      go idx = case (testBit int idx) of
                 True -> "1" ++ go (idx - 1)
                 False -> "0" ++ go (idx -1)



instance PositC III where
  type IntN III = Int64
  exponentSize = 3
  
  -- Posit Integer Rep of various values
  unReal = minBound @Int64
  
  maxPositive = maxBound @Int64
  minPositive = 1
  maxNegative = -1
  minNegative = negate maxPositive
  
  encode (Nothing) = unReal @III
  encode (Just 0) = 0
  encode (Just r)
    | r > maxPosRat @III = maxPositive @III
    | r < minNegRat @III = minNegative @III
    | r > 0 && r < minPosRat @III = minPositive @III
    | r < 0 && r > maxNegRat @III = maxNegative @III
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
            then fromIntegral $ int `shiftL` (negate nBitsToTheRight)
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
      go 0 = case (testBit int 0) of
               True -> "1"
               False -> "0"
      go idx = case (testBit int idx) of
                 True -> "1" ++ go (idx - 1)
                 False -> "0" ++ go (idx -1)



instance PositC IV where
  type IntN IV = Int128
  exponentSize = 4
  
  -- Posit Integer Rep of various values
  unReal = minBound @Int128
  
  maxPositive = maxBound @Int128
  minPositive = 1
  maxNegative = -1
  minNegative = negate maxPositive
  
  encode (Nothing) = unReal @IV
  encode (Just 0) = 0
  encode (Just r)
    | r > maxPosRat @IV = maxPositive @IV
    | r < minNegRat @IV = minNegative @IV
    | r > 0 && r < minPosRat @IV = minPositive @IV
    | r < 0 && r > maxNegRat @IV = maxNegative @IV
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
            then fromIntegral $ int `shiftL` (negate nBitsToTheRight)
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
      go 0 = case (testBit int 0) of
               True -> "1"
               False -> "0"
      go idx = case (testBit int idx) of
                 True -> "1" ++ go (idx - 1)
                 False -> "0" ++ go (idx -1)



instance PositC V where
  type IntN V = Int256
  exponentSize = 5
  
  -- Posit Integer Rep of various values
  unReal = minBound @Int256
  
  maxPositive = maxBound @Int256
  minPositive = 1
  maxNegative = -1
  minNegative = negate maxPositive
  
  encode (Nothing) = unReal @V
  encode (Just 0) = 0
  encode (Just r)
    | r > maxPosRat @V = maxPositive @V
    | r < minNegRat @V = minNegative @V
    | r > 0 && r < minPosRat @V = minPositive @V
    | r < 0 && r > maxNegRat @V = maxNegative @V
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
            then fromIntegral $ int `shiftL` (negate nBitsToTheRight)
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
      go 0 = case (testBit int 0) of
               True -> "1"
               False -> "0"
      go idx = case (testBit int idx) of
                 True -> "1" ++ go (idx - 1)
                 False -> "0" ++ go (idx -1)



-- Show
--
instance forall es. PositC es => Show (Posit es) where
  show NaR = "NaR"
  show (Posit (decode -> Just r)) =
    let (s,_) = fromRationalRepetendUnlimited r
    in formatScientific Generic (Just $ decimalPrec @es) s
--
{-
instance Show (Posit8) where
  show (Posit (decode -> Nothing)) = "NaR"
  show (Posit (decode -> Just r)) =
    let (s,_) = fromRationalRepetendUnlimited r
    in formatScientific Generic (Just $ decimalPrec @Z) s

instance Show (Posit16) where
  show (Posit (decode -> Nothing)) = "NaR"
  show (Posit (decode -> Just r)) =
    let (s,_) = fromRationalRepetendUnlimited r
    in formatScientific Generic (Just $ decimalPrec @I) s

instance Show (Posit32) where
  show (Posit (decode -> Nothing)) = "NaR"
  show (Posit (decode -> Just r)) =
    let (s,_) = fromRationalRepetendUnlimited r
    in formatScientific Generic (Just $ decimalPrec @II) s

instance Show (Posit64) where
  show (Posit (decode -> Nothing)) = "NaR"
  show (Posit (decode -> Just r)) =
    let (s,_) = fromRationalRepetendUnlimited r
    in formatScientific Generic (Just $ decimalPrec @III) s

instance Show (Posit128) where
  show (Posit (decode -> Nothing)) = "NaR"
  show (Posit (decode -> Just r)) =
    let (s,_) = fromRationalRepetendUnlimited r
    in formatScientific Generic (Just $ decimalPrec @IV) s

instance Show (Posit256) where
  show (Posit (decode -> Nothing)) = "NaR"
  show (Posit (decode -> Just r)) =
    let (s,_) = fromRationalRepetendUnlimited r
    in formatScientific Generic (Just $ decimalPrec @V) s
-}



-- Two Posit Numbers are Equal if their Finite Precision Integer representation is Equal
--
-- All things equal I would rather write it like this:
instance forall es. Eq (IntN es) => Eq (Posit es) where
  (Posit int1) == (Posit int2) = int1 == int2
--
{-
instance Eq (Posit8) where
  (Posit int1) == (Posit int2) = int1 == int2

instance Eq (Posit16) where
  (Posit int1) == (Posit int2) = int1 == int2

instance Eq (Posit32) where
  (Posit int1) == (Posit int2) = int1 == int2

instance Eq (Posit64) where
  (Posit int1) == (Posit int2) = int1 == int2

instance Eq (Posit128) where
  (Posit int1) == (Posit int2) = int1 == int2

instance Eq (Posit256) where
  (Posit int1) == (Posit int2) = int1 == int2
-}


-- Two Posit Numbers are ordered by their Finite Precision Integer representation
--
-- Ordinarilery I would only like one instance to cover them all
instance forall es. (Ord (IntN es), PositC es) => Ord (Posit es) where
  compare (Posit int1) (Posit int2) = compare int1 int2
--
{-
instance Ord (Posit8) where
  compare (Posit int1) (Posit int2) = compare int1 int2

instance Ord (Posit16) where
  compare (Posit int1) (Posit int2) = compare int1 int2

instance Ord (Posit32) where
  compare (Posit int1) (Posit int2) = compare int1 int2

instance Ord (Posit64) where
  compare (Posit int1) (Posit int2) = compare int1 int2

instance Ord (Posit128) where
  compare (Posit int1) (Posit int2) = compare int1 int2

instance Ord (Posit256) where
  compare (Posit int1) (Posit int2) = compare int1 int2
-}


-- Num
--
-- I'm num trying to get this definition:
instance forall es. (Num (IntN es), PositC es) => Num (Posit es) where
  -- Addition
  (Posit (decode -> Nothing)) + (Posit _) = Posit $ encode Nothing
  (Posit _) + (Posit (decode -> Nothing)) = Posit $ encode Nothing
  (Posit (decode -> Just r1)) + (Posit (decode -> Just r2)) = Posit $ encode (Just $ r1 + r2) 
  -- Multiplication
  NaR * (_) = NaR
  (_) * NaR = NaR
  (Posit (decode -> Just r1)) * (Posit (decode -> Just r2)) = Posit $ encode (Just $ r1 * r2)  
  -- 'abs', Absoulte Value, it's like a magnitude of sorts, abs of a posit is the same as abs of the integer representation
  abs (Posit int) = Posit $ abs int
  -- 'signum' it is a kind of an representation of directionality
  signum NaR = 0  -- I think it should be NaR, but the standard says zero
  signum (Posit (decode -> Just r)) = Posit $ encode (Just $ signum r)
  -- If you have an Integer, and want a Posit (es :: ES)? Use 'fromInteger'!
  fromInteger int = Posit $ encode (Just $ fromInteger int)
  -- 'negate', Negates the sign of the directionality. negate of a posit is the same as negate of the integer representation
  negate (Posit int) = Posit $ negate int
--
{-
instance Num (Posit8) where
  -- Addition
  (Posit (decode -> Nothing)) + (Posit _) = Posit $ encode Nothing
  (Posit _) + (Posit (decode -> Nothing)) = Posit $ encode Nothing
  (Posit (decode -> Just r1)) + (Posit (decode -> Just r2)) = Posit $ encode (Just $ r1 + r2) 
  -- Multiplication
  (Posit (decode -> Nothing)) * (Posit _) = Posit $ encode Nothing
  (Posit _) * (Posit (decode -> Nothing)) = Posit $ encode Nothing
  (Posit (decode -> Just r1)) * (Posit (decode -> Just r2)) = Posit $ encode (Just $ r1 * r2) 
  -- 'abs', Absoulte Value, it's like a magnitude of sorts, abs of a posit is the same as abs of the integer representation
  abs (Posit int) = Posit $ abs int
  -- 'signum' it is a kind of an representation of directionality
  signum (Posit (decode -> Nothing)) = Posit $ encode Nothing
  signum (Posit (decode -> Just r)) = Posit $ encode (Just $ signum r)
  -- If you have an Integer, and want a Posit (es :: ES)? Use 'fromInteger'!
  fromInteger int = Posit $ encode (Just $ fromInteger int)
  -- 'negate', Negates the sign of the directionality. negate of a posit is the same as negate of the integer representation
  negate (Posit int) = Posit $ negate int

instance Num (Posit16) where
  -- Addition
  (Posit (decode -> Nothing)) + (Posit _) = Posit $ encode Nothing
  (Posit _) + (Posit (decode -> Nothing)) = Posit $ encode Nothing
  (Posit (decode -> Just r1)) + (Posit (decode -> Just r2)) = Posit $ encode (Just $ r1 + r2) 
  -- Multiplication
  (Posit (decode -> Nothing)) * (Posit _) = Posit $ encode Nothing
  (Posit _) * (Posit (decode -> Nothing)) = Posit $ encode Nothing
  (Posit (decode -> Just r1)) * (Posit (decode -> Just r2)) = Posit $ encode (Just $ r1 * r2)  
  -- 'abs', Absoulte Value, it's like a magnitude of sorts, abs of a posit is the same as abs of the integer representation
  abs (Posit int) = Posit $ abs int
  -- 'signum' it is a kind of an representation of directionality
  signum (Posit (decode -> Nothing)) = Posit $ encode Nothing
  signum (Posit (decode -> Just r)) = Posit $ encode (Just $ signum r)
  -- If you have an Integer, and want a Posit (es :: ES)? Use 'fromInteger'!
  fromInteger int = Posit $ encode (Just $ fromInteger int)
  -- 'negate', Negates the sign of the directionality. negate of a posit is the same as negate of the integer representation
  negate (Posit int) = Posit $ negate int

instance Num (Posit32) where
  -- Addition
  (Posit (decode -> Nothing)) + (Posit _) = Posit $ encode Nothing
  (Posit _) + (Posit (decode -> Nothing)) = Posit $ encode Nothing
  (Posit (decode -> Just r1)) + (Posit (decode -> Just r2)) = Posit $ encode (Just $ r1 + r2) 
  -- Multiplication
  (Posit (decode -> Nothing)) * (Posit _) = Posit $ encode Nothing
  (Posit _) * (Posit (decode -> Nothing)) = Posit $ encode Nothing
  (Posit (decode -> Just r1)) * (Posit (decode -> Just r2)) = Posit $ encode (Just $ r1 * r2)  
  -- 'abs', Absoulte Value, it's like a magnitude of sorts, abs of a posit is the same as abs of the integer representation
  abs (Posit int) = Posit $ abs int
  -- 'signum' it is a kind of an representation of directionality
  signum (Posit (decode -> Nothing)) = Posit $ encode Nothing
  signum (Posit (decode -> Just r)) = Posit $ encode (Just $ signum r)
  -- If you have an Integer, and want a Posit (es :: ES)? Use 'fromInteger'!
  fromInteger int = Posit $ encode (Just $ fromInteger int)
  -- 'negate', Negates the sign of the directionality. negate of a posit is the same as negate of the integer representation
  negate (Posit int) = Posit $ negate int

instance Num (Posit64) where
  -- Addition
  (Posit (decode -> Nothing)) + (Posit _) = Posit $ encode Nothing
  (Posit _) + (Posit (decode -> Nothing)) = Posit $ encode Nothing
  (Posit (decode -> Just r1)) + (Posit (decode -> Just r2)) = Posit $ encode (Just $ r1 + r2) 
  -- Multiplication
  (Posit (decode -> Nothing)) * (Posit _) = Posit $ encode Nothing
  (Posit _) * (Posit (decode -> Nothing)) = Posit $ encode Nothing
  (Posit (decode -> Just r1)) * (Posit (decode -> Just r2)) = Posit $ encode (Just $ r1 * r2)  
  -- 'abs', Absoulte Value, it's like a magnitude of sorts, abs of a posit is the same as abs of the integer representation
  abs (Posit int) = Posit $ abs int
  -- 'signum' it is a kind of an representation of directionality
  signum (Posit (decode -> Nothing)) = Posit $ encode Nothing
  signum (Posit (decode -> Just r)) = Posit $ encode (Just $ signum r)
  -- If you have an Integer, and want a Posit (es :: ES)? Use 'fromInteger'!
  fromInteger int = Posit $ encode (Just $ fromInteger int)
  -- 'negate', Negates the sign of the directionality. negate of a posit is the same as negate of the integer representation
  negate (Posit int) = Posit $ negate int

instance Num (Posit128) where
  -- Addition
  (Posit (decode -> Nothing)) + (Posit _) = Posit $ encode Nothing
  (Posit _) + (Posit (decode -> Nothing)) = Posit $ encode Nothing
  (Posit (decode -> Just r1)) + (Posit (decode -> Just r2)) = Posit $ encode (Just $ r1 + r2) 
  -- Multiplication
  (Posit (decode -> Nothing)) * (Posit _) = Posit $ encode Nothing
  (Posit _) * (Posit (decode -> Nothing)) = Posit $ encode Nothing
  (Posit (decode -> Just r1)) * (Posit (decode -> Just r2)) = Posit $ encode (Just $ r1 * r2)  
  -- 'abs', Absoulte Value, it's like a magnitude of sorts, abs of a posit is the same as abs of the integer representation
  abs (Posit int) = Posit $ abs int
  -- 'signum' it is a kind of an representation of directionality
  signum (Posit (decode -> Nothing)) = Posit $ encode Nothing
  signum (Posit (decode -> Just r)) = Posit $ encode (Just $ signum r)
  -- If you have an Integer, and want a Posit (es :: ES)? Use 'fromInteger'!
  fromInteger int = Posit $ encode (Just $ fromInteger int)
  -- 'negate', Negates the sign of the directionality. negate of a posit is the same as negate of the integer representation
  negate (Posit int) = Posit $ negate int

instance Num (Posit256) where
  -- Addition
  (Posit (decode -> Nothing)) + (Posit _) = Posit $ encode Nothing
  (Posit _) + (Posit (decode -> Nothing)) = Posit $ encode Nothing
  (Posit (decode -> Just r1)) + (Posit (decode -> Just r2)) = Posit $ encode (Just $ r1 + r2) 
  -- Multiplication
  (Posit (decode -> Nothing)) * (Posit _) = Posit $ encode Nothing
  (Posit _) * (Posit (decode -> Nothing)) = Posit $ encode Nothing
  (Posit (decode -> Just r1)) * (Posit (decode -> Just r2)) = Posit $ encode (Just $ r1 * r2)  
  -- 'abs', Absoulte Value, it's like a magnitude of sorts, abs of a posit is the same as abs of the integer representation
  abs (Posit int) = Posit $ abs int
  -- 'signum' it is a kind of an representation of directionality
  signum (Posit (decode -> Nothing)) = Posit $ encode Nothing
  signum (Posit (decode -> Just r)) = Posit $ encode (Just $ signum r)
  -- If you have an Integer, and want a Posit (es :: ES)? Use 'fromInteger'!
  fromInteger int = Posit $ encode (Just $ fromInteger int)
  -- 'negate', Negates the sign of the directionality. negate of a posit is the same as negate of the integer representation
  negate (Posit int) = Posit $ negate int
-}


-- Fractional Instances; (Num => Fractional)
--
-- How the Frac do I get this definition:
instance forall es. (Num (IntN es), Ord (IntN es), Eq (IntN es), PositC es) => Fractional (Posit es) where
  fromRational r = Posit $ encode (Just r)
  
  recip NaR = NaR
  recip 0 = NaR
  recip (Posit (decode -> Just r))  = Posit $ encode (Just $ recip r)
--
{-
instance Fractional (Posit8) where
  fromRational r = Posit $ encode (Just r)
  
  recip (Posit (decode -> Nothing)) = Posit $ encode Nothing
  recip (Posit (decode -> Just 0)) = Posit $ encode Nothing
  recip (Posit (decode -> Just r))  = Posit $ encode (Just $ recip r)

instance Fractional (Posit16) where
  fromRational r = Posit $ encode (Just r)
  
  recip (Posit (decode -> Nothing)) = Posit $ encode Nothing
  recip (Posit (decode -> Just 0)) = Posit $ encode Nothing
  recip (Posit (decode -> Just r))  = Posit $ encode (Just $ recip r)

instance Fractional (Posit32) where
  fromRational r = Posit $ encode (Just r)
  
  recip (Posit (decode -> Nothing)) = Posit $ encode Nothing
  recip (Posit (decode -> Just 0)) = Posit $ encode Nothing
  recip (Posit (decode -> Just r))  = Posit $ encode (Just $ recip r)

instance Fractional (Posit64) where
  fromRational r = Posit $ encode (Just r)
  
  recip (Posit (decode -> Nothing)) = Posit $ encode Nothing
  recip (Posit (decode -> Just 0)) = Posit $ encode Nothing
  recip (Posit (decode -> Just r))  = Posit $ encode (Just $ recip r)

instance Fractional (Posit128) where
  fromRational r = Posit $ encode (Just r)
  
  recip (Posit (decode -> Nothing)) = Posit $ encode Nothing
  recip (Posit (decode -> Just 0)) = Posit $ encode Nothing
  recip (Posit (decode -> Just r))  = Posit $ encode (Just $ recip r)

instance Fractional (Posit256) where
  fromRational r = Posit $ encode (Just r)
  
  recip (Posit (decode -> Nothing)) = Posit $ encode Nothing
  recip (Posit (decode -> Just 0)) = Posit $ encode Nothing
  recip (Posit (decode -> Just r))  = Posit $ encode (Just $ recip r)
-}


-- Rational Instances; Num & Ord Instanced => Real
--
-- I for real want this definition:
instance forall es. (Num (IntN es), Ord (IntN es), PositC es) => Real (Posit es) where
  toRational NaR = error "Your input is not Rational"
  toRational (Posit (decode -> Just r)) = r
--
{-
instance Real (Posit8) where
  toRational (Posit (decode -> Nothing)) = error "Your input is not Rational"
  toRational (Posit (decode -> Just r)) = r

instance Real (Posit16) where
  toRational (Posit (decode -> Nothing)) = error "Your input is not Rational"
  toRational (Posit (decode -> Just r)) = r

instance Real (Posit32) where
  toRational (Posit (decode -> Nothing)) = error "Your input is not Rational"
  toRational (Posit (decode -> Just r)) = r

instance Real (Posit64) where
  toRational (Posit (decode -> Nothing)) = error "Your input is not Rational"
  toRational (Posit (decode -> Just r)) = r

instance Real (Posit128) where
  toRational (Posit (decode -> Nothing)) = error "Your input is not Rational"
  toRational (Posit (decode -> Just r)) = r

instance Real (Posit256) where
  toRational (Posit (decode -> Nothing)) = error "Your input is not Rational"
  toRational (Posit (decode -> Just r)) = r
-}

-- Bounded, bounded to what?!? To the ℝ! NaR is out of bounds!!!
--
-- I'm bound to want this definition:
instance forall es. PositC es => Bounded (Posit es) where
  -- 'minBound'
  minBound = Posit $ minNegative
  -- 'maxBound'
  maxBound = Posit $ maxPositive
--
{-
instance Bounded (Posit8) where
  -- 'minBound'
  minBound = Posit $ minNegative -- minBound int is NaR but 1 more than that is minBound posit
  -- 'maxBound'
  maxBound = Posit $ maxPositive

instance Bounded (Posit16) where
  -- 'minBound'
  minBound = Posit $ minNegative -- minBound int is NaR but 1 more than that is minBound posit
  -- 'maxBound'
  maxBound = Posit $ maxPositive

instance Bounded (Posit32) where
  -- 'minBound'
  minBound = Posit $ minNegative -- minBound int is NaR but 1 more than that is minBound posit
  -- 'maxBound'
  maxBound = Posit $ maxPositive

instance Bounded (Posit64) where
  -- 'minBound'
  minBound = Posit $ minNegative -- minBound int is NaR but 1 more than that is minBound posit
  -- 'maxBound'
  maxBound = Posit $ maxPositive

instance Bounded (Posit128) where
  -- 'minBound'
  minBound = Posit $ minNegative -- minBound int is NaR but 1 more than that is minBound posit
  -- 'maxBound'
  maxBound = Posit $ maxPositive

instance Bounded (Posit256) where
  -- 'minBound'
  minBound = Posit $ minNegative -- minBound int is NaR but 1 more than that is minBound posit
  -- 'maxBound'
  maxBound = Posit $ maxPositive
-}

-- =====================================================================
-- ===                Encode and Decode Helpers                      ===
-- =====================================================================


-- getSign finds the sign value and then returns the absolute value of the Posit
getSign :: Rational -> (Bool, Rational)
getSign r =
  let s = r <= 0
      absPosit = if s
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






-- =====================================================================
-- ===                    Fused Operations                           ===
-- =====================================================================

-- A class that delays the rounding operation until the end for some operations
class Num a => FusedOps a where
  fma :: a -> a -> a -> a  -- Fused Multiply Add: (a * b) + c
  fam :: a -> a -> a -> a  -- Fused Add Multiply: (a + b) * c
  fmms :: a -> a -> a -> a -> a  -- Fused Multiply Multiply Subtract: (a * b) - (c * d)
  fsum3 :: a -> a -> a -> a  -- Fused Sum of 3 values: a + b + c
  fsum4 :: a -> a -> a -> a -> a  -- Fused Sum of 4 values: a + b + c + d
  fsumL :: Foldable t => t a -> a  -- Fused Sum of a List of Posits
  fdot3 :: a -> a -> a -> a -> a -> a -> a  -- Fused Dot Product of 3 element vector: (a1 * b1) + (a2 * b2) + (a3 * b3)
  fdot4 :: a -> a -> a -> a -> a -> a -> a -> a -> a  -- Fused Dot Product of 4 element veector: (a0 * b0) + (a1 * b1) + (a2 * b2) + (a3 * b3)
  fdotL :: Foldable t => t a -> t a -> a  -- Fused Dot Product of Two Lists
  fsm :: a -> a -> a -> a -- Fused Subtract Multiply: a - (b * c)

--
instance forall es. (Num (IntN es), PositC es) => FusedOps (Posit es) where
  -- Fused Subtract Multiply
  fsm NaR (_) (_) = NaR
  fsm (_) NaR (_) = NaR
  fsm (_) (_) NaR = NaR
  fsm (Posit (decode -> Just a)) (Posit (decode -> Just b)) (Posit (decode -> Just c)) = Posit $ encode (Just $ a - (b * c))
  -- Fuse Multiply Add
  fma NaR (_) (_) = NaR
  fma (_) NaR (_) = NaR
  fma (_) (_) NaR = NaR
  fma (Posit (decode -> Just a)) (Posit (decode -> Just b)) (Posit (decode -> Just c)) = Posit $ encode (Just $ (a * b) + c)
  -- Fuse Add Multiply
  fam NaR (_) (_) = NaR
  fam (_) NaR (_) = NaR
  fam (_) (_) NaR = NaR
  fam (Posit (decode -> Just a)) (Posit (decode -> Just b)) (Posit (decode -> Just c)) = Posit $ encode (Just $ (a + b) * c)
  -- Fuse Multiply Multiply Subtract
  fmms NaR (_) (_) (_) = NaR
  fmms (_) NaR (_) (_) = NaR
  fmms (_) (_) NaR (_) = NaR
  fmms (_) (_) (_) NaR = NaR
  fmms (Posit (decode -> Just a)) (Posit (decode -> Just b)) (Posit (decode -> Just c)) (Posit (decode -> Just d)) = Posit $ encode (Just $ (a * b) - (c * d))
  -- Fuse Sum of 3 Posits
  fsum3 NaR (_) (_) = NaR
  fsum3 (_) NaR (_) = NaR
  fsum3 (_) (_) NaR = NaR
  fsum3 (Posit (decode -> Just a)) (Posit (decode -> Just b)) (Posit (decode -> Just c)) = Posit $ encode (Just $ a + b + c)
  -- Fuse Sum of 4 Posits
  fsum4 NaR (_) (_) (_) = NaR
  fsum4 (_) NaR (_) (_) = NaR
  fsum4 (_) (_) NaR (_) = NaR
  fsum4 (_) (_) (_) NaR = NaR
  fsum4 (Posit (decode -> Just a)) (Posit (decode -> Just b)) (Posit (decode -> Just c)) (Posit (decode -> Just d)) = Posit $ encode (Just $ a + b + c + d)
  -- Fuse Sum of a List
  fsumL (toList -> l) = Posit $ encode (Just $ go l 0)
    where
      go [] acc = acc
      go ((Posit int) : xs) acc = case (decode int) of
                                    Nothing -> error "Posit List contains NaR"
                                    Just r -> go xs (acc + r)
  -- Fuse Dot Product of a 3-Vector
  fdot3 NaR (_) (_) (_) (_) (_) = NaR
  fdot3 (_) NaR (_) (_) (_) (_) = NaR
  fdot3 (_) (_) NaR (_) (_) (_) = NaR
  fdot3 (_) (_) (_) NaR (_) (_) = NaR
  fdot3 (_) (_) (_) (_) NaR (_) = NaR
  fdot3 (_) (_) (_) (_) (_) NaR = NaR
  fdot3 (Posit (decode -> Just a1)) (Posit (decode -> Just a2)) (Posit (decode -> Just a3))
        (Posit (decode -> Just b1)) (Posit (decode -> Just b2)) (Posit (decode -> Just b3)) = Posit $ encode (Just $ (a1 * b1) + (a2 * b2) + (a3 * b3))
  -- Fuse Dot Product of a 4-Vector
  fdot4 NaR (_) (_) (_) (_) (_) (_) (_) = NaR
  fdot4 (_) NaR (_) (_) (_) (_) (_) (_) = NaR
  fdot4 (_) (_) NaR (_) (_) (_) (_) (_) = NaR
  fdot4 (_) (_) (_) NaR (_) (_) (_) (_) = NaR
  fdot4 (_) (_) (_) (_) NaR (_) (_) (_) = NaR
  fdot4 (_) (_) (_) (_) (_) NaR (_) (_) = NaR
  fdot4 (_) (_) (_) (_) (_) (_) NaR (_) = NaR
  fdot4 (_) (_) (_) (_) (_) (_) (_) NaR = NaR
  fdot4 (Posit (decode -> Just a0)) (Posit (decode -> Just a1)) (Posit (decode -> Just a2)) (Posit (decode -> Just a3))
        (Posit (decode -> Just b0)) (Posit (decode -> Just b1)) (Posit (decode -> Just b2)) (Posit (decode -> Just b3)) = Posit $ encode (Just $ (a0 * b0) + (a1 * b1) + (a2 * b2) + (a3 * b3))
  -- Fuse Dot Product of two Lists
  fdotL (toList -> l1) (toList -> l2) = Posit $ encode (Just $ go l1 l2 0)
    where
      go [] [] acc = acc
      go [] (_) _ = error "Lists not the same length"
      go (_) [] _ = error "Lists not the same length"
      go ((Posit int1) : bs) ((Posit int2) : cs) acc = case (decode int1) of
                                                         Nothing -> error "First Posit List contains NaR"
                                                         Just r1 -> case (decode int2) of
                                                                      Nothing -> error "Second Posit List contains NaR"
                                                                      Just r2 -> go bs cs (acc + (r1 * r2))
--

{-
instance FusedOps (Posit8) where
  -- Fuse Multiply Add
  fma (Posit (decode -> Nothing)) (_) (_) = Posit $ encode Nothing
  fma (_) (Posit (decode -> Nothing)) (_) = Posit $ encode Nothing
  fma (_) (_) (Posit (decode -> Nothing)) = Posit $ encode Nothing
  fma (Posit (decode -> Just a)) (Posit (decode -> Just b)) (Posit (decode -> Just c)) = Posit $ encode (Just $ (a * b) + c)
  -- Fuse Add Multiply
  fam (Posit (decode -> Nothing)) (_) (_) = Posit $ encode Nothing
  fam (_) (Posit (decode -> Nothing)) (_) = Posit $ encode Nothing
  fam (_) (_) (Posit (decode -> Nothing)) = Posit $ encode Nothing
  fam (Posit (decode -> Just a)) (Posit (decode -> Just b)) (Posit (decode -> Just c)) = Posit $ encode (Just $ (a + b) * c)
  -- Fuse Multiply Multiply Subtract
  fmms (Posit (decode -> Nothing)) (_) (_) (_) = Posit $ encode Nothing
  fmms (_) (Posit (decode -> Nothing)) (_) (_) = Posit $ encode Nothing
  fmms (_) (_) (Posit (decode -> Nothing)) (_) = Posit $ encode Nothing
  fmms (_) (_) (_) (Posit (decode -> Nothing)) = Posit $ encode Nothing
  fmms (Posit (decode -> Just a)) (Posit (decode -> Just b)) (Posit (decode -> Just c)) (Posit (decode -> Just d)) = Posit $ encode (Just $ (a * b) - (c * d))
  -- Fuse Sum of 3 Posits
  fsum3 (Posit (decode -> Nothing)) (_) (_) = Posit $ encode Nothing
  fsum3 (_) (Posit (decode -> Nothing)) (_) = Posit $ encode Nothing
  fsum3 (_) (_) (Posit (decode -> Nothing)) = Posit $ encode Nothing
  fsum3 (Posit (decode -> Just a)) (Posit (decode -> Just b)) (Posit (decode -> Just c)) = Posit $ encode (Just $ a + b + c)
  -- Fuse Sum of 4 Posits
  fsum4 (Posit (decode -> Nothing)) (_) (_) (_) = Posit $ encode Nothing
  fsum4 (_) (Posit (decode -> Nothing)) (_) (_) = Posit $ encode Nothing
  fsum4 (_) (_) (Posit (decode -> Nothing)) (_) = Posit $ encode Nothing
  fsum4 (_) (_) (_) (Posit (decode -> Nothing)) = Posit $ encode Nothing
  fsum4 (Posit (decode -> Just a)) (Posit (decode -> Just b)) (Posit (decode -> Just c)) (Posit (decode -> Just d)) = Posit $ encode (Just $ a + b + c + d)
  -- Fuse Sum of a List
  fsumL (toList -> l) = Posit $ encode (Just $ go l 0)
    where
      go [] acc = acc
      go ((Posit int) : xs) acc = case (decode int) of
                                    Nothing -> error "Posit List contains NaR"
                                    Just r -> go xs (acc + r)
  -- Fuse Dot Product of a 3-Vector
  fdot3 (Posit (decode -> Nothing)) (_) (_) (_) (_) (_) = Posit $ encode Nothing
  fdot3 (_) (Posit (decode -> Nothing)) (_) (_) (_) (_) = Posit $ encode Nothing
  fdot3 (_) (_) (Posit (decode -> Nothing)) (_) (_) (_) = Posit $ encode Nothing
  fdot3 (_) (_) (_) (Posit (decode -> Nothing)) (_) (_) = Posit $ encode Nothing
  fdot3 (_) (_) (_) (_) (Posit (decode -> Nothing)) (_) = Posit $ encode Nothing
  fdot3 (_) (_) (_) (_) (_) (Posit (decode -> Nothing)) = Posit $ encode Nothing
  fdot3 (Posit (decode -> Just a1)) (Posit (decode -> Just a2)) (Posit (decode -> Just a3)) (Posit (decode -> Just b1)) (Posit (decode -> Just b2)) (Posit (decode -> Just b3)) = Posit $ encode (Just $ (a1 * b1) + (a2 * b2) + (a3 * b3))
  -- Fuse Dot Product of a 4-Vector
  fdot4 (Posit (decode -> Nothing)) (_) (_) (_) (_) (_) (_) (_) = Posit $ encode Nothing
  fdot4 (_) (Posit (decode -> Nothing)) (_) (_) (_) (_) (_) (_) = Posit $ encode Nothing
  fdot4 (_) (_) (Posit (decode -> Nothing)) (_) (_) (_) (_) (_) = Posit $ encode Nothing
  fdot4 (_) (_) (_) (Posit (decode -> Nothing)) (_) (_) (_) (_) = Posit $ encode Nothing
  fdot4 (_) (_) (_) (_) (Posit (decode -> Nothing)) (_) (_) (_) = Posit $ encode Nothing
  fdot4 (_) (_) (_) (_) (_) (Posit (decode -> Nothing)) (_) (_) = Posit $ encode Nothing
  fdot4 (_) (_) (_) (_) (_) (_) (Posit (decode -> Nothing)) (_) = Posit $ encode Nothing
  fdot4 (_) (_) (_) (_) (_) (_) (_) (Posit (decode -> Nothing)) = Posit $ encode Nothing
  fdot4 (Posit (decode -> Just a0)) (Posit (decode -> Just a1)) (Posit (decode -> Just a2)) (Posit (decode -> Just a3)) (Posit (decode -> Just b0)) (Posit (decode -> Just b1)) (Posit (decode -> Just b2)) (Posit (decode -> Just b3)) = Posit $ encode (Just $ (a0 * b0) + (a1 * b1) + (a2 * b2) + (a3 * b3))
  -- Fuse Dot Product of two Lists
  fdotL (toList -> l1) (toList -> l2) = Posit $ encode (Just $ go l1 l2 0)
    where
      go [] [] acc = acc
      go [] (_) _ = error "Lists not the same length"
      go (_) [] _ = error "Lists not the same length"
      go ((Posit int1) : bs) ((Posit int2) : cs) acc = case (decode int1) of
                                                         Nothing -> error "First Posit List contains NaR"
                                                         Just r1 -> case (decode int2) of
                                                                      Nothing -> error "Second Posit List contains NaR"
                                                                      Just r2 -> go bs cs (acc + (r1 * r2))

instance FusedOps (Posit16) where
  -- Fuse Multiply Add
  fma (Posit (decode -> Nothing)) (_) (_) = Posit $ encode Nothing
  fma (_) (Posit (decode -> Nothing)) (_) = Posit $ encode Nothing
  fma (_) (_) (Posit (decode -> Nothing)) = Posit $ encode Nothing
  fma (Posit (decode -> Just a)) (Posit (decode -> Just b)) (Posit (decode -> Just c)) = Posit $ encode (Just $ (a * b) + c)
  -- Fuse Add Multiply
  fam (Posit (decode -> Nothing)) (_) (_) = Posit $ encode Nothing
  fam (_) (Posit (decode -> Nothing)) (_) = Posit $ encode Nothing
  fam (_) (_) (Posit (decode -> Nothing)) = Posit $ encode Nothing
  fam (Posit (decode -> Just a)) (Posit (decode -> Just b)) (Posit (decode -> Just c)) = Posit $ encode (Just $ (a + b) * c)
  -- Fuse Multiply Multiply Subtract
  fmms (Posit (decode -> Nothing)) (_) (_) (_) = Posit $ encode Nothing
  fmms (_) (Posit (decode -> Nothing)) (_) (_) = Posit $ encode Nothing
  fmms (_) (_) (Posit (decode -> Nothing)) (_) = Posit $ encode Nothing
  fmms (_) (_) (_) (Posit (decode -> Nothing)) = Posit $ encode Nothing
  fmms (Posit (decode -> Just a)) (Posit (decode -> Just b)) (Posit (decode -> Just c)) (Posit (decode -> Just d)) = Posit $ encode (Just $ (a * b) - (c * d))
  -- Fuse Sum of 3 Posits
  fsum3 (Posit (decode -> Nothing)) (_) (_) = Posit $ encode Nothing
  fsum3 (_) (Posit (decode -> Nothing)) (_) = Posit $ encode Nothing
  fsum3 (_) (_) (Posit (decode -> Nothing)) = Posit $ encode Nothing
  fsum3 (Posit (decode -> Just a)) (Posit (decode -> Just b)) (Posit (decode -> Just c)) = Posit $ encode (Just $ a + b + c)
  -- Fuse Sum of 4 Posits
  fsum4 (Posit (decode -> Nothing)) (_) (_) (_) = Posit $ encode Nothing
  fsum4 (_) (Posit (decode -> Nothing)) (_) (_) = Posit $ encode Nothing
  fsum4 (_) (_) (Posit (decode -> Nothing)) (_) = Posit $ encode Nothing
  fsum4 (_) (_) (_) (Posit (decode -> Nothing)) = Posit $ encode Nothing
  fsum4 (Posit (decode -> Just a)) (Posit (decode -> Just b)) (Posit (decode -> Just c)) (Posit (decode -> Just d)) = Posit $ encode (Just $ a + b + c + d)
  -- Fuse Sum of a List
  fsumL (toList -> l) = Posit $ encode (Just $ go l 0)
    where
      go [] acc = acc
      go ((Posit int) : xs) acc = case (decode int) of
                                    Nothing -> error "Posit List contains NaR"
                                    Just r -> go xs (acc + r)
  -- Fuse Dot Product of a 3-Vector
  fdot3 (Posit (decode -> Nothing)) (_) (_) (_) (_) (_) = Posit $ encode Nothing
  fdot3 (_) (Posit (decode -> Nothing)) (_) (_) (_) (_) = Posit $ encode Nothing
  fdot3 (_) (_) (Posit (decode -> Nothing)) (_) (_) (_) = Posit $ encode Nothing
  fdot3 (_) (_) (_) (Posit (decode -> Nothing)) (_) (_) = Posit $ encode Nothing
  fdot3 (_) (_) (_) (_) (Posit (decode -> Nothing)) (_) = Posit $ encode Nothing
  fdot3 (_) (_) (_) (_) (_) (Posit (decode -> Nothing)) = Posit $ encode Nothing
  fdot3 (Posit (decode -> Just a1)) (Posit (decode -> Just a2)) (Posit (decode -> Just a3)) (Posit (decode -> Just b1)) (Posit (decode -> Just b2)) (Posit (decode -> Just b3)) = Posit $ encode (Just $ (a1 * b1) + (a2 * b2) + (a3 * b3))
  -- Fuse Dot Product of a 4-Vector
  fdot4 (Posit (decode -> Nothing)) (_) (_) (_) (_) (_) (_) (_) = Posit $ encode Nothing
  fdot4 (_) (Posit (decode -> Nothing)) (_) (_) (_) (_) (_) (_) = Posit $ encode Nothing
  fdot4 (_) (_) (Posit (decode -> Nothing)) (_) (_) (_) (_) (_) = Posit $ encode Nothing
  fdot4 (_) (_) (_) (Posit (decode -> Nothing)) (_) (_) (_) (_) = Posit $ encode Nothing
  fdot4 (_) (_) (_) (_) (Posit (decode -> Nothing)) (_) (_) (_) = Posit $ encode Nothing
  fdot4 (_) (_) (_) (_) (_) (Posit (decode -> Nothing)) (_) (_) = Posit $ encode Nothing
  fdot4 (_) (_) (_) (_) (_) (_) (Posit (decode -> Nothing)) (_) = Posit $ encode Nothing
  fdot4 (_) (_) (_) (_) (_) (_) (_) (Posit (decode -> Nothing)) = Posit $ encode Nothing
  fdot4 (Posit (decode -> Just a0)) (Posit (decode -> Just a1)) (Posit (decode -> Just a2)) (Posit (decode -> Just a3)) (Posit (decode -> Just b0)) (Posit (decode -> Just b1)) (Posit (decode -> Just b2)) (Posit (decode -> Just b3)) = Posit $ encode (Just $ (a0 * b0) + (a1 * b1) + (a2 * b2) + (a3 * b3))
  -- Fuse Dot Product of two Lists
  fdotL (toList -> l1) (toList -> l2) = Posit $ encode (Just $ go l1 l2 0)
    where
      go [] [] acc = acc
      go [] (_) _ = error "Lists not the same length"
      go (_) [] _ = error "Lists not the same length"
      go ((Posit int1) : bs) ((Posit int2) : cs) acc = case (decode int1) of
                                                         Nothing -> error "First Posit List contains NaR"
                                                         Just r1 -> case (decode int2) of
                                                                      Nothing -> error "Second Posit List contains NaR"
                                                                      Just r2 -> go bs cs (acc + (r1 * r2))

instance FusedOps (Posit32) where
  -- Fuse Multiply Add
  fma (Posit (decode -> Nothing)) (_) (_) = Posit $ encode Nothing
  fma (_) (Posit (decode -> Nothing)) (_) = Posit $ encode Nothing
  fma (_) (_) (Posit (decode -> Nothing)) = Posit $ encode Nothing
  fma (Posit (decode -> Just a)) (Posit (decode -> Just b)) (Posit (decode -> Just c)) = Posit $ encode (Just $ (a * b) + c)
  -- Fuse Add Multiply
  fam (Posit (decode -> Nothing)) (_) (_) = Posit $ encode Nothing
  fam (_) (Posit (decode -> Nothing)) (_) = Posit $ encode Nothing
  fam (_) (_) (Posit (decode -> Nothing)) = Posit $ encode Nothing
  fam (Posit (decode -> Just a)) (Posit (decode -> Just b)) (Posit (decode -> Just c)) = Posit $ encode (Just $ (a + b) * c)
  -- Fuse Multiply Multiply Subtract
  fmms (Posit (decode -> Nothing)) (_) (_) (_) = Posit $ encode Nothing
  fmms (_) (Posit (decode -> Nothing)) (_) (_) = Posit $ encode Nothing
  fmms (_) (_) (Posit (decode -> Nothing)) (_) = Posit $ encode Nothing
  fmms (_) (_) (_) (Posit (decode -> Nothing)) = Posit $ encode Nothing
  fmms (Posit (decode -> Just a)) (Posit (decode -> Just b)) (Posit (decode -> Just c)) (Posit (decode -> Just d)) = Posit $ encode (Just $ (a * b) - (c * d))
  -- Fuse Sum of 3 Posits
  fsum3 (Posit (decode -> Nothing)) (_) (_) = Posit $ encode Nothing
  fsum3 (_) (Posit (decode -> Nothing)) (_) = Posit $ encode Nothing
  fsum3 (_) (_) (Posit (decode -> Nothing)) = Posit $ encode Nothing
  fsum3 (Posit (decode -> Just a)) (Posit (decode -> Just b)) (Posit (decode -> Just c)) = Posit $ encode (Just $ a + b + c)
  -- Fuse Sum of 4 Posits
  fsum4 (Posit (decode -> Nothing)) (_) (_) (_) = Posit $ encode Nothing
  fsum4 (_) (Posit (decode -> Nothing)) (_) (_) = Posit $ encode Nothing
  fsum4 (_) (_) (Posit (decode -> Nothing)) (_) = Posit $ encode Nothing
  fsum4 (_) (_) (_) (Posit (decode -> Nothing)) = Posit $ encode Nothing
  fsum4 (Posit (decode -> Just a)) (Posit (decode -> Just b)) (Posit (decode -> Just c)) (Posit (decode -> Just d)) = Posit $ encode (Just $ a + b + c + d)
  -- Fuse Sum of a List
  fsumL (toList -> l) = Posit $ encode (Just $ go l 0)
    where
      go [] acc = acc
      go ((Posit int) : xs) acc = case (decode int) of
                                    Nothing -> error "Posit List contains NaR"
                                    Just r -> go xs (acc + r)
  -- Fuse Dot Product of a 3-Vector
  fdot3 (Posit (decode -> Nothing)) (_) (_) (_) (_) (_) = Posit $ encode Nothing
  fdot3 (_) (Posit (decode -> Nothing)) (_) (_) (_) (_) = Posit $ encode Nothing
  fdot3 (_) (_) (Posit (decode -> Nothing)) (_) (_) (_) = Posit $ encode Nothing
  fdot3 (_) (_) (_) (Posit (decode -> Nothing)) (_) (_) = Posit $ encode Nothing
  fdot3 (_) (_) (_) (_) (Posit (decode -> Nothing)) (_) = Posit $ encode Nothing
  fdot3 (_) (_) (_) (_) (_) (Posit (decode -> Nothing)) = Posit $ encode Nothing
  fdot3 (Posit (decode -> Just a1)) (Posit (decode -> Just a2)) (Posit (decode -> Just a3)) (Posit (decode -> Just b1)) (Posit (decode -> Just b2)) (Posit (decode -> Just b3)) = Posit $ encode (Just $ (a1 * b1) + (a2 * b2) + (a3 * b3))
  -- Fuse Dot Product of a 4-Vector
  fdot4 (Posit (decode -> Nothing)) (_) (_) (_) (_) (_) (_) (_) = Posit $ encode Nothing
  fdot4 (_) (Posit (decode -> Nothing)) (_) (_) (_) (_) (_) (_) = Posit $ encode Nothing
  fdot4 (_) (_) (Posit (decode -> Nothing)) (_) (_) (_) (_) (_) = Posit $ encode Nothing
  fdot4 (_) (_) (_) (Posit (decode -> Nothing)) (_) (_) (_) (_) = Posit $ encode Nothing
  fdot4 (_) (_) (_) (_) (Posit (decode -> Nothing)) (_) (_) (_) = Posit $ encode Nothing
  fdot4 (_) (_) (_) (_) (_) (Posit (decode -> Nothing)) (_) (_) = Posit $ encode Nothing
  fdot4 (_) (_) (_) (_) (_) (_) (Posit (decode -> Nothing)) (_) = Posit $ encode Nothing
  fdot4 (_) (_) (_) (_) (_) (_) (_) (Posit (decode -> Nothing)) = Posit $ encode Nothing
  fdot4 (Posit (decode -> Just a0)) (Posit (decode -> Just a1)) (Posit (decode -> Just a2)) (Posit (decode -> Just a3)) (Posit (decode -> Just b0)) (Posit (decode -> Just b1)) (Posit (decode -> Just b2)) (Posit (decode -> Just b3)) = Posit $ encode (Just $ (a0 * b0) + (a1 * b1) + (a2 * b2) + (a3 * b3))
  -- Fuse Dot Product of two Lists
  fdotL (toList -> l1) (toList -> l2) = Posit $ encode (Just $ go l1 l2 0)
    where
      go [] [] acc = acc
      go [] (_) _ = error "Lists not the same length"
      go (_) [] _ = error "Lists not the same length"
      go ((Posit int1) : bs) ((Posit int2) : cs) acc = case (decode int1) of
                                                         Nothing -> error "First Posit List contains NaR"
                                                         Just r1 -> case (decode int2) of
                                                                      Nothing -> error "Second Posit List contains NaR"
                                                                      Just r2 -> go bs cs (acc + (r1 * r2))

instance FusedOps (Posit64) where
  -- Fuse Multiply Add
  fma (Posit (decode -> Nothing)) (_) (_) = Posit $ encode Nothing
  fma (_) (Posit (decode -> Nothing)) (_) = Posit $ encode Nothing
  fma (_) (_) (Posit (decode -> Nothing)) = Posit $ encode Nothing
  fma (Posit (decode -> Just a)) (Posit (decode -> Just b)) (Posit (decode -> Just c)) = Posit $ encode (Just $ (a * b) + c)
  -- Fuse Add Multiply
  fam (Posit (decode -> Nothing)) (_) (_) = Posit $ encode Nothing
  fam (_) (Posit (decode -> Nothing)) (_) = Posit $ encode Nothing
  fam (_) (_) (Posit (decode -> Nothing)) = Posit $ encode Nothing
  fam (Posit (decode -> Just a)) (Posit (decode -> Just b)) (Posit (decode -> Just c)) = Posit $ encode (Just $ (a + b) * c)
  -- Fuse Multiply Multiply Subtract
  fmms (Posit (decode -> Nothing)) (_) (_) (_) = Posit $ encode Nothing
  fmms (_) (Posit (decode -> Nothing)) (_) (_) = Posit $ encode Nothing
  fmms (_) (_) (Posit (decode -> Nothing)) (_) = Posit $ encode Nothing
  fmms (_) (_) (_) (Posit (decode -> Nothing)) = Posit $ encode Nothing
  fmms (Posit (decode -> Just a)) (Posit (decode -> Just b)) (Posit (decode -> Just c)) (Posit (decode -> Just d)) = Posit $ encode (Just $ (a * b) - (c * d))
  -- Fuse Sum of 3 Posits
  fsum3 (Posit (decode -> Nothing)) (_) (_) = Posit $ encode Nothing
  fsum3 (_) (Posit (decode -> Nothing)) (_) = Posit $ encode Nothing
  fsum3 (_) (_) (Posit (decode -> Nothing)) = Posit $ encode Nothing
  fsum3 (Posit (decode -> Just a)) (Posit (decode -> Just b)) (Posit (decode -> Just c)) = Posit $ encode (Just $ a + b + c)
  -- Fuse Sum of 4 Posits
  fsum4 (Posit (decode -> Nothing)) (_) (_) (_) = Posit $ encode Nothing
  fsum4 (_) (Posit (decode -> Nothing)) (_) (_) = Posit $ encode Nothing
  fsum4 (_) (_) (Posit (decode -> Nothing)) (_) = Posit $ encode Nothing
  fsum4 (_) (_) (_) (Posit (decode -> Nothing)) = Posit $ encode Nothing
  fsum4 (Posit (decode -> Just a)) (Posit (decode -> Just b)) (Posit (decode -> Just c)) (Posit (decode -> Just d)) = Posit $ encode (Just $ a + b + c + d)
  -- Fuse Sum of a List
  fsumL (toList -> l) = Posit $ encode (Just $ go l 0)
    where
      go [] acc = acc
      go ((Posit int) : xs) acc = case (decode int) of
                                    Nothing -> error "Posit List contains NaR"
                                    Just r -> go xs (acc + r)
  -- Fuse Dot Product of a 3-Vector
  fdot3 (Posit (decode -> Nothing)) (_) (_) (_) (_) (_) = Posit $ encode Nothing
  fdot3 (_) (Posit (decode -> Nothing)) (_) (_) (_) (_) = Posit $ encode Nothing
  fdot3 (_) (_) (Posit (decode -> Nothing)) (_) (_) (_) = Posit $ encode Nothing
  fdot3 (_) (_) (_) (Posit (decode -> Nothing)) (_) (_) = Posit $ encode Nothing
  fdot3 (_) (_) (_) (_) (Posit (decode -> Nothing)) (_) = Posit $ encode Nothing
  fdot3 (_) (_) (_) (_) (_) (Posit (decode -> Nothing)) = Posit $ encode Nothing
  fdot3 (Posit (decode -> Just a1)) (Posit (decode -> Just a2)) (Posit (decode -> Just a3)) (Posit (decode -> Just b1)) (Posit (decode -> Just b2)) (Posit (decode -> Just b3)) = Posit $ encode (Just $ (a1 * b1) + (a2 * b2) + (a3 * b3))
  -- Fuse Dot Product of a 4-Vector
  fdot4 (Posit (decode -> Nothing)) (_) (_) (_) (_) (_) (_) (_) = Posit $ encode Nothing
  fdot4 (_) (Posit (decode -> Nothing)) (_) (_) (_) (_) (_) (_) = Posit $ encode Nothing
  fdot4 (_) (_) (Posit (decode -> Nothing)) (_) (_) (_) (_) (_) = Posit $ encode Nothing
  fdot4 (_) (_) (_) (Posit (decode -> Nothing)) (_) (_) (_) (_) = Posit $ encode Nothing
  fdot4 (_) (_) (_) (_) (Posit (decode -> Nothing)) (_) (_) (_) = Posit $ encode Nothing
  fdot4 (_) (_) (_) (_) (_) (Posit (decode -> Nothing)) (_) (_) = Posit $ encode Nothing
  fdot4 (_) (_) (_) (_) (_) (_) (Posit (decode -> Nothing)) (_) = Posit $ encode Nothing
  fdot4 (_) (_) (_) (_) (_) (_) (_) (Posit (decode -> Nothing)) = Posit $ encode Nothing
  fdot4 (Posit (decode -> Just a0)) (Posit (decode -> Just a1)) (Posit (decode -> Just a2)) (Posit (decode -> Just a3)) (Posit (decode -> Just b0)) (Posit (decode -> Just b1)) (Posit (decode -> Just b2)) (Posit (decode -> Just b3)) = Posit $ encode (Just $ (a0 * b0) + (a1 * b1) + (a2 * b2) + (a3 * b3))
  -- Fuse Dot Product of two Lists
  fdotL (toList -> l1) (toList -> l2) = Posit $ encode (Just $ go l1 l2 0)
    where
      go [] [] acc = acc
      go [] (_) _ = error "Lists not the same length"
      go (_) [] _ = error "Lists not the same length"
      go ((Posit int1) : bs) ((Posit int2) : cs) acc = case (decode int1) of
                                                         Nothing -> error "First Posit List contains NaR"
                                                         Just r1 -> case (decode int2) of
                                                                      Nothing -> error "Second Posit List contains NaR"
                                                                      Just r2 -> go bs cs (acc + (r1 * r2))

instance FusedOps (Posit128) where
  -- Fuse Multiply Add
  fma (Posit (decode -> Nothing)) (_) (_) = Posit $ encode Nothing
  fma (_) (Posit (decode -> Nothing)) (_) = Posit $ encode Nothing
  fma (_) (_) (Posit (decode -> Nothing)) = Posit $ encode Nothing
  fma (Posit (decode -> Just a)) (Posit (decode -> Just b)) (Posit (decode -> Just c)) = Posit $ encode (Just $ (a * b) + c)
  -- Fuse Add Multiply
  fam (Posit (decode -> Nothing)) (_) (_) = Posit $ encode Nothing
  fam (_) (Posit (decode -> Nothing)) (_) = Posit $ encode Nothing
  fam (_) (_) (Posit (decode -> Nothing)) = Posit $ encode Nothing
  fam (Posit (decode -> Just a)) (Posit (decode -> Just b)) (Posit (decode -> Just c)) = Posit $ encode (Just $ (a + b) * c)
  -- Fuse Multiply Multiply Subtract
  fmms (Posit (decode -> Nothing)) (_) (_) (_) = Posit $ encode Nothing
  fmms (_) (Posit (decode -> Nothing)) (_) (_) = Posit $ encode Nothing
  fmms (_) (_) (Posit (decode -> Nothing)) (_) = Posit $ encode Nothing
  fmms (_) (_) (_) (Posit (decode -> Nothing)) = Posit $ encode Nothing
  fmms (Posit (decode -> Just a)) (Posit (decode -> Just b)) (Posit (decode -> Just c)) (Posit (decode -> Just d)) = Posit $ encode (Just $ (a * b) - (c * d))
  -- Fuse Sum of 3 Posits
  fsum3 (Posit (decode -> Nothing)) (_) (_) = Posit $ encode Nothing
  fsum3 (_) (Posit (decode -> Nothing)) (_) = Posit $ encode Nothing
  fsum3 (_) (_) (Posit (decode -> Nothing)) = Posit $ encode Nothing
  fsum3 (Posit (decode -> Just a)) (Posit (decode -> Just b)) (Posit (decode -> Just c)) = Posit $ encode (Just $ a + b + c)
  -- Fuse Sum of 4 Posits
  fsum4 (Posit (decode -> Nothing)) (_) (_) (_) = Posit $ encode Nothing
  fsum4 (_) (Posit (decode -> Nothing)) (_) (_) = Posit $ encode Nothing
  fsum4 (_) (_) (Posit (decode -> Nothing)) (_) = Posit $ encode Nothing
  fsum4 (_) (_) (_) (Posit (decode -> Nothing)) = Posit $ encode Nothing
  fsum4 (Posit (decode -> Just a)) (Posit (decode -> Just b)) (Posit (decode -> Just c)) (Posit (decode -> Just d)) = Posit $ encode (Just $ a + b + c + d)
  -- Fuse Sum of a List
  fsumL (toList -> l) = Posit $ encode (Just $ go l 0)
    where
      go [] acc = acc
      go ((Posit int) : xs) acc = case (decode int) of
                                    Nothing -> error "Posit List contains NaR"
                                    Just r -> go xs (acc + r)
  -- Fuse Dot Product of a 3-Vector
  fdot3 (Posit (decode -> Nothing)) (_) (_) (_) (_) (_) = Posit $ encode Nothing
  fdot3 (_) (Posit (decode -> Nothing)) (_) (_) (_) (_) = Posit $ encode Nothing
  fdot3 (_) (_) (Posit (decode -> Nothing)) (_) (_) (_) = Posit $ encode Nothing
  fdot3 (_) (_) (_) (Posit (decode -> Nothing)) (_) (_) = Posit $ encode Nothing
  fdot3 (_) (_) (_) (_) (Posit (decode -> Nothing)) (_) = Posit $ encode Nothing
  fdot3 (_) (_) (_) (_) (_) (Posit (decode -> Nothing)) = Posit $ encode Nothing
  fdot3 (Posit (decode -> Just a1)) (Posit (decode -> Just a2)) (Posit (decode -> Just a3)) (Posit (decode -> Just b1)) (Posit (decode -> Just b2)) (Posit (decode -> Just b3)) = Posit $ encode (Just $ (a1 * b1) + (a2 * b2) + (a3 * b3))
  -- Fuse Dot Product of a 4-Vector
  fdot4 (Posit (decode -> Nothing)) (_) (_) (_) (_) (_) (_) (_) = Posit $ encode Nothing
  fdot4 (_) (Posit (decode -> Nothing)) (_) (_) (_) (_) (_) (_) = Posit $ encode Nothing
  fdot4 (_) (_) (Posit (decode -> Nothing)) (_) (_) (_) (_) (_) = Posit $ encode Nothing
  fdot4 (_) (_) (_) (Posit (decode -> Nothing)) (_) (_) (_) (_) = Posit $ encode Nothing
  fdot4 (_) (_) (_) (_) (Posit (decode -> Nothing)) (_) (_) (_) = Posit $ encode Nothing
  fdot4 (_) (_) (_) (_) (_) (Posit (decode -> Nothing)) (_) (_) = Posit $ encode Nothing
  fdot4 (_) (_) (_) (_) (_) (_) (Posit (decode -> Nothing)) (_) = Posit $ encode Nothing
  fdot4 (_) (_) (_) (_) (_) (_) (_) (Posit (decode -> Nothing)) = Posit $ encode Nothing
  fdot4 (Posit (decode -> Just a0)) (Posit (decode -> Just a1)) (Posit (decode -> Just a2)) (Posit (decode -> Just a3)) (Posit (decode -> Just b0)) (Posit (decode -> Just b1)) (Posit (decode -> Just b2)) (Posit (decode -> Just b3)) = Posit $ encode (Just $ (a0 * b0) + (a1 * b1) + (a2 * b2) + (a3 * b3))
  -- Fuse Dot Product of two Lists
  fdotL (toList -> l1) (toList -> l2) = Posit $ encode (Just $ go l1 l2 0)
    where
      go [] [] acc = acc
      go [] (_) _ = error "Lists not the same length"
      go (_) [] _ = error "Lists not the same length"
      go ((Posit int1) : bs) ((Posit int2) : cs) acc = case (decode int1) of
                                                         Nothing -> error "First Posit List contains NaR"
                                                         Just r1 -> case (decode int2) of
                                                                      Nothing -> error "Second Posit List contains NaR"
                                                                      Just r2 -> go bs cs (acc + (r1 * r2))

instance FusedOps (Posit256) where
  -- Fuse Multiply Add
  fma (Posit (decode -> Nothing)) (_) (_) = Posit $ encode Nothing
  fma (_) (Posit (decode -> Nothing)) (_) = Posit $ encode Nothing
  fma (_) (_) (Posit (decode -> Nothing)) = Posit $ encode Nothing
  fma (Posit (decode -> Just a)) (Posit (decode -> Just b)) (Posit (decode -> Just c)) = Posit $ encode (Just $ (a * b) + c)
  -- Fuse Add Multiply
  fam (Posit (decode -> Nothing)) (_) (_) = Posit $ encode Nothing
  fam (_) (Posit (decode -> Nothing)) (_) = Posit $ encode Nothing
  fam (_) (_) (Posit (decode -> Nothing)) = Posit $ encode Nothing
  fam (Posit (decode -> Just a)) (Posit (decode -> Just b)) (Posit (decode -> Just c)) = Posit $ encode (Just $ (a + b) * c)
  -- Fuse Multiply Multiply Subtract
  fmms (Posit (decode -> Nothing)) (_) (_) (_) = Posit $ encode Nothing
  fmms (_) (Posit (decode -> Nothing)) (_) (_) = Posit $ encode Nothing
  fmms (_) (_) (Posit (decode -> Nothing)) (_) = Posit $ encode Nothing
  fmms (_) (_) (_) (Posit (decode -> Nothing)) = Posit $ encode Nothing
  fmms (Posit (decode -> Just a)) (Posit (decode -> Just b)) (Posit (decode -> Just c)) (Posit (decode -> Just d)) = Posit $ encode (Just $ (a * b) - (c * d))
  -- Fuse Sum of 3 Posits
  fsum3 (Posit (decode -> Nothing)) (_) (_) = Posit $ encode Nothing
  fsum3 (_) (Posit (decode -> Nothing)) (_) = Posit $ encode Nothing
  fsum3 (_) (_) (Posit (decode -> Nothing)) = Posit $ encode Nothing
  fsum3 (Posit (decode -> Just a)) (Posit (decode -> Just b)) (Posit (decode -> Just c)) = Posit $ encode (Just $ a + b + c)
  -- Fuse Sum of 4 Posits
  fsum4 (Posit (decode -> Nothing)) (_) (_) (_) = Posit $ encode Nothing
  fsum4 (_) (Posit (decode -> Nothing)) (_) (_) = Posit $ encode Nothing
  fsum4 (_) (_) (Posit (decode -> Nothing)) (_) = Posit $ encode Nothing
  fsum4 (_) (_) (_) (Posit (decode -> Nothing)) = Posit $ encode Nothing
  fsum4 (Posit (decode -> Just a)) (Posit (decode -> Just b)) (Posit (decode -> Just c)) (Posit (decode -> Just d)) = Posit $ encode (Just $ a + b + c + d)
  -- Fuse Sum of a List
  fsumL (toList -> l) = Posit $ encode (Just $ go l 0)
    where
      go [] acc = acc
      go ((Posit int) : xs) acc = case (decode int) of
                                    Nothing -> error "Posit List contains NaR"
                                    Just r -> go xs (acc + r)
  -- Fuse Dot Product of a 3-Vector
  fdot3 (Posit (decode -> Nothing)) (_) (_) (_) (_) (_) = Posit $ encode Nothing
  fdot3 (_) (Posit (decode -> Nothing)) (_) (_) (_) (_) = Posit $ encode Nothing
  fdot3 (_) (_) (Posit (decode -> Nothing)) (_) (_) (_) = Posit $ encode Nothing
  fdot3 (_) (_) (_) (Posit (decode -> Nothing)) (_) (_) = Posit $ encode Nothing
  fdot3 (_) (_) (_) (_) (Posit (decode -> Nothing)) (_) = Posit $ encode Nothing
  fdot3 (_) (_) (_) (_) (_) (Posit (decode -> Nothing)) = Posit $ encode Nothing
  fdot3 (Posit (decode -> Just a1)) (Posit (decode -> Just a2)) (Posit (decode -> Just a3)) (Posit (decode -> Just b1)) (Posit (decode -> Just b2)) (Posit (decode -> Just b3)) = Posit $ encode (Just $ (a1 * b1) + (a2 * b2) + (a3 * b3))
  -- Fuse Dot Product of a 4-Vector
  fdot4 (Posit (decode -> Nothing)) (_) (_) (_) (_) (_) (_) (_) = Posit $ encode Nothing
  fdot4 (_) (Posit (decode -> Nothing)) (_) (_) (_) (_) (_) (_) = Posit $ encode Nothing
  fdot4 (_) (_) (Posit (decode -> Nothing)) (_) (_) (_) (_) (_) = Posit $ encode Nothing
  fdot4 (_) (_) (_) (Posit (decode -> Nothing)) (_) (_) (_) (_) = Posit $ encode Nothing
  fdot4 (_) (_) (_) (_) (Posit (decode -> Nothing)) (_) (_) (_) = Posit $ encode Nothing
  fdot4 (_) (_) (_) (_) (_) (Posit (decode -> Nothing)) (_) (_) = Posit $ encode Nothing
  fdot4 (_) (_) (_) (_) (_) (_) (Posit (decode -> Nothing)) (_) = Posit $ encode Nothing
  fdot4 (_) (_) (_) (_) (_) (_) (_) (Posit (decode -> Nothing)) = Posit $ encode Nothing
  fdot4 (Posit (decode -> Just a0)) (Posit (decode -> Just a1)) (Posit (decode -> Just a2)) (Posit (decode -> Just a3)) (Posit (decode -> Just b0)) (Posit (decode -> Just b1)) (Posit (decode -> Just b2)) (Posit (decode -> Just b3)) = Posit $ encode (Just $ (a0 * b0) + (a1 * b1) + (a2 * b2) + (a3 * b3))
  -- Fuse Dot Product of two Lists
  fdotL (toList -> l1) (toList -> l2) = Posit $ encode (Just $ go l1 l2 0)
    where
      go [] [] acc = acc
      go [] (_) _ = error "Lists not the same length"
      go (_) [] _ = error "Lists not the same length"
      go ((Posit int1) : bs) ((Posit int2) : cs) acc = case (decode int1) of
                                                         Nothing -> error "First Posit List contains NaR"
                                                         Just r1 -> case (decode int2) of
                                                                      Nothing -> error "Second Posit List contains NaR"
                                                                      Just r2 -> go bs cs (acc + (r1 * r2))
-}


-- =====================================================================
-- ===                  Conversion Between Posits Types              ===
-- =====================================================================

class Convertable a b where
  convert :: a -> b

instance forall es1 es2. (PositC es1, PositC es2) => Convertable (Posit es1) (Posit es2) where
  convert NaR = NaR
  convert (Posit (decode -> Just r)) = Posit $ encode (Just r)  -- What about: fromRational.toRational


{-
instance Convertable Posit8 Posit16 where
  convert (Posit (decode -> Nothing)) = Posit $ encode Nothing
  convert (Posit (decode -> Just r)) = Posit $ encode (Just r)

instance Convertable Posit8 Posit32 where
  convert (Posit (decode -> Nothing)) = Posit $ encode Nothing
  convert (Posit (decode -> Just r)) = Posit $ encode (Just r)

instance Convertable Posit8 Posit64 where
  convert (Posit (decode -> Nothing)) = Posit $ encode Nothing
  convert (Posit (decode -> Just r)) = Posit $ encode (Just r)

instance Convertable Posit8 Posit128 where
  convert (Posit (decode -> Nothing)) = Posit $ encode Nothing
  convert (Posit (decode -> Just r)) = Posit $ encode (Just r)

instance Convertable Posit8 Posit256 where
  convert (Posit (decode -> Nothing)) = Posit $ encode Nothing
  convert (Posit (decode -> Just r)) = Posit $ encode (Just r)

instance Convertable Posit16 Posit8 where
  convert (Posit (decode -> Nothing)) = Posit $ encode Nothing
  convert (Posit (decode -> Just r)) = Posit $ encode (Just r)

instance Convertable Posit16 Posit32 where
  convert (Posit (decode -> Nothing)) = Posit $ encode Nothing
  convert (Posit (decode -> Just r)) = Posit $ encode (Just r)

instance Convertable Posit16 Posit64 where
  convert (Posit (decode -> Nothing)) = Posit $ encode Nothing
  convert (Posit (decode -> Just r)) = Posit $ encode (Just r)

instance Convertable Posit16 Posit128 where
  convert (Posit (decode -> Nothing)) = Posit $ encode Nothing
  convert (Posit (decode -> Just r)) = Posit $ encode (Just r)

instance Convertable Posit16 Posit256 where
  convert (Posit (decode -> Nothing)) = Posit $ encode Nothing
  convert (Posit (decode -> Just r)) = Posit $ encode (Just r)

instance Convertable Posit32 Posit8 where
  convert (Posit (decode -> Nothing)) = Posit $ encode Nothing
  convert (Posit (decode -> Just r)) = Posit $ encode (Just r)

instance Convertable Posit32 Posit16 where
  convert (Posit (decode -> Nothing)) = Posit $ encode Nothing
  convert (Posit (decode -> Just r)) = Posit $ encode (Just r)

instance Convertable Posit32 Posit64 where
  convert (Posit (decode -> Nothing)) = Posit $ encode Nothing
  convert (Posit (decode -> Just r)) = Posit $ encode (Just r)

instance Convertable Posit32 Posit128 where
  convert (Posit (decode -> Nothing)) = Posit $ encode Nothing
  convert (Posit (decode -> Just r)) = Posit $ encode (Just r)

instance Convertable Posit32 Posit256 where
  convert (Posit (decode -> Nothing)) = Posit $ encode Nothing
  convert (Posit (decode -> Just r)) = Posit $ encode (Just r)

instance Convertable Posit64 Posit8 where
  convert (Posit (decode -> Nothing)) = Posit $ encode Nothing
  convert (Posit (decode -> Just r)) = Posit $ encode (Just r)

instance Convertable Posit64 Posit16 where
  convert (Posit (decode -> Nothing)) = Posit $ encode Nothing
  convert (Posit (decode -> Just r)) = Posit $ encode (Just r)

instance Convertable Posit64 Posit32 where
  convert (Posit (decode -> Nothing)) = Posit $ encode Nothing
  convert (Posit (decode -> Just r)) = Posit $ encode (Just r)

instance Convertable Posit64 Posit128 where
  convert (Posit (decode -> Nothing)) = Posit $ encode Nothing
  convert (Posit (decode -> Just r)) = Posit $ encode (Just r)

instance Convertable Posit64 Posit256 where
  convert (Posit (decode -> Nothing)) = Posit $ encode Nothing
  convert (Posit (decode -> Just r)) = Posit $ encode (Just r)

instance Convertable Posit128 Posit8 where
  convert (Posit (decode -> Nothing)) = Posit $ encode Nothing
  convert (Posit (decode -> Just r)) = Posit $ encode (Just r)

instance Convertable Posit128 Posit16 where
  convert (Posit (decode -> Nothing)) = Posit $ encode Nothing
  convert (Posit (decode -> Just r)) = Posit $ encode (Just r)

instance Convertable Posit128 Posit32 where
  convert (Posit (decode -> Nothing)) = Posit $ encode Nothing
  convert (Posit (decode -> Just r)) = Posit $ encode (Just r)

instance Convertable Posit128 Posit64 where
  convert (Posit (decode -> Nothing)) = Posit $ encode Nothing
  convert (Posit (decode -> Just r)) = Posit $ encode (Just r)

instance Convertable Posit128 Posit256 where
  convert (Posit (decode -> Nothing)) = Posit $ encode Nothing
  convert (Posit (decode -> Just r)) = Posit $ encode (Just r)

instance Convertable Posit256 Posit8 where
  convert (Posit (decode -> Nothing)) = Posit $ encode Nothing
  convert (Posit (decode -> Just r)) = Posit $ encode (Just r)

instance Convertable Posit256 Posit16 where
  convert (Posit (decode -> Nothing)) = Posit $ encode Nothing
  convert (Posit (decode -> Just r)) = Posit $ encode (Just r)

instance Convertable Posit256 Posit32 where
  convert (Posit (decode -> Nothing)) = Posit $ encode Nothing
  convert (Posit (decode -> Just r)) = Posit $ encode (Just r)

instance Convertable Posit256 Posit64 where
  convert (Posit (decode -> Nothing)) = Posit $ encode Nothing
  convert (Posit (decode -> Just r)) = Posit $ encode (Just r)

instance Convertable Posit256 Posit128 where
  convert (Posit (decode -> Nothing)) = Posit $ encode Nothing
  convert (Posit (decode -> Just r)) = Posit $ encode (Just r)
-}

-- =====================================================================
-- ===                Alternative Show Methods                       ===
-- =====================================================================


class AltShow a where
  -- Display the Posit in its Binary Representation
  displayBinary :: a -> String
  -- Display the Posit in its Integral Representation
  displayIntegral :: a -> String
  -- Display the Posit as a Rational
  displayRational :: a -> String
  -- Display the Posit as a Decimal until the Repetend occurs
  displayDecimal :: a -> String
--
--
instance forall es. (Show (IntN es), PositC es) => AltShow (Posit es) where
  displayBinary (Posit int) = displayBin int
  
  displayIntegral (Posit int) = show int
  
  displayRational NaR = "NaR"
  displayRational (Posit (decode -> Just r)) = show r
  
  displayDecimal NaR = "NaR"
  displayDecimal (Posit (decode -> Just r)) =
    let (s,_) = fromRationalRepetendUnlimited r
    in show s
--
{-
instance AltShow (Posit8) where
  displayBinary (Posit int) = displayBin int
  
  displayIntegral (Posit int) = show int
  
  displayRational (Posit (decode -> Nothing)) = "NaR"
  displayRational (Posit (decode -> Just r)) = show r
  
  displayDecimal (Posit (decode -> Nothing)) = "NaR"
  displayDecimal (Posit (decode -> Just r)) =
    let (s,_) = fromRationalRepetendUnlimited r
    in show s

instance AltShow (Posit16) where
  displayBinary (Posit int) = displayBin int
  
  displayIntegral (Posit int) = show int
  
  displayRational (Posit (decode -> Nothing)) = "NaR"
  displayRational (Posit (decode -> Just r)) = show r
  
  displayDecimal (Posit (decode -> Nothing)) = "NaR"
  displayDecimal (Posit (decode -> Just r)) =
    let (s,_) = fromRationalRepetendUnlimited r
    in show s

instance AltShow (Posit32) where
  displayBinary (Posit int) = displayBin int
  
  displayIntegral (Posit int) = show int
  
  displayRational (Posit (decode -> Nothing)) = "NaR"
  displayRational (Posit (decode -> Just r)) = show r
  
  displayDecimal (Posit (decode -> Nothing)) = "NaR"
  displayDecimal (Posit (decode -> Just r)) =
    let (s,_) = fromRationalRepetendUnlimited r
    in show s

instance AltShow (Posit64) where
  displayBinary (Posit int) = displayBin int
  
  displayIntegral (Posit int) = show int
  
  displayRational (Posit (decode -> Nothing)) = "NaR"
  displayRational (Posit (decode -> Just r)) = show r
  
  displayDecimal (Posit (decode -> Nothing)) = "NaR"
  displayDecimal (Posit (decode -> Just r)) =
    let (s,_) = fromRationalRepetendUnlimited r
    in show s

instance AltShow (Posit128) where
  displayBinary (Posit int) = displayBin int
  
  displayIntegral (Posit int) = show int
  
  displayRational (Posit (decode -> Nothing)) = "NaR"
  displayRational (Posit (decode -> Just r)) = show r
  
  displayDecimal (Posit (decode -> Nothing)) = "NaR"
  displayDecimal (Posit (decode -> Just r)) =
    let (s,_) = fromRationalRepetendUnlimited r
    in show s

instance AltShow (Posit256) where
  displayBinary (Posit int) = displayBin int
  
  displayIntegral (Posit int) = show int
  
  displayRational (Posit (decode -> Nothing)) = "NaR"
  displayRational (Posit (decode -> Just r)) = show r
  
  displayDecimal (Posit (decode -> Nothing)) = "NaR"
  displayDecimal (Posit (decode -> Just r)) =
    let (s,_) = fromRationalRepetendUnlimited r
    in show s
-}

-- =====================================================================
-- ===                         Read Posit                            ===
-- =====================================================================

--
instance forall es. (PositC es) => Read (Posit es) where
  readPrec = 
    parens $ do
      x <- lexP
      case x of
        Ident "NaR" -> return NaR
        _ -> pfail
      +++
      do
        s <- lift scientificP
        return $ Posit (encode $ Just (toRational s))
  
  readListPrec = readListPrecDefault
--
{-
instance Read (Posit8) where
  readPrec = 
    parens $ do
      x <- lexP
      case x of
        Ident "NaR" -> return $ Posit unReal
        _ -> pfail
      +++
      do
        s <- lift scientificP
        return $ Posit (encode $ Just (toRational s))
  
  readListPrec = readListPrecDefault

instance Read (Posit16) where
  readPrec = 
    parens $ do
      x <- lexP
      case x of
        Ident "NaR" -> return $ Posit unReal
        _ -> pfail
      +++
      do
        s <- lift scientificP
        return $ Posit (encode $ Just (toRational s))
  
  readListPrec = readListPrecDefault

instance Read (Posit32) where
  readPrec = 
    parens $ do
      x <- lexP
      case x of
        Ident "NaR" -> return $ Posit unReal
        _ -> pfail
      +++
      do
        s <- lift scientificP
        return $ Posit (encode $ Just (toRational s))
  
  readListPrec = readListPrecDefault

instance Read (Posit64) where
  readPrec = 
    parens $ do
      x <- lexP
      case x of
        Ident "NaR" -> return $ Posit unReal
        _ -> pfail
      +++
      do
        s <- lift scientificP
        return $ Posit (encode $ Just (toRational s))
  
  readListPrec = readListPrecDefault

instance Read (Posit128) where
  readPrec = 
    parens $ do
      x <- lexP
      case x of
        Ident "NaR" -> return $ Posit unReal
        _ -> pfail
      +++
      do
        s <- lift scientificP
        return $ Posit (encode $ Just (toRational s))
  
  readListPrec = readListPrecDefault

instance Read (Posit256) where
  readPrec = 
    parens $ do
      x <- lexP
      case x of
        Ident "NaR" -> return $ Posit unReal
        _ -> pfail
      +++
      do
        s <- lift scientificP
        return $ Posit (encode $ Just (toRational s))
  
  readListPrec = readListPrecDefault
-}


-- =====================================================================
-- ===                  Storable Instances                           ===
-- =====================================================================
--
-- Orphan Instance for Word128 using the DoubleWord type class
instance Storable (Word128) where
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
instance Storable (Int128) where
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
instance Storable (Int256) where
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


--
instance forall es. (Storable (IntN es), PositC es) => Storable (Posit es) where
  sizeOf _ = fromIntegral $ nBytes @es
  alignment _ = fromIntegral $ nBytes @es
  peek ptr = do
    int <- peek (castPtr ptr :: Ptr (IntN es))
    return $ Posit int
  poke ptr (Posit int) = do
    poke (castPtr ptr :: Ptr (IntN es)) int
--
--
{- Instance for Posit
instance Storable (Posit8) where
  sizeOf _ = fromIntegral $ nBytes @Z
  alignment _ = fromIntegral $ nBytes @Z
  peek ptr = do
    int <- peek (castPtr ptr :: Ptr (IntN Z))
    return $ Posit int
  poke ptr (Posit int) = do
    poke (castPtr ptr :: Ptr (IntN Z)) int

instance Storable (Posit16) where
  sizeOf _ = fromIntegral $ nBytes @I
  alignment _ = fromIntegral $ nBytes @I
  peek ptr = do
    int <- peek (castPtr ptr :: Ptr (IntN I))
    return $ Posit int
  poke ptr (Posit int) = do
    poke (castPtr ptr :: Ptr (IntN I)) int

instance Storable (Posit32) where
  sizeOf _ = fromIntegral $ nBytes @II
  alignment _ = fromIntegral $ nBytes @II
  peek ptr = do
    int <- peek (castPtr ptr :: Ptr (IntN II))
    return $ Posit int
  poke ptr (Posit int) = do
    poke (castPtr ptr :: Ptr (IntN II)) int

instance Storable (Posit64) where
  sizeOf _ = fromIntegral $ nBytes @III
  alignment _ = fromIntegral $ nBytes @III
  peek ptr = do
    int <- peek (castPtr ptr :: Ptr (IntN III))
    return $ Posit int
  poke ptr (Posit int) = do
    poke (castPtr ptr :: Ptr (IntN III)) int

instance Storable (Posit128) where
  sizeOf _ = fromIntegral $ nBytes @IV
  alignment _ = fromIntegral $ nBytes @IV
  peek ptr = do
    int <- peek (castPtr ptr :: Ptr (IntN IV))
    return $ Posit int
  poke ptr (Posit int) = do
    poke (castPtr ptr :: Ptr (IntN IV)) int

instance Storable (Posit256) where
  sizeOf _ = fromIntegral $ nBytes @V
  alignment _ = fromIntegral $ nBytes @V
  peek ptr = do
    int <- peek (castPtr ptr :: Ptr (IntN V))
    return $ Posit int
  poke ptr (Posit int) = do
    poke (castPtr ptr :: Ptr (IntN V)) int
-}



-- =====================================================================
-- ===                        Real Frac                              ===
-- =====================================================================

--
instance forall es. (Num (IntN es), Ord (IntN es), PositC es) => RealFrac (Posit es) where
  properFraction NaR = error "NaR value is not a RealFrac"
  properFraction (Posit (decode -> Just r)) =
    let (int, r') = properFraction r
    in (int, Posit (encode $ Just r'))
--
{-
instance RealFrac (Posit8) where
  properFraction (Posit (decode -> Nothing)) = error "NaR value is not a RealFrac"
  properFraction (Posit (decode -> Just r)) =
    let (int, r') = properFraction r
    in (int, Posit (encode $ Just r'))

instance RealFrac (Posit16) where
  properFraction (Posit (decode -> Nothing)) = error "NaR value is not a RealFrac"
  properFraction (Posit (decode -> Just r)) =
    let (int, r') = properFraction r
    in (int, Posit (encode $ Just r'))

instance RealFrac (Posit32) where
  properFraction (Posit (decode -> Nothing)) = error "NaR value is not a RealFrac"
  properFraction (Posit (decode -> Just r)) =
    let (int, r') = properFraction r
    in (int, Posit (encode $ Just r'))

instance RealFrac (Posit64) where
  properFraction (Posit (decode -> Nothing)) = error "NaR value is not a RealFrac"
  properFraction (Posit (decode -> Just r)) =
    let (int, r') = properFraction r
    in (int, Posit (encode $ Just r'))

instance RealFrac (Posit128) where
  properFraction (Posit (decode -> Nothing)) = error "NaR value is not a RealFrac"
  properFraction (Posit (decode -> Just r)) =
    let (int, r') = properFraction r
    in (int, Posit (encode $ Just r'))

instance RealFrac (Posit256) where
  properFraction (Posit (decode -> Nothing)) = error "NaR value is not a RealFrac"
  properFraction (Posit (decode -> Just r)) =
    let (int, r') = properFraction r
    in (int, Posit (encode $ Just r'))
-}



-- =====================================================================
-- ===                         Real Float                            ===
-- =====================================================================
--
instance forall es. (Eq (IntN es), Ord (IntN es), Num (IntN es), Floating (Posit es), PositC es) => RealFloat (Posit es) where
  isIEEE _ = False
  isDenormalized _ = False
  isNegativeZero _ = False
  
  isNaN NaR = True
  isNaN _ = False
  
  isInfinite NaR = True
  isInfinite _ = False
  
  atan2 x y
    | x > 0            =  atan (y/x)
    | x == 0 && y > 0  =  pi/2
    | x <  0 && y > 0  =  pi + atan (y/x)
    | x <= 0 && y < 0  = -atan2 (-y) x
    | y == 0 && x < 0  =  pi
    | x==0 && y==0     =  y
    | otherwise        =  x + y
  
  floatRadix _ = 2
  floatDigits = undefined
  floatRange _ = (negate maxExponent, maxExponent)
    where
      maxExponent = fromIntegral $ (nBytes @es) * ((nBits @es) - 2)
  decodeFloat = undefined
  encodeFloat = undefined
--
{-
instance RealFloat (Posit8) where
  isIEEE _ = False
  isDenormalized _ = False
  isNegativeZero _ = False
  isNaN (Posit int)
    | int == unReal = True
    | otherwise = False
  isInfinite (Posit int)
    | int == unReal = True
    | otherwise = False
  atan2 x y
    | x > 0            =  atan (y/x)
    | x == 0 && y > 0  =  pi/2
    | x <  0 && y > 0  =  pi + atan (y/x)
    | x <= 0 && y < 0  = -atan2 (-y) x
    | y == 0 && x < 0  =  pi
    | x==0 && y==0     =  y
    | otherwise        =  x + y
  
  floatRadix = undefined
  floatDigits = undefined
  floatRange = undefined
  decodeFloat = undefined
  encodeFloat = undefined

instance RealFloat (Posit16) where
  isIEEE _ = False
  isDenormalized _ = False
  isNegativeZero _ = False
  isNaN (Posit int)
    | int == unReal = True
    | otherwise = False
  isInfinite (Posit int)
    | int == unReal = True
    | otherwise = False
  atan2 x y
    | x > 0            =  atan (y/x)
    | x == 0 && y > 0  =  pi/2
    | x <  0 && y > 0  =  pi + atan (y/x)
    | x <= 0 && y < 0  = -atan2 (-y) x
    | y == 0 && x < 0  =  pi
    | x==0 && y==0     =  y
    | otherwise        =  x + y
  
  floatRadix = undefined
  floatDigits = undefined
  floatRange = undefined
  decodeFloat = undefined
  encodeFloat = undefined

instance RealFloat (Posit32) where
  isIEEE _ = False
  isDenormalized _ = False
  isNegativeZero _ = False
  isNaN (Posit int)
    | int == unReal = True
    | otherwise = False
  isInfinite (Posit int)
    | int == unReal = True
    | otherwise = False
  atan2 x y
    | x > 0            =  atan (y/x)
    | x == 0 && y > 0  =  pi/2
    | x <  0 && y > 0  =  pi + atan (y/x)
    | x <= 0 && y < 0  = -atan2 (-y) x
    | y == 0 && x < 0  =  pi
    | x==0 && y==0     =  y
    | otherwise        =  x + y
  
  floatRadix = undefined
  floatDigits = undefined
  floatRange = undefined
  decodeFloat = undefined
  encodeFloat = undefined

instance RealFloat (Posit64) where
  isIEEE _ = False
  isDenormalized _ = False
  isNegativeZero _ = False
  isNaN (Posit int)
    | int == unReal = True
    | otherwise = False
  isInfinite (Posit int)
    | int == unReal = True
    | otherwise = False
  atan2 x y
    | x > 0            =  atan (y/x)
    | x == 0 && y > 0  =  pi/2
    | x <  0 && y > 0  =  pi + atan (y/x)
    | x <= 0 && y < 0  = -atan2 (-y) x
    | y == 0 && x < 0  =  pi
    | x==0 && y==0     =  y
    | otherwise        =  x + y
  
  floatRadix = undefined
  floatDigits = undefined
  floatRange = undefined
  decodeFloat = undefined
  encodeFloat = undefined

instance RealFloat (Posit128) where
  isIEEE _ = False
  isDenormalized _ = False
  isNegativeZero _ = False
  isNaN (Posit int)
    | int == unReal = True
    | otherwise = False
  isInfinite (Posit int)
    | int == unReal = True
    | otherwise = False
  atan2 x y
    | x > 0            =  atan (y/x)
    | x == 0 && y > 0  =  pi/2
    | x <  0 && y > 0  =  pi + atan (y/x)
    | x <= 0 && y < 0  = -atan2 (-y) x
    | y == 0 && x < 0  =  pi
    | x==0 && y==0     =  y
    | otherwise        =  x + y
  
  floatRadix = undefined
  floatDigits = undefined
  floatRange = undefined
  decodeFloat = undefined
  encodeFloat = undefined

instance RealFloat (Posit256) where
  isIEEE _ = False
  isDenormalized _ = False
  isNegativeZero _ = False
  isNaN (Posit int)
    | int == unReal = True
    | otherwise = False
  isInfinite (Posit int)
    | int == unReal = True
    | otherwise = False
  atan2 x y
    | x > 0            =  atan (y/x)
    | x == 0 && y > 0  =  pi/2
    | x <  0 && y > 0  =  pi + atan (y/x)
    | x <= 0 && y < 0  = -atan2 (-y) x
    | y == 0 && x < 0  =  pi
    | x==0 && y==0     =  y
    | otherwise        =  x + y
  
  floatRadix = undefined
  floatDigits = undefined
  floatRange = undefined
  decodeFloat = undefined
  encodeFloat = undefined
-}


-- =====================================================================
-- ===                         Floating                              ===
-- =====================================================================


instance Floating (Posit8) where
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

instance Floating (Posit16) where
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

instance Floating (Posit32) where
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

instance Floating (Posit64) where
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

instance Floating (Posit128) where
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

instance Floating (Posit256) where
  pi = 3.141592653589793238462643383279502884197169399375105820974944592307816406286 :: Posit256
  exp = funExp
  log = funLogTuma
  (**) = funPow
  sin = funSin
  cos = funCos
  asin = funAsin
  acos = funAcos
  atan = funAtan
  sinh = funSinh
  cosh = funCosh
  asinh = undefined
  acosh = undefined
  atanh = funAtanh

class AltFloating p where
  phi :: p
  gamma :: p -> p

instance AltFloating (Posit8) where
  phi = convert (phi :: Posit256) :: Posit8
  gamma x = convert (gamma (convert x) :: Posit256) :: Posit8

instance AltFloating (Posit16) where
  phi = convert (phi :: Posit256) :: Posit16
  gamma x = convert (gamma (convert x) :: Posit256) :: Posit16

instance AltFloating (Posit32) where
  phi = convert (phi :: Posit256) :: Posit32
  gamma x = convert (gamma (convert x) :: Posit256) :: Posit32

instance AltFloating (Posit64) where
  phi = convert (phi :: Posit256) :: Posit64
  gamma x = convert (gamma (convert x) :: Posit256) :: Posit64

instance AltFloating (Posit128) where
  phi = convert (phi :: Posit256) :: Posit128
  gamma x = convert (gamma (convert x) :: Posit256) :: Posit128

instance AltFloating (Posit256) where
  phi = funPhi 1.6
  gamma = undefined

funPhi :: Posit256 -> Posit256
funPhi  px@(Posit x)
    | x == x' = (Posit x)
    | otherwise = funPhi (Posit x')
      where
        (Posit x') = (px^2 + 2*px) / (px^2 + 1)


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
  | otherwise = 0.5 * (log $ (1+t) / (1-t)) - (fromIntegral ex / 2) * lnOf2
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
-- funSinhTuma z | z > 3 = 0.5 * (funExpTuma z - (funExpTuma $ negate z))
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
funCoshTuma z | z > 3 = 0.5 * (funExpTuma z + (funExpTuma $ negate z))
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
      term s = findSquaring 0 s  -- returns (m,s') m the number of times to square, and the new significand
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

--  Borwein's algorithm, with quintic convergence,
--  gets to 7 ULP in 4 iterations, but really slow due to expensive function evaluations
funPi2 :: Posit256
funPi2 = recip $ go 0 0 0.5 (5 / phi^3)
  where
    go :: Posit256 -> Natural -> Posit256 -> Posit256 -> Posit256
    go !prev !n !a !s
      | prev == a = a
      | otherwise =
        let x = 5 / s - 1
            y = (x - 1)^2 + 7
            z = (0.5 * x * (y + (sqrt $ y^2 - 4 * x^3)))**(1/5)
            a' = s^2 * a - (5^n * ((s^2 - 5)/2 + (sqrt $ s * (s^2 - 2*s + 5))))
            s' = 25 / ((z + x/z + 1)^2 * s)
        in go a (n+1) a' s'
--


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
funExp x = funExp2 $ x / lnOf2
--

--
--
funExp2 :: Posit256 -> Posit256
funExp2 NaR = NaR
funExp2 0 = 1
funExp2 x
  | x < 0 = recip.funExp2.negate $ x  -- always calculate the positive method
  | otherwise =
    let (int,rem) = properFraction x  -- change domain to something near zero for best accuracy of the Taylor Series Approximation
        twoToTheInt = 2^int
        exp2Taylor = funExpTaylor $ rem * lnOf2
    in fromIntegral twoToTheInt * exp2Taylor
--


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
      | acc == (acc + term k) = acc
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
funExpTuma z | z < 0 = recip.funExpTuma.negate $ z
funExpTuma z = go 66 1
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
  | otherwise = exp $ y * (log x)
--

-- Looks like 1 ULP for 0.7813
funSinc :: Posit256 -> Posit256
funSinc NaR = NaR
funSinc 0 = 1  -- Why the hell not!
funSinc theta = sin theta / theta
--

-- Shannon wavelet
funPsiSha :: Posit256 -> Posit256
funPsiSha NaR = NaR
funPsiSha x = 2 * funSinc x - funSinc x
--

-- Using the CORDIC domain reduction and fixed point Taylor series approx
funLog' :: Posit256 -> Posit256
funLog' NaR = NaR
funLog' 1 = 0
funLog' x
  | x <= 0 = NaR
  | otherwise = funLogTaylor sig + (fromIntegral ex * lnOf2)
    where
      (ex, sig) = (int * fromIntegral (nBytes @V) + fromIntegral nat + 1, fromRational rat / 2) -- move significand range from 1,2 to 0.5,1
      (_,int,nat,rat) = (posit2TupPosit @V).toRational $ x -- sign should always be positive
      
  

-- natural log with log phi acurate to 9 ULP
funLogTaylor :: Posit256 -> Posit256
funLogTaylor NaR = NaR
funLogTaylor 1 = 0
funLogTaylor x
  | x <= 0 = NaR
  -- | x < 1 = negate $ -- 
  | x <= 2 = go 1 0
  | otherwise = error "The funLogTaylor algorithm is being used improperly"
    where
      go :: Natural -> Posit256 -> Posit256
      go !k !acc
        | acc == (acc + term k) = acc
        | otherwise = go (k + 1) (acc + term k)
      term :: Natural -> Posit256
      term k = (-1)^(k+1) * (x - 1)^k / (fromIntegral k)
      



-- natural log the Jan J Tuma way
funLogTuma :: Posit256 -> Posit256
funLogTuma NaR = NaR
funLogTuma 1 = 0  -- this is almost like branch point, see conversions from [1,2) to [0.5,1) and/or , where funLogTuma 1 = 0
funLogTuma x | x <= 0 = NaR  -- zero and less than zero is NaR
funLogTuma x | otherwise = go 217 1 + (fromIntegral ex * lnOf2)  -- domain reduction where significand used in 'go' is now [0.5,1)
    where
      (_,int,nat,rat) = (posit2TupPosit @V).toRational $ x -- sign should always be positive
      (ex, sig) = (fromIntegral (nBytes @V) * int + fromIntegral nat + 1, fromRational rat / 2) -- move significand range from [1,2) to [0.5,1)
      sigM1 = sig - 1  -- now [-0.5, 0)
      go :: Natural -> Posit256 -> Posit256
      go !k !acc
        | k == 0 = sigM1 * acc
        -- | otherwise = go (k-1) (fsm (recip.fromIntegral $ k) sigM1 acc) --  = a - (b * c)  -- but fused with only 1 rounding operation, don't really see much of a difference, should try and benchmark
        | otherwise = go (k-1) (recip (fromIntegral k) - sigM1 * acc)






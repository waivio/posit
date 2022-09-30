-- Trouble with (TypeFamilies/TypeFaimlyDependencies, GADTs, DataKinds)

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}  -- The 'IntN es' has to be an injective type family, this turns on TypeFamilies
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}  -- for various coerse expermentation
{-# LANGUAGE GADTs #-}

{-# LANGUAGE ViewPatterns #-}

{-# LANGUAGE InstanceSigs #-} -- For good documenting code style


module Data.Posit
(Posit8,
 Posit16,
 Posit32,
 Posit64,
 Posit128,
 Posit256,
 coerse,
 incPrec,
 decPrec,
 nBits,
 nBytes,
 signBitSize,
 exponentSize,
 uSeed ) where


import GHC.Natural
import GHC.TypeNats

import Data.Ratio  -- Rational numbers can get arbitrarily close to Real numbers


-- 2's complement format integers:
import Data.Int (Int8,Int16,Int32,Int64)  -- Import standard Int sizes
import Data.DoubleWord (Int128,Int256) -- Import large Int sizes
import Data.Bits

-- import Debug.Trace  -- for debuging purposes


-- for showing the int in binary
import Numeric (showIntAtBase)
import Data.Char (intToDigit)
showBinary :: Integral a => a -> String -- Only really works for 64bit precision
showBinary x =
  let x' = toInteger x
  in showIntAtBase 2 intToDigit (fromInteger x' :: Word) ""

{-
showBinary :: forall es.(KnownNat es) => IntN es -> String
-}

-- I gotta GADT where the exponent size is a natural number,
-- Exponent Size 'es' of type Natural
exponentSize :: forall es.(KnownNat es) => Posit es -> Natural
exponentSize _ = natVal @es (undefined :: Posit es)
-- implementing easy parts of the Posit Standard implementing
--  A value obtained by starting with 2 and squaring repeatedly es times:  2,  4,  16,  256, ... influencing the way the projective real circle of unums gets populated
uSeed :: forall es.(KnownNat es) => Posit es -> Natural
uSeed _ = 2^2^exponentSize (undefined :: Posit es)
-- 
-- The Field of Real numbers (ℝ) isn't algebraically closed, so that admits
-- an approximation of ℝ as Maybe Rational, where sometimes the result
-- of functions are not totally constrained in the ℝ algebra.  There are
-- also algorithms that allow Rational Numbers to get arbitrally close to
-- irrational numbers (e.g. sqrt 2) or transendental numbers (e.g. pi).
-- The "Posit es" GADT uses the infinite precision rational numbers ℚ, and
-- is mapped to the finite field of ℤ for different 2^es number of bytes sized integers.
-- 
data Posit es where
  InfPrec :: KnownNat es => Maybe Rational -> Posit es
  FinPrec :: (KnownNat es, Bits (IntN es), Bounded (IntN es), Integral (IntN es), Num (IntN es), Eq (IntN es), Ord (IntN es)) => IntN es -> Posit es

toFinPrec :: Posit es -> Posit es
toFinPrec (FinPrec int) = FinPrec int
toFinPrec inf@InfPrec{} = encode inf

toInfPrec :: Posit es -> Posit es
toInfPrec (InfPrec inf) = InfPrec inf
toInfPrec int@FinPrec{} = decode int


instance Show (Posit es) where
  show (InfPrec Nothing) = "NaR"
  show (InfPrec (Just r)) = show r
  show (FinPrec int) = showBinary int


-- Two Posit Numbers are Equal if their Finite Precision Integer representation is Equal
instance Eq (Posit es) where
  (toFinPrec -> int1) == (toFinPrec -> int2) = int1 == int2


-- Two Posit Numbers are ordered by their Finite Precision Integer representation
instance Ord (Posit es) where
  compare (toFinPrec -> int1) (toFinPrec -> int2) = compare int1 int2


instance forall es.(KnownNat es) => Fractional (Posit es) where
  fromRational r = InfPrec (Just r)
  
  recip (InfPrec Nothing) = InfPrec Nothing
  recip (InfPrec (Just 0)) = InfPrec Nothing
  recip (InfPrec (Just r)) = InfPrec (Just (recip r)) 
  recip fin@FinPrec{} = toFinPrec.recip.toInfPrec $ fin


instance forall es.(KnownNat es) => Real (Posit es) where
  toRational (InfPrec Nothing) = error "The Posit is no longer Real, a divide by zero issue, or it should have been a complex number"
  toRational (InfPrec (Just r)) = r
  toRational fin@FinPrec{} = toRational (toInfPrec fin)


instance forall es.(KnownNat es) => Num (Posit es) where
  fromInteger int = InfPrec (Just (int % 1))
  
  signum (InfPrec Nothing) = 0
  signum (InfPrec (Just r)) = InfPrec (Just $ signum r)
  signum fin@FinPrec{} = signum (toInfPrec fin)
  
  negate p = pLift1 negate p
  
  abs p = pLift1 abs p
  
  p1 + p2 = toFinPrec $ pLift2 (+) p1 p2
  
  p1 * p2 = toFinPrec $ pLift2 (*) p1 p2

-- lift any Rational Instance arity 1 to the rational part of the maybe rational Infinite Precision Posit
pLift1 :: (Rational -> Rational) -> Posit es -> Posit es
pLift1 _ (toInfPrec -> InfPrec Nothing) = InfPrec Nothing
pLift1 f (toInfPrec -> InfPrec (Just r)) = InfPrec (Just (f r))

-- lift any Rational Instance arity 2 to the rational part of the maybe rational Infinite Precision Posit
pLift2 :: (Rational -> Rational -> Rational) -> Posit es -> Posit es -> Posit es
pLift2 _ (toInfPrec -> InfPrec Nothing) _ = InfPrec Nothing
pLift2 _ _ (toInfPrec -> InfPrec Nothing) = InfPrec Nothing
pLift2 f (toInfPrec -> InfPrec (Just r1)) (toInfPrec -> InfPrec (Just r2)) = InfPrec (Just (r1 `f` r2))


instance forall es.(KnownNat es) => RealFrac (Posit es) where
  properFraction (InfPrec Nothing) = error "The Posit is no longer Real, a divide by zero issue, or it should have been a complex number"
  properFraction (InfPrec (Just r)) =
    let (int,r') = properFraction r
    in (int,InfPrec (Just r'))
  properFraction fin@FinPrec{} = properFraction $ toInfPrec fin



-- where we have a serialization or integer encoding to a fixed size integer IntN es
pRound :: forall es.(KnownNat es) => Posit es -> Posit es
pRound = decode.encode


-- encode will take a Posit number and convert to a Finite Precision Posit number
encode :: forall es.(KnownNat es) => Posit es -> Posit es
encode fin@FinPrec{} = fin
encode p@(InfPrec Nothing) = mkNaR p
encode (InfPrec (Just 0)) = FinPrec (zeroBits @(IntN es))
encode p | p > maxPos p = encode $ maxPos p
         | p < minNeg p = encode $ minNeg p
         | p > 0 && p < minPos p = encode $ minPos p
         | p < 0 && p > maxNeg p = encode $ maxNeg p
         | otherwise = buildIntRep p


mkNaR :: forall es.(KnownNat es) => Posit es -> Posit es
mkNaR _ = FinPrec (minBound @(IntN es))


buildIntRep :: Posit es -> Posit es
buildIntRep p =
  let (s,r,e,p') = posit2TupPosit p  -- sign s, regime r, exponent e, significand p'
      intRep = mkIntRep r e p'
  in if s
     then FinPrec (negate intRep)
     else FinPrec intRep

mkIntRep :: Integer -> Natural -> Posit es -> IntN es
mkIntRep r e p =
  let (r', offset) = formRegime p r  -- offset is the number of binary digits remaining after the regime is formed
      (e', offset') = formExponent p e offset  -- offset' is the number of binary digits remaining after the exponent is formed
      fraction = formFraction p (toInteger offset')
  in r' .|. e' .|. fraction

formRegime :: Posit es -> Integer -> (IntN es, Integer)
formRegime posit power | 0 <= power = 
                          let offset = (fromIntegral (nBits posit - 1) -     power - 1)
                          in (fromIntegral (2^(power + 1) - 1) `shiftL` fromInteger offset, offset - 1)
                       | otherwise = 
                          let offset = (fromIntegral (nBits posit - 1) - abs power - 1)
                          in (1 `shiftL` fromInteger offset, offset)


formExponent :: Posit es -> Natural -> Integer -> (IntN es, Integer)
formExponent posit power offset = 
  let offset' = offset - fromIntegral (exponentSize posit)
  in (fromIntegral power `shift` fromInteger offset', offset')


formFraction :: Posit es -> Integer -> IntN es
formFraction posit offset =
  let numFractionBits = offset
      fractionSize = 2^numFractionBits
      normFraction = round $ (posit - 1) * fractionSize  -- "posit - 1" is changing it from the significand to the fraction: [1,2) -> [0,1)
  in if numFractionBits >= 1
     then fromInteger normFraction
     else 0

decode :: forall es.(KnownNat es) => Posit es -> Posit es
decode int | int == minBound @(IntN es) = InfPrec Nothing
           | int == 0 = InfPrec (Just 0)
           | otherwise =
               let s = int < 0
                   int' = if s
                          then negate int
                          else int
                   (r,nR) = regime2Integer int'
                   e = exponent2Nat nR int'  -- if no e or some bits missing, then they are considered zero
                   p = fraction2Posit nR int'  -- if no fraction or some bits missing, then the missing bits are zero, making the significand p=1
               in tupPosit2Posit (s,r,e,p)



regime2Integer :: IntN es -> (Integer, Int)
regime2Integer posit =
  let regimeFormat = findRegimeFormat posit
      regimeCount = countRegimeBits regimeFormat posit
      regime = calcRegimeInt regimeFormat regimeCount
  in (regime, regimeCount + 1) -- a rational representation of the regime and the regimeCount plus rBar which is the numBitsRegime

calcRegimeInt :: Bool -> Int -> Integer
calcRegimeInt format count | format = fromIntegral (count - 1)
                           | otherwise = fromIntegral $ negate count

countRegimeBits :: forall es.(KnownNat es, Bits (IntN es), Integral (IntN es)) => Bool -> IntN es -> Int
countRegimeBits format posit = go (fromIntegral (nBits (undefined :: Posit es)) - 1 - fromIntegral signBitSize) 0
  where
    go (-1) acc = acc
    go index acc | xnor format (testBit posit index)  = go (index - 1) (acc + 1)
                 | otherwise = acc

xnor :: Bool -> Bool -> Bool
xnor a b = not $ (a || b) && not (b && a)

-- will return the format of the regime, either HI or LO; it could get refactored in the future
-- True means a 1 is the first bit in the regime
findRegimeFormat :: forall es.(KnownNat es) => IntN es -> Bool
findRegimeFormat posit = testBit posit (fromIntegral (nBits (undefined :: Posit es)) - 1 - fromIntegral signBitSize)


-- knowing the number of the regime bits, and the sign bit we can extract
-- the exponent.  We mask to the left of the exponent to remove the sign and regime, and
-- then shift to the right to remove the fraction.
exponent2Nat :: forall es.(KnownNat es) => Int -> IntN es -> Natural
exponent2Nat numBitsRegime posit =
  let signNRegimeMask :: IntN es = 2^(fromIntegral (nBits (undefined :: Posit es)) - numBitsRegime - fromIntegral signBitSize) - 1
      int = posit .&. signNRegimeMask
      nBitsToTheRight = fromIntegral (nBits (undefined :: Posit es)) - numBitsRegime - fromIntegral signBitSize - fromIntegral (exponentSize (undefined :: Posit es))
  in fromIntegral $ int `shiftR` nBitsToTheRight

-- knowing the number of the regime bits, sign bit, and the number of the 
-- exponent bits we can extract the fraction.  We mask to the left of the fraction to 
-- remove the sign, regime, and exponent. If there is no fraction then the value is 1.
fraction2Posit :: forall es.(KnownNat es, Bits (IntN es), Integral (IntN es)) => Int -> IntN es -> Posit es
fraction2Posit numBitsRegime posit = 
  let offset = fromIntegral $ signBitSize + fromIntegral numBitsRegime + exponentSize (undefined :: Posit es)
      fractionSize = fromIntegral (nBits (undefined :: Posit es) :: Natural) - offset
      fractionBits = posit .&. (2^fractionSize - 1)
  in if fractionSize >= 1
     then InfPrec $ Just $ (2^fractionSize + toInteger fractionBits) % 2^fractionSize
     else InfPrec $ Just $ 1 % 1


-- 'posit2TupPosit' is the exact decomposition of a Posit number
-- extracting the sign bit, the regime, the exponent, and a Posit Number
-- with the range of [1,2) called the significand.  The input posit must
-- be bounds checked, and between [minNeg,maxNeg] || [minPos,maxPos].
-- 
-- The following law holds:
-- tupPosit2Posit.posit2TupPosit == id
-- 
posit2TupPosit :: forall es.(KnownNat es) => Posit es -> (Bool, Integer, Natural, Posit es)
posit2TupPosit p =
  let (s,p') = getSign p -- returns the sign and a positive posit
      (r,p'') = getRegime p' -- returns the regime and a posit between uSeed^-1 to uSeed^1
      (e,p''') = getExponent p'' -- returns the exponent and a posit between [1,2), the significand
  in (s,r,e,p''')


-- getSign finds the sign value and then returns the absolute value of the Posit
getSign :: forall es.(KnownNat es) => Posit es -> (Bool, Posit es)
getSign p =
  let s = p <= 0
      absPosit = if s
                 then negate p
                 else p
  in (s,absPosit)  -- pretty much the same as 'abs')

getRegime :: forall es.(KnownNat es) => Posit es -> (Integer, Posit es)
getRegime p = log_uSeed (0,p)

-- After calculating the regime the posit should be in the range [1,uSeed)
log_uSeed :: forall es.(KnownNat es) => (Integer, Posit es) -> (Integer, Posit es)
log_uSeed (r,p) | p < fromRational (1 % 1) = log_uSeed (r-1,p * fromRational (toInteger (uSeed p) % 1))
                | p >= fromRational (toInteger (uSeed p) % 1) = log_uSeed (r+1,p * fromRational (1 % toInteger (uSeed p)))
                | otherwise = (r,p)

-- Exponent should be an integer in the range of [0,uSeed), and also return the posit [1,2)
getExponent :: forall es.(KnownNat es) => Posit es -> (Natural, Posit es)
getExponent p = log_2 (0,p)

log_2 :: forall es.(KnownNat es) => (Natural, Posit es) -> (Natural, Posit es)
log_2 (e,p) | p < fromRational 1 = error "Should never happen, exponent should be a natural number, i.e. positive integer."
            | p >= fromRational (2 % 1) = log_2 (e+1,p * fromRational (1 % 2))
            | otherwise = (e,p)

tupPosit2Posit :: forall es.(KnownNat es) => (Bool,Integer,Natural,Posit es) -> Posit es
tupPosit2Posit (s,r,e,p) = -- s = isNeg posit == True
  let pow2 = (toRational $ uSeed p)^^r * 2^e
      scale :: Rational = if s
                          then negate pow2
                          else pow2
  in fromRational scale * p

-- The smallest positive nonzero value expressible as a posit
minPos :: forall es.(KnownNat es) => Posit es -> Posit es
minPos p =
  let n = toInteger $ nBits p
      nEighth = quot n 8
  in InfPrec (Just (1 % (2^(nEighth * (n-2)))))

-- The largest positive value expressible as a posit
maxPos :: forall es.(KnownNat es) => Posit es -> Posit es
maxPos p =
  let n = toInteger $ nBits p
      nEighth = quot n 8
  in InfPrec (Just ((2^(nEighth * (n-2))) % 1))

-- The smallest negative value expressible as a posit, yet NaR is smaller
minNeg :: forall es.(KnownNat es) => Posit es -> Posit es
minNeg p =
  let n = toInteger $ nBits p
      nEighth = quot n 8
  in InfPrec (Just ((2^(nEighth * (n-2))) % (-1)))

-- The largest negative nonzero value expressible as a posit
maxNeg :: forall es.(KnownNat es) => Posit es -> Posit es
maxNeg p =
  let n = toInteger $ nBits p
      nEighth = quot n 8
  in InfPrec (Just ((-1) % (2^(nEighth * (n-2)))))


-- The implemented type synonyms:
type Posit8 = Posit 0
type Posit16 = Posit 1
type Posit32 = Posit 2
type Posit64 = Posit 3
type Posit128 = Posit 4
type Posit256 = Posit 5
-- so that,
-- > :t (undefined :: Posit8)
-- (undefined :: Posit8) :: Posit8

-- These Nat indexed type synonyms have an alis of Posit8, through Posit256.
-- The algebraic data types are constructed with Posit 0, through Posit 5.

-- Coersions between different Posit types are required
coerse :: forall es1.forall es0.(KnownNat es1,KnownNat es0) => Posit es0 -> Posit es1 -- Type application switcharoo
coerse (InfPrec Nothing) = InfPrec Nothing :: Posit es1
coerse (InfPrec (Just r)) = InfPrec (Just r) :: Posit es1 -- TODO insert (pRound $) when we have an integer encoding
coerse fin@FinPrec{} = coerse (toInfPrec fin)

incPrec :: forall es.(KnownNat es, KnownNat (es + 1)) => Posit es -> Posit (es + 1)
incPrec = coerse @(es + 1)

decPrec :: forall es.(KnownNat es, KnownNat (es - 1)) => Posit es -> Posit (es - 1)
decPrec = coerse @(es - 1)


-- Number of bits.  The number of bits of a posit format, the total number of bits (8, 16, 32, or 64)
nBits :: forall es.(KnownNat es) => Posit es -> Natural
nBits _ = 8 * (2^exponentSize (undefined :: Posit es))
-- so that,
-- > nBits (undefined :: Posit8)
-- 8

-- and,
-- > :t nBits (undefined :: Posit8)
-- nBits (undefined :: Posit8) :: Natural

-- and,
nBytes :: forall es.(KnownNat es) => Posit es -> Natural
nBytes _ = 2^exponentSize (undefined :: Posit es)
-- so that,
-- > nBytes (undefined :: Posit64)
-- 8

-- and,
-- > :t nBytes (undefined :: Posit64)
-- nBytes (undefined :: Posit64) :: Natural

signBitSize :: Natural
signBitSize = 1


-- This firm seperation can enforce type level garuitees, and runtime,
-- garentees that some of the functions will pass the same functions as
-- Integral types.

-- So, for instance 

-- Let's write an injective type family.  The Natural number value at the
-- type level defines the precision of the integral type.  And if you
-- know the precision of the integral type you can know the natural number.
type family IntN (es :: Nat) = int | int -> es where
  IntN 0 = Int8
  IntN 1 = Int16
  IntN 2 = Int32
  IntN 3 = Int64
  IntN 4 = Int128
  IntN 5 = Int256


-- so that,
-- > :instances IntN 0
-- instance Eq Int8 -- Defined in ‘GHC.Int’
-- instance Ord Int8 -- Defined in ‘GHC.Int’
-- instance Enum Int8 -- Defined in ‘GHC.Int’
-- instance Num Int8 -- Defined in ‘GHC.Int’
-- instance Real Int8 -- Defined in ‘GHC.Int’
-- instance Show Int8 -- Defined in ‘GHC.Int’
-- instance Read Int8 -- Defined in ‘GHC.Int’
-- instance Bits Int8 -- Defined in ‘GHC.Int’
-- instance Bounded Int8 -- Defined in ‘GHC.Int’
-- instance FiniteBits Int8 -- Defined in ‘GHC.Int’
-- instance Integral Int8 -- Defined in ‘GHC.Int’
-- instance GHC.Ix.Ix Int8 -- Defined in ‘GHC.Int’
-- instance Show Int8 -- Defined in ‘GHC.Int’
-- instance Eq Int8 -- Defined in ‘GHC.Int’
-- instance Ord Int8 -- Defined in ‘GHC.Int’
-- instance Real Int8 -- Defined in ‘GHC.Int’
-- instance Num Int8 -- Defined in ‘GHC.Int’


-- It's True:
-- > signum (2 :: IntN 2)
-- 1
-- > signum (-2 :: IntN 2)
-- -1
-- > signum (0 :: IntN 2)
-- 0
-- > :t signum (0 :: IntN 2)
-- signum (0 :: IntN 2) :: Int32



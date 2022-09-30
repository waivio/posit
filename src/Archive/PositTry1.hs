-- Trouble with (TypeFamilies/TypeFaimlyDependencies, GADTs, DataKinds)

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}  -- The 'IntN es' has to be an injective type family, this turns on TypeFamilies
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}  -- for various coerse expermentation
{-# LANGUAGE GADTs #-}

{-# LANGUAGE InstanceSigs #-} -- For good documenting code style


module Data.Posit
(Posit8,
 Posit16,
 Posit32,
 Posit64,
 Posit128,
 Posit256) where


import GHC.Natural
import GHC.TypeNats

import Data.Ratio  -- Rational numbers can get arbitrarrarlrary close to Real numbers


-- 2's complement format integers:
import Data.Int (Int8,Int16,Int32,Int64)  -- Import standard Int sizes
import Data.DoubleWord (Int128,Int256) -- Import large Int sizes
import Data.Bits

-- import Debug.Trace  -- for debuging purposes


-- for showing the int in binary
import Numeric (showIntAtBase)
import Data.Char (intToDigit)
showBinary :: Integral a => a -> String
showBinary x =
  let x' = toInteger x
  in showIntAtBase 2 intToDigit (fromInteger x' :: Word) ""


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
  Posit :: KnownNat es => Maybe Rational -> Posit es

instance Show (Posit es) where
  show (Posit Nothing) = "NaR"
  show (Posit (Just r)) = show r  -- TODO should make more pretty

-- Equal like integers IntN
instance forall es.(KnownNat es) => Eq (Posit es) where
  (==) :: Posit es -> Posit es -> Bool
  (Posit Nothing) == (Posit Nothing) = True
  (Posit Nothing) == (Posit (Just _)) = False
  (Posit (Just _)) == (Posit Nothing) = False
  (Posit (Just r1)) == (Posit (Just r2)) = r1 == r2 -- TODO should be integer Eq, when we have an integer encoding 

-- Ordered like the rational instance, with some special rules where NaR is smaller than the Reals
instance forall es.(KnownNat es) => Ord (Posit es) where
  compare :: Posit es -> Posit es -> Ordering
  compare (Posit Nothing) (Posit Nothing) = EQ
  compare (Posit Nothing) _ = LT
  compare _ (Posit Nothing) = GT
  compare (Posit (Just r1)) (Posit (Just r2)) = compare r1 r2

instance forall es.(KnownNat es) => Fractional (Posit es) where
  fromRational :: Rational -> Posit es
  fromRational r = Posit (Just r)
  
  recip :: Posit es -> Posit es
  recip (Posit Nothing) = Posit Nothing
  recip (Posit (Just 0)) = Posit Nothing
  recip (Posit (Just r)) = Posit (Just (recip r))

instance forall es.(KnownNat es) => Real (Posit es) where
  toRational :: Posit es -> Rational
  toRational (Posit Nothing) = error "The Posit is no longer Real, a divide by zero issue, or it should have been a complex number"
  toRational (Posit (Just r)) = r

instance forall es.(KnownNat es) => Num (Posit es) where
  fromInteger :: Integer -> Posit es
  fromInteger int = Posit (Just (int % 1))
  
  signum :: Posit es -> Posit es
  signum (Posit Nothing) = 0
  signum (Posit (Just r)) = Posit (Just $ signum r)
  
  negate :: Posit es -> Posit es
  negate p = pLift1 negate p
  
  abs :: Posit es -> Posit es
  abs p = pLift1 abs p
  
  (+) :: Posit es -> Posit es -> Posit es
  p1 + p2 = pLift2 (+) p1 p2
  -- p1 + p2 = pRound $ pLift2 (+) p1 p2
  
  (*) :: Posit es -> Posit es -> Posit es
  p1 * p2 = pLift2 (*) p1 p2


instance forall es.(KnownNat es) => RealFrac (Posit es) where
  properFraction (Posit Nothing) = error "The Posit is no longer Real, divide by zero, or should be a complex number"
  properFraction (Posit (Just r)) =
    let (int,r') = properFraction r
    in (int,Posit (Just r'))


pLift1 :: forall es.(KnownNat es) => (Rational -> Rational) -> Posit es -> Posit es
pLift1 _ (Posit Nothing) = Posit Nothing
pLift1 f (Posit (Just r)) = Posit (Just (f r))

pLift2 :: forall es.(KnownNat es) => (Rational -> Rational -> Rational) -> Posit es -> Posit es -> Posit es
pLift2 _ (Posit Nothing) _ = Posit Nothing
pLift2 _ _ (Posit Nothing) = Posit Nothing
pLift2 f (Posit (Just r1)) (Posit (Just r2)) = Posit (Just (r1 `f` r2))




-- where we have a serialization or integer encoding to a fixed size integer IntN es
pRound :: forall es.(KnownNat es, Bits (IntN es), Bounded (IntN es), Integral (IntN es)) => Posit es -> Posit es
pRound = decode.encode



encode :: forall es.(KnownNat es, Bits (IntN es), Num (IntN es), Bounded (IntN es)) => Posit es -> IntN es
encode p@(Posit Nothing) = mkNaR p
encode (Posit (Just 0)) = zeroBits @(IntN es)
encode p | p > maxPos p = encode $ maxPos p
         | p < minNeg p = encode $ minNeg p
         | p > 0 && p < minPos p = encode $ minPos p
         | p < 0 && p > maxNeg p = encode $ maxNeg p
         | otherwise = buildIntRep p

mkNaR :: forall es.(KnownNat es, Bounded (IntN es)) => Posit es -> IntN es
mkNaR _ = minBound @(IntN es)


buildIntRep :: forall es.(KnownNat es, Bits (IntN es), Num (IntN es)) => Posit es -> IntN es
buildIntRep p =
  let (s,r,e,p') = posit2TupPosit p
      intRep = mkIntRep r e p'
  in if s == 0
     then intRep
     else negate intRep

mkIntRep :: forall es.(KnownNat es, Bits (IntN es), Num (IntN es)) => Integer -> Integer -> Posit es -> IntN es
mkIntRep r e p =
  let (r', offset) = formRegime p r  -- offset is the number of binary digits remaining after the regime is formed
      (e', offset') = formExponent p e offset  -- offset' is the number of binary digits remaining after the exponent is formed
      fraction = formFraction p (toInteger offset')
  in r' .|. e' .|. fraction

formRegime :: forall es.(KnownNat es, Bits (IntN es), Num (IntN es)) => Posit es -> Integer -> (IntN es, Integer)
formRegime posit power | 0 <= power = 
                          let offset = (fromIntegral (nBits posit - 1) -     power - 1)
                          in (fromIntegral (2^(power + 1) - 1) `shiftL` fromInteger offset, offset - 1)
                       | otherwise = 
                          let offset = (fromIntegral (nBits posit - 1) - abs power - 1)
                          in (1 `shiftL` fromInteger offset, offset)


formExponent :: forall es.(KnownNat es, Bits (IntN es), Num (IntN es)) => Posit es -> Integer -> Integer -> (IntN es, Integer)
formExponent posit power offset = 
  let offset' = offset - fromIntegral (exponentSize posit)
  in (fromIntegral power `shift` fromInteger offset', offset')


formFraction :: forall es.(KnownNat es, Bits (IntN es), Num (IntN es)) => Posit es -> Integer -> IntN es
formFraction posit offset =
  let numFractionBits = offset -- - (fromIntegral.exponentSize $ posit)
      fractionSize = 2^numFractionBits
      normFraction = round $ (posit - 1) * fractionSize  -- "posit - 1" is changing it from the significand to the fraction: [1,2) -> [0,1)
  in if numFractionBits >= 1
     then fromInteger normFraction
     else 0

decode :: forall es.(KnownNat es, Bits (IntN es), Num (IntN es), Bounded (IntN es), Ord (IntN es), Integral (IntN es)) => IntN es -> Posit es
decode int | int == minBound @(IntN es) = Posit Nothing
           | int == 0 = Posit (Just 0)
           | otherwise =
               let s = if int < 0
                       then 1
                       else 0
                   int' = if s == 1
                          then negate int
                          else int
                   (r,nR) = regime2Integer int'
                   e = exponent2Int nR int'  -- if no e or some bits missing, then they are considered zero
                   p = fraction2Posit nR int'  -- if no fraction or some bits missing, then the missing bits are zero, making the significand p=1
               in tupPosit2Posit (s,r,e,p)



regime2Integer :: forall es.(KnownNat es, Bits (IntN es), Num (IntN es), Integral (IntN es)) => IntN es -> (Integer, Int)
regime2Integer posit = 
  let regimeFormat = findRegimeFormat posit
      regimeCount = countRegimeBits regimeFormat posit
      regime = calcRegimeInt regimeFormat regimeCount
  in (regime, regimeCount + 1) -- a rational representation of the regime and the regimeCount plus r_bar which is the numBitsRegime

calcRegimeInt :: Bool -> Int -> Integer
calcRegimeInt format count | format = fromIntegral (count - 1)
                           | otherwise = fromIntegral $ negate count

countRegimeBits :: forall es.(KnownNat es, Bits (IntN es), Num (IntN es), Integral (IntN es)) => Bool -> IntN es -> Int
countRegimeBits format posit = go (fromIntegral (nBits (undefined :: Posit es)) - 1 - fromIntegral signBitSize) 0
  where
    go (-1) acc = acc
    go index acc | xnor format (testBit posit index)  = go (index - 1) (acc + 1)
                 | otherwise = acc

xnor :: Bool -> Bool -> Bool
xnor a b = not $ (a || b) && not (b && a)

-- will return the format of the regime, either HI or LO; it could get refactored in the future
-- True means a 1 is the first bit in the regime
findRegimeFormat :: forall es.(KnownNat es, Bits (IntN es), Num (IntN es), Integral (IntN es)) => IntN es -> Bool
findRegimeFormat posit = testBit posit (fromIntegral (nBits (undefined :: Posit es)) - 1 - fromIntegral signBitSize)


 -- fromIntegral $ nBits (undefined :: Posit es) - exponentSize (undefined :: Posit es)  -- Move enough to leave only the exponent
-- nBitsToTheLeft = (fromIntegral signBitSize) + numBitsRegime  -- Move enought to remove the sign and regime
-- knowing the number of the regime bits, and the sign bit we can extract
-- the exponent.  We shift to the left to remove the sign and regime, and
-- then shift to the right to remove the fraction.
exponent2Int :: forall es.(KnownNat es, Bits (IntN es), Num (IntN es), Integral (IntN es)) => Int -> IntN es -> Integer
exponent2Int numBitsRegime posit =
  let signNRegimeMask :: IntN es = 2^(fromIntegral (nBits (undefined :: Posit es)) - numBitsRegime - fromIntegral signBitSize) - 1
      int = posit .&. signNRegimeMask
      nBitsToTheRight = fromIntegral (nBits (undefined :: Posit es)) - numBitsRegime - fromIntegral signBitSize - fromIntegral (exponentSize (undefined :: Posit es))
  in fromIntegral $ int `shiftR` nBitsToTheRight

-- knowing the number of the regime bits, sign bit, and the number of the 
-- exponent bits we can extract the fraction.  We shift to the left to 
-- remove the sign, regime, and exponent. then shift to the right to scale
-- the fraction.
fraction2Posit :: forall es.(KnownNat es, Bits (IntN es), Num (IntN es), Integral (IntN es)) => Int -> IntN es -> Posit es
fraction2Posit numBitsRegime posit = 
  let offset = fromIntegral $ signBitSize + fromIntegral numBitsRegime + exponentSize (undefined :: Posit es)
      fractionSize = fromIntegral (nBits (undefined :: Posit es) :: Natural) - offset
      fractionBits = posit .&. (2^fractionSize - 1)
  in if fractionSize >= 1
     then Posit $ Just $ (2^fractionSize + toInteger fractionBits) % 2^fractionSize
     else Posit $ Just $ 1 % 1


-- 'posit2TupPosit' is the exact decomposition of a Posit number
-- extracting the sign bit, the regime, the exponent, and a Posit Number
-- with the range of [1,2) called the significand.  The input posit must
-- be bounds checked, and between [minNeg,maxNeg] || [minPos,maxPos].
-- 
-- The following law holds:
-- tupPosit2Posit.posit2TupPosit = id
-- 
posit2TupPosit :: forall es.(KnownNat es) => Posit es -> (Integer, Integer, Integer, Posit es)
posit2TupPosit p =
  let (s,p') = getSign p -- returns the sign and a positive posit
      (r,p'') = getRegime p' -- returns the regime and a posit between uSeed^-1 to uSeed^1
      (e,p''') = getExponent p'' -- returns the exponent and a posit between [1,2), the significand
  in (s,r,e,p''')


getSign :: forall es.(KnownNat es) => Posit es -> (Integer, Posit es)
getSign p =
  let s = if signum p >= 0
          then 0
          else 1
      absPosit = if s == 0
                 then p
                 else negate p
  in (s,absPosit)  -- pretty much the same as 'abs')

getRegime :: forall es.(KnownNat es) => Posit es -> (Integer, Posit es)
getRegime p = log_uSeed (0,p)

-- After calculating the regime the posit should be in the range [1,uSeed)
log_uSeed :: forall es.(KnownNat es) => (Integer, Posit es) -> (Integer, Posit es)
log_uSeed (r,p) | p < fromRational (1 % 1) = log_uSeed (r-1,p * fromRational (toInteger (uSeed p) % 1))
                | p >= fromRational (toInteger (uSeed p) % 1) = log_uSeed (r+1,p * fromRational (1 % toInteger (uSeed p)))
                | otherwise = (r,p)

-- Exponent should be an integer in the range of [0,uSeed), and the posit [1,2)
getExponent :: forall es.(KnownNat es) => Posit es -> (Integer, Posit es)
getExponent p = log_2 (0,p)

log_2 :: forall es.(KnownNat es) => (Integer, Posit es) -> (Integer, Posit es)
log_2 (e,p) | p < fromRational 1 = error "Should never happen, exponent should be a natural number, i.e. positive integer."
            | p >= fromRational (2 % 1) = log_2 (e+1,p * fromRational (1 % 2))
            | otherwise = (e,p)

tupPosit2Posit :: forall es.(KnownNat es) => (Integer,Integer,Integer,Posit es) -> Posit es
tupPosit2Posit (s,r,e,p) =
  let scale = fromRational $ (-1)^s * toRational (uSeed p)^^r * toRational 2^e
  in scale * p

-- The smallest positive nonzero value expressible as a posit
minPos :: forall es.(KnownNat es) => Posit es -> Posit es
minPos p =
  let n = toInteger $ nBits p
      nEighth = quot n 8
  in Posit (Just (1 % (2^(nEighth * (n-2)))))

-- The largest positive value expressible as a posit
maxPos :: forall es.(KnownNat es) => Posit es -> Posit es
maxPos p =
  let n = toInteger $ nBits p
      nEighth = quot n 8
  in Posit (Just ((2^(nEighth * (n-2))) % 1))

-- The smallest negative value expressible as a posit, yet NaR is smaller
minNeg :: forall es.(KnownNat es) => Posit es -> Posit es
minNeg p =
  let n = toInteger $ nBits p
      nEighth = quot n 8
  in Posit (Just ((2^(nEighth * (n-2))) % (-1)))

-- The largest negative nonzero value expressible as a posit
maxNeg :: forall es.(KnownNat es) => Posit es -> Posit es
maxNeg p =
  let n = toInteger $ nBits p
      nEighth = quot n 8
  in Posit (Just ((-1) % (2^(nEighth * (n-2)))))


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
coerse (Posit Nothing) = Posit Nothing :: Posit es1
coerse (Posit (Just r)) = Posit (Just r) :: Posit es1 -- TODO insert (pRound $) when we have an integer encoding

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
-- instance Integral Int8 -- Defined in ‘GHC.Int’
-- instance Read Int8 -- Defined in ‘GHC.Int’
-- instance Data.BinaryWord.BinaryWord Int8
--   -- Defined in ‘Data.BinaryWord’
-- instance Data.Bits.Bits Int8 -- Defined in ‘GHC.Int’
-- instance Bounded Int8 -- Defined in ‘GHC.Int’
-- instance Data.Data.Data Int8 -- Defined in ‘Data.Data’
-- instance Data.Bits.FiniteBits Int8 -- Defined in ‘GHC.Int’
-- instance hashable-1.3.0.0:Data.Hashable.Class.Hashable Int8
--   -- Defined in ‘hashable-1.3.0.0:Data.Hashable.Class’
-- instance GHC.Ix.Ix Int8 -- Defined in ‘GHC.Int’
-- instance binary-0.8.8.0:Data.Binary.Class.Binary Int8
--   -- Defined in ‘binary-0.8.8.0:Data.Binary.Class’
-- instance [safe] Control.DeepSeq.NFData Int8
--   -- Defined in ‘Control.DeepSeq’
-- instance [safe] Text.Printf.PrintfArg Int8
--   -- Defined in ‘Text.Printf’


-- It's True:
-- > signum (2 :: IntN 2)
-- 1
-- > signum (-2 :: IntN 2)
-- -1
-- > signum (0 :: IntN 2)
-- 0
-- > :t signum (0 :: IntN 2)
-- signum (0 :: IntN 2) :: Int32

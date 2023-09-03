
--------------------------------------------------------------------------------------------
-- | Posit Numbers
--   Copyright   :  (C) 2022-2023 Nathan Waivio
--   License     :  BSD3
--   Maintainer  :  Nathan Waivio <nathan.waivio@gmail.com>
--   Stability   :  Stable
--   Portability :  Portable
--
--   Test Suite for a Library implementing standard Posit Numbers
-- 
---------------------------------------------------------------------------------------------

-- Hmm... phi is the most irrational number?

{-# LANGUAGE ScopedTypeVariables #-} --   To reduce some code duplication, this is important
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

import Posit
import Posit.Internal.PositC

import Data.List (partition)

import Data.Ratio ((%))  -- Import the Rational Numbers ℚ (u+211A), ℚ can get arbitrarily close to Real numbers ℝ (u+211D), used for some of the Transcendental Functions

main :: IO ()
main = do
  let (pPosit8, fPosit8) = partition testCase [ x | x <- enumFrom (NaR :: Posit8)]
      lpPosit8 = length pPosit8 :: Int -- Int
      lfPosit8 = length fPosit8 :: Int -- Int
  print $ "Fraction Error (read.show) :: Posit8 : " ++ show (lfPosit8 % (lpPosit8 + lfPosit8))
  print $ "Failure case: " ++ show fPosit8
  let (pP8, fP8) = partition testCase [ x | x <- enumFrom (NaR :: P8)]
      lpP8 = length pP8 :: Int
      lfP8 = length fP8 :: Int
  print $ "Fraction Error (read.show) :: P8 : " ++ show (lfP8 % (lpP8 + lfP8))
  print $ "Failure case: " ++ show fP8
  let (pPosit16, fPosit16) = partition testCase [ x | x <- enumFrom (NaR :: Posit16)]
      lpPosit16 = length pPosit16 :: Int -- Int
      lfPosit16 = length fPosit16 :: Int -- Int
  print $ "Fraction Error (read.show) :: Posit16 : " ++ show (lfPosit16 % (lpPosit16 + lfPosit16))
  print $ "Failure case: " ++ show fPosit16
  let (pP16, fP16) = partition testCase [ x | x <- enumFrom (NaR :: P16)]
      lpP16 = length pP16 :: Int
      lfP16 = length fP16 :: Int
  print $ "Fraction Error (read.show) :: P16 : " ++ show (lfP16 % (lpP16 + lfP16))
  print $ "Failure case: " ++ show fP16
  -- Larger size can only test percentage
  --
  let lfPosit32 = length [ x | x <- enumFrom (NaR :: Posit32), not (testCase x)]
  print $ "Fraction Error (read.show) :: Posit32 : " ++ show (lfPosit32 % 2^32)
  let lfP32 = length [ x | x <- enumFrom (NaR :: P32), not (testCase x)]
  print $ "Fraction Error (read.show) :: P32 : " ++ show (lfP32 % 2^32)
  --


testCase :: forall es. PositC es => Posit es -> Bool
testCase x = x == ((read @(Posit es)).show $ x)


-- * Gustafson's coorispondance with Alessandro
-- 
-- Decimal accuracy is not the same as the minimal number of decimals
-- needed to assure conversion back to the original posit. The latter is
-- larger. For example, a 32-bit posit has 28 bits of significance in
-- its highest-accuracy region, which is about:
-- > 28 * log10(2) + 0.5 log10(2) ≈ 8.6 
-- decimals of minimum relative accuracy, but if you only use 9 decimals
-- to express a posit and you convert those 9 decimals back into a
-- posit, occasionally you will get a different posit from what you
-- started with.

-- * And then responding again to Alessandro

-- First of all, there are two regime bits (01 or 10), not one. The
-- termination (opposite) bit is part of the regime. That leaves 27 bits
-- for the fraction, which is 28 bits of significand including the
-- hidden value left of the radix point.

-- Yes, your formula should then work, but remember that you have
-- wobbling relative accuracy (wobble of ±½ log10(2) ≈ ±0.15 decimals)
-- for both posits and floats within any given binade. You should
-- probably be pessimistic and use the lower bound on the sawtooth shape
-- if you are trying to prove something about the accuracy of a
-- computation.


-- * During test of "round tripping" a Decimal text string, a rounding
-- issue was found.

-- After resolving a rounding error that added many failure cases when 
-- there should have been rounding for when `2 * remainingSignificand - 1 > 1`
-- for Posits with no fraction.
-- >  ...
-- >      else if 2 * (r - 1) > 1 -- Previously was 0, this should be "banker's rounding" as per Gustafson (upcoming Ch3), where if `2*(r - 1) > 1` then 1 else 0
-- >           then 1
-- >           else 0


-- The baseline results using `decimalPrec = fromIntegral $ 2 * (nBytes @es) + 1` -- [3,5,9,17,33,65]
-- Benchmark test-posit-readShowId: RUNNING...
-- "Fraction Error (read.show) :: Posit8 : 0 % 1"
-- "Failure case: []" 
-- "Fraction Error (read.show) :: P8 : 1 % 128"
-- "Failure case: [-2.441e-4,2.441e-4]"
-- "Fraction Error (read.show) :: Posit16 : 1 % 8192"
-- "Failure case: [-2.68435e8,-3.35544e7,-5.96046e-8,-2.98023e-8,2.98023e-8,5.96046e-8,3.35544e7,2.68435e8]"
-- "Fraction Error (read.show) :: P16 : 3 % 16384"
-- "Failure case: [-1.40737e14,-7.03687e13,-5.68434e-14,-2.84217e-14,-3.55271e-15,-8.88178e-16,8.88178e-16,3.55271e-15,2.84217e-14,5.68434e-14,7.03687e13,1.40737e14]"
-- "Fraction Error (read.show) :: Posit32 : 37904825 % 2147483648"
-- "Fraction Error (read.show) :: P32 : 37904825 % 2147483648"
-- Benchmark test-posit-readShowId: FINISH
--
-- After bug fix:
-- Benchmark test-posit-readShowId: RUNNING...
-- "Fraction Error (read.show) :: Posit8 : 0 % 1"
-- "Failure case: []" 
-- "Fraction Error (read.show) :: P8 : 0 % 1"
-- "Failure case: []" 
-- "Fraction Error (read.show) :: Posit16 : 0 % 1"
-- "Failure case: []" 
-- "Fraction Error (read.show) :: P16 : 0 % 1"
-- "Failure case: []" 
-- "Fraction Error (read.show) :: Posit32 : 37904819 % 2147483648"
-- "Fraction Error (read.show) :: P32 : 37904819 % 2147483648"
-- Benchmark test-posit-readShowId: FINISH


-- Gustafson's Decimal Accuracy: -- []
-- >   decimalPrec posit =
-- >    let regimeFormat = findRegimeFormat @es posit
-- >        regimeCount = countRegimeBits @es regimeFormat posit
-- >        fractionSize = max (fromIntegral (nBits @es) - fromIntegral (signBitSize @es) - regimeCount - fromIntegral (exponentSize @es)) 0  -- fractionSize is at least zero
-- >    in ceiling $ (fromIntegral fractionSize + 1) * log10Of2 + halflog10Of2
-- Benchmark test-posit-readShowId: RUNNING...
-- "Fraction Error (read.show) :: Posit8 : 1 % 128"
-- "Failure case: [-3.1e-2,3.12e-2]"
-- "Fraction Error (read.show) :: P8 : 7 % 128"
-- "Failure case: [-0.12,-2.4e-4,-1.2e-4,-6.1e-5,-1.5e-5,-3.8e-6,-9.5e-7,9.5e-7,3.8e-6,1.5e-5,6.1e-5,1.2e-4,2.44e-4,0.12]"
-- "Fraction Error (read.show) :: Posit16 : 3 % 65536"
-- "Failure case: [-6.7e7,5.96e-8,6.7e7]"
-- "Fraction Error (read.show) :: P16 : 23 % 65536"
-- "Failure case: [-7.2e16,-4.5e15,-1.1e15,-2.8e14,-1.4e14,-7.0e13,-3.5e13,-2.8e-14,-1.4e-14,-7.1e-15,-2.2e-16,2.2e-16,7.1e-15,1.4e-14,2.8e-14,5.68e-14,3.5e13,7.0e13,1.4e14,2.8e14,1.1e15,4.5e15,7.2e16]"
-- "Fraction Error (read.show) :: Posit32 : 75809653 % 4294967296"
-- "Fraction Error (read.show) :: P32 : 75809653 % 4294967296"
-- Benchmark test-posit-readShowId: FINISH
--
-- After bug fix:
-- Benchmark test-posit-readShowId: RUNNING...
-- "Fraction Error (read.show) :: Posit8 : 0 % 1"
-- "Failure case: []" 
-- "Fraction Error (read.show) :: P8 : 1 % 128"
-- "Failure case: [-0.12,0.12]"
-- "Fraction Error (read.show) :: Posit16 : 0 % 1"
-- "Failure case: []" 
-- "Fraction Error (read.show) :: P16 : 0 % 1"
-- "Failure case: []" 
-- "Fraction Error (read.show) :: Posit32 : 37904819 % 2147483648"
-- "Fraction Error (read.show) :: P32 : 37904819 % 2147483648"
-- Benchmark test-posit-readShowId: FINISH
--
-- After bug fix and incrementing by 1 decimal digit
-- Benchmark test-posit-readShowId: RUNNING...
-- "Fraction Error (read.show) :: Posit8 : 0 % 1"
-- "Failure case: []" 
-- "Fraction Error (read.show) :: P8 : 0 % 1"
-- "Failure case: []" 
-- "Fraction Error (read.show) :: Posit16 : 0 % 1"
-- "Failure case: []" 
-- "Fraction Error (read.show) :: P16 : 0 % 1"
-- "Failure case: []" 
-- "Fraction Error (read.show) :: Posit32 : 0 % 1"
-- "Fraction Error (read.show) :: P32 : 0 % 1"
-- Benchmark test-posit-readShowId: FINISH




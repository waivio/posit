

{-# LANGUAGE TypeApplications #-} --   To apply types: @Type, it seems to select the specific class instance, when GHC is not able to reason about things, commenting this out shows an interesting interface
{-# LANGUAGE ScopedTypeVariables #-} --   To reduce some code duplication, this is important
{-# LANGUAGE FlexibleContexts #-} -- to talk about class constraints like: (PositC es, PositC (Next es)) => 
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}  --   For our ES kind and the constructors Z, I, II, III, IV, V for exponent size type, post-pended with the version.
{-# OPTIONS_GHC -Wno-type-defaults #-}  --   Turn off noise
{-# OPTIONS_GHC -Wno-unused-top-binds #-}  --   Turn off noise


module Test.Algorithms
 ( funLogDomainReduction
 , funLogTaylor
 , funExp2
 , funExpTaylor
 , funLogTuma
 , funExpTuma
 , funGammaSeriesFused
 , funGammaRamanujan
 , funGammaCalc
 , funGammaNemes
 , funGammaYang
 , funGammaChen
 , funGammaXminus1
 -- , funGammaViaLngamma
 , funPi1
 , funPi2
 , funPi3
 , funPi4
 , funPi5
 , funPi6
   ) where

import Posit  -- run with -O_TEST CPP directive

import Prelude hiding (rem)

-- would like to:
-- import Posit.Internal.ElementaryFunctions
-- Perhaps on the chopping block if we are moving to ElementaryFunctions
-- Imports for implementing the Transcendental Functions
import GHC.Natural (Natural) -- Import the Natural Numbers ℕ (u+2115) for some of the Transcendental Functions
import Data.Ratio ((%))  -- Import the Rational Numbers ℚ (u+211A), ℚ can get arbitrarily close to Real numbers ℝ (u+211D), used for some of the Transcendental Functions


import Debug.Trace (trace) -- temporary for debug purposes



-- The machine implementation of the Posit encoding/decoding
import Posit.Internal.PositC  -- The main internal implementation details

 -- Algorithms in Type: `Posit es`


-- ==============================================================
--                        Other functions:
-- ==============================================================



-- Approximation of log2 "Log Base 2"
approx_log2 :: forall es. PositC es => Posit es -> Posit es
approx_log2 NaR = NaR
approx_log2 z
  | z <= 0 = NaR -- includes the NaR case
  | otherwise = go (fromInteger ex) 1 sig  -- domain reduction
    where
      go :: Posit es -> Posit es -> Posit es -> Posit es
      go !acc !mak !sig' -- fixed point iteration, y is [1,2) :: Posit256
        | sig == 1 = acc
        | acc == (acc + mak * 2^^(negate.fst.term $ sig')) = acc  -- stop when fixed point is reached
        | otherwise = go (acc + mak * 2^^(negate.fst.term $ sig')) (mak * 2^^(negate.fst.term $ sig')) (snd.term $ sig')
      term = findSquaring 0  -- returns (m,s') m the number of times to square, and the new significand
      (ex, sig) = (int * fromIntegral (2^(exponentSize @es)) + fromIntegral nat, fromRational rat)
      (_,int,nat,rat) = (posit2TupPosit @es).toRational $ z -- sign should always be positive
      findSquaring m s
        | s >= 2 && s < 4 = (m, s/2)
        | otherwise = findSquaring (m+1) (s^2)



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


{-
-- | 'phi' fixed point recursive algorithm,
approx_phi :: (PositC es) => Posit es -> Posit es
approx_phi  px@(Posit x)
    | x == x' = Posit x
    | otherwise = approx_phi (Posit x')
      where
        (Posit x') = (px^2 + 2*px) / (px^2 + 1)
        -- LiquidHaskell is telling me this is unsafe if px is imaginary
        -- lucky for us Posit256 is not imaginary
-}


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
--  Gauss–Legendre algorithm, Seems only accurate to 2-3 ULP, but really slow
funPi1 :: forall es. (PositC es, PositC (Next es)) => Posit es
funPi1 = go 0 3 1 (recip.sqrt $ 2) (recip 4) 1
  where
    go :: Posit es -> Posit es -> Posit es -> Posit es -> Posit es -> Posit es -> Posit es
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
--  quite unstable and will not converge if sqrt is not accurate, which means log must be accurate
funPi2 :: forall es. (PositC es, PositC (Next es)) => Posit es
funPi2 = recip $ go 0 0 0 0.5 (5 / phi^3)
  where
    go :: Posit es -> Posit es -> Natural -> Posit es -> Posit es -> Posit es
    go !prevA !prevS !n !a !s
      | prevA == a = a
      | prevS == s = a
      | abs (prevA - a) <= 2*eps = a  -- P256 or Posit128, will not reach a fixed point where `prevA == a` it sort of oscelates until divergence occurs, if we test for less than 2*eps it can stop early
      | abs (prevS - s) <= 2*eps = a
      | otherwise =
        let x = 5 / s - 1
            y = (x - 1)^2 + 7
            z = (0.5 * x * (y + sqrt (y^2 - 4 * x^3)))**(1/5)
            a' = s^2 * a - (5^n * ((s^2 - 5)/2 + sqrt (s * (s^2 - 2*s + 5))))
            s' = 25 / ((z + x/z + 1)^2 * s)
        in go a s (n+1) (trace ("ΔA: " ++ show (a' - a)) a') (trace ("ΔS: " ++ show (s' - s)) s')
--



-- Bailey–Borwein–Plouffe (BBP) formula, to 1-2 ULP, and blazing fast, converges in 60 iterations
funPi3 :: forall es. (PositC es) => Posit es
funPi3 = go 0 0
  where
    go :: Integer -> Posit es -> Posit es
    go !k !acc
      | acc == acc + term k = acc
      | otherwise = go (k+1) (acc + term k)
    term :: Integer -> Posit es
    term k = fromRational $ (1 % 16^k) * ((120 * k^2 + 151 * k + 47) % (512 * k^4 + 1024 * k^3 + 712 * k^2 + 194 * k + 15))
--


-- Fabrice Bellard improvement on the BBP, 2-3 ULP, even faster, converges in 25 iterations, really fast
funPi4 :: forall es. (PositC es) => Posit es
funPi4 = (1/2^6) * go 0 0
  where
    go :: Integer -> Posit es -> Posit es
    go !k !acc
      | acc == acc + term k = acc
      | otherwise = go (k+1) (acc + term k)
    term :: Integer -> Posit es
    term k = fromRational $ ((-1)^k % (2^(10*k))) * ((1 % (10 * k + 9)) - (2^2 % (10 * k + 7)) - (2^2 % (10 * k + 5)) - (2^6 % (10 * k + 3)) + (2^8 % (10 * k + 1)) - (1 % (4 * k + 3)) - (2^5 % (4 * k + 1)))
--


-- Borwin's Quadradic Alogrithm 1985
funPi5 :: forall es. (PositC es, PositC (Next es)) => Posit es
funPi5 = recip $ go 0 0 1 (6 - 4 * sqrt 2) (sqrt 2 - 1)
  where
    go :: Posit es -> Posit es -> Natural -> Posit es -> Posit es -> Posit es
    go !prevA !prevY !n a y
      | prevA == a = a
      | prevY == y = a
      | otherwise =
        let f = (1 - y^4)**(1/4)
            y' = (1 - f) / (1 + f)
            a' = a * (1 + y')^4 - 2^(2 * n + 1) * y' * (1 + y' + y'^2) 
        in if n == 3
           then a'
           else go a y (n+1) (trace ("A: " ++ show a') a') (trace ("Y: " ++ show y') y')
--
-- 3.14159265358979323846264338327950288419716939937510582097494459231
-- ULP: -97

-- Borwin's Cubic Algirthm
funPi6 :: forall es. (PositC es, PositC (Next es)) => Posit es
funPi6 = recip $ go 0 0 1 (1/3) ((sqrt 3 - 1) / 2)
  where
    go :: Posit es -> Posit es -> Natural -> Posit es -> Posit es -> Posit es
    go !prevA !prevS !n !a !s
      | prevA == a = a
      | prevS == s = a
      | otherwise =
        let r = 3 / (1 + 2 * (1 - s^3)**(1/3))
            s'= (r - 1) / 2
            a'= r^2 * a - 3^(n-1) * (r^2 - 1)
        in if n == 4
           then a'
           else go a s (n+1) a' s'
-- 3.14159265358979323846264338327950288419716939937510582097494459231
-- ULP: 216
--
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
-- Interestingly enough, wikipedia defines two alternative solutions
-- for the Shannon Wavelet, eventhough there are infinite solutions
-- where the functions are equal, they are not equal.  It a class of 
-- functions with the charicteristic of being a band pass filter in the 
-- frequency space.
-- Shannon wavelet
funPsiSha1 :: Posit256 -> Posit256
funPsiSha1 NaR = NaR
funPsiSha1 t = 2 * sinc (2 * t) - sinc t
--

-- Shannon wavelet
funPsiSha2 :: Posit256 -> Posit256
funPsiSha2 NaR = NaR
funPsiSha2 t = sinc (t/2) * cos (3*pi*t/2)
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


--
-- Using the CORDIC domain reduction and some approximation function
funLogDomainReduction :: (Posit256 -> Posit256) -> Posit256 -> Posit256
funLogDomainReduction _ NaR = NaR
funLogDomainReduction _ 1 = 0
funLogDomainReduction f x
  | x <= 0 = NaR
  | otherwise = f sig + (fromIntegral ex * lnOf2)
    where
      (ex, sig) = (int * fromIntegral (2^(exponentSize @V_3_2)) + fromIntegral nat + 1, fromRational rat / 2) -- move significand range from 1,2 to 0.5,1
      (_,int,nat,rat) = (posit2TupPosit @V_3_2).toRational $ x -- sign should always be positive
--

-- Use the constant, for performance
lnOf2 :: PositC es => Posit es
lnOf2 = 0.6931471805599453094172321214581765680755001343602552541206800094933936219696947156058633269964186875420014810205706857336855202
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
      | acc == (acc + term k) = acc  -- if x == x + dx then terminate and return x
      | otherwise = go (k+1) (acc + term k)
    term :: Natural -> Posit256
    term k = (z^k) / (fromIntegral.fac $ k)
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



funGammaSeriesFused :: forall es. (PositC es, PositC (Next es)) => Posit es -> Posit es
funGammaSeriesFused z = sqrt(2 * pi) * (z**(z - 0.5)) * exp (negate z) * (1 + series)
  where
    series :: Posit es
    series = fsumL $ zipWith (*) [fromRational (a % b) | (a,b) <- zip a001163 a001164] [recip $ z^n |  n <- [1..len]]  -- zipWith (\x y -> ) a001163 a001164
    lenA = length a001163
    lenB = length a001164
    len = if lenA == lenB
            then lenA
            else error "Seiries Numerator and Denominator do not have the same length."
--

--
a001163 :: [Integer] -- Numerator
a001163 = [1, 1, -139, -571, 163879, 5246819, -534703531, -4483131259, 432261921612371, 6232523202521089, -25834629665134204969, -1579029138854919086429, 746590869962651602203151, 1511513601028097903631961, -8849272268392873147705987190261, -142801712490607530608130701097701]
a001164 :: [Integer]  -- Denominator
a001164 = [12, 288, 51840, 2488320, 209018880, 75246796800, 902961561600, 86684309913600, 514904800886784000, 86504006548979712000, 13494625021640835072000, 9716130015581401251840000, 116593560186976815022080000, 2798245444487443560529920000, 299692087104605205332754432000000, 57540880724084199423888850944000000]


--
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
--

--
funGammaRamanujan :: (PositC es, PositC (Next es)) => Posit es -> Posit es
funGammaRamanujan z = sqrt pi * (x / exp 1)**x * (8*x^3 + 4*x^2 + x + (1/30))**(1/6)
  where
    x = z - 1
--


--
funGammaCalc :: (PositC es, PositC (Next es)) => Posit es -> Posit es
funGammaCalc z = sqrt (2*pi / z) * ((z / exp 1) * sqrt (z * sinh (recip z) + recip (810 * z^6)))**z


funGammaNemes :: (PositC es, PositC (Next es)) => Posit es -> Posit es
funGammaNemes z = sqrt (2*pi / z) * (recip (exp 1) * (z + recip (12 * z - recip (10 * z))))**z

funGammaYang :: (PositC es, PositC (Next es)) => Posit es -> Posit es
funGammaYang z = sqrt (2 * pi * x) * (x / exp 1)**x * (x * sinh (recip x))**(x/2) * exp (fromRational (7 % 324) * recip (x^3 * (35 * x^2 + 33)))
  where
    x = z - 1

funGammaChen :: (PositC es, PositC (Next es)) => Posit es -> Posit es
funGammaChen z = sqrt (2 * pi * x) * (x / exp 1)**x * (1 + recip (12*x^3 + (24/7)*x - 0.5))**(x^2 + fromRational (53 % 210))
  where
    x = z - 1

funGammaXminus1 :: (PositC es, PositC (Next es)) => Posit es -> Posit es
funGammaXminus1 x = go (x - 1)
  where
    go z = sqrt (2 * pi) * exp z ** (negate z) * z ** (z + 0.5)

{-
funGammaInfProd :: Posit es -> Posit es
funGammaInfProd


funGammaViaInv :: Posit es -> Posit es
funGammaViaInv
-}
{-
funGammaViaLngamma :: forall es. (PositC es, PositC (Next es)) => Posit es -> Posit es
funGammaViaLngamma z = exp $ lngamma
  where
    lngamma :: Posit es
    lngamma = negate eulersConstant * z - log z + go 0 1
    go :: Posit es -> Integer -> Posit es
    go NaR _ = NaR
    go prev k | prev == prev + next k = prev
              | otherwise = go (trace ("Next: " ++ show (prev + next k)) (prev + next k)) (k+1)
    next :: Integer -> Posit es
    next k = z / fromIntegral k - (log $ 1 + z / fromIntegral k)

eulersConstant :: PositC es => Posit es
eulersConstant = 0.57721566490153286060651209008240243104215933593992
-}

fac :: Natural -> Natural
fac 0 = 1
fac n = n * fac (n - 1)
--











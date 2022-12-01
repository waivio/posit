
--------------------------------------------------------------------------------------------
-- | Posit Numbers
--   Copyright   :  (C) 2022 Nathan Waivio
--   License     :  BSD3
--   Maintainer  :  Nathan Waivio <nathan.waivio@gmail.com>
--   Stability   :  Stable
--   Portability :  Portable
--
--   Test Suite for a Library implementing standard Posit Numbers
-- 
---------------------------------------------------------------------------------------------

import Posit
import Posit.Internal.PositC



main :: IO ()
main = do
--
  print $ "exp(1)**(pi*sqrt 43): " ++ show (exp(1 :: Posit256) ** (pi * sqrt 43)) -- 
  print $ "exp(1)**(pi*sqrt 67): " ++ show (exp(1 :: Posit256) ** (pi * sqrt 67)) -- 
  print $ "exp(1)**(pi*sqrt 163): " ++ show (exp(1 :: Posit256) ** (pi * sqrt 163)) --
  print $ "Machine Alpha Posit8 ~1.0: " ++ show (1.0 - succ (1.0 :: Posit8)) -- succ (Posit int) = Posit (succ int)
  print $ "Machine Alpha Posit16 ~1.0: " ++ show (1.0 - succ (1.0 :: Posit16)) -- 
  print $ "Machine Alpha Posit32 ~1.0: " ++ show (1.0 - succ (1.0 :: Posit32)) -- 
  print $ "Machine Alpha Posit64 ~1.0: " ++ show (1.0 - succ (1.0 :: Posit64)) -- 
  print $ "Machine Alpha Posit128 ~1.0: " ++ show (1.0 - succ (1.0 :: Posit128)) -- 
  print $ "Machine Alpha Posit256 ~1.0: " ++ show (1.0 - succ (1.0 :: Posit256)) -- 
  print $ "Does (1 - 1) == 0 ?: " ++ show ((1 - 1) == (0 :: Posit256)) -- [(1 - 1) == zero | zero = 0 :: Posit es, es <- Z .. V]
  let sqrtTaylor = (funLogDomainReduction funLogTaylor).(/2).(funExp2 funExpTaylor).(/log 2)
  print $ "sqrt phi using a Taylor algorithm: " ++ show (sqrtTaylor phi)
  let sqrtTuma = (funLogDomainReduction funLogTuma).(/2).(funExp2 funExpTuma).(/log 2)
  print $ "sqrt phi using a Tuma algorithm: " ++ show (sqrtTuma phi)
  print $ "Tuma is fasta: " ++ show (sqrtTaylor (1/1000000) - sqrtTuma (1/1000000))
  let truth = 0.8956731517052878608869612167009786079379812529831641161347143256836782657295966290940929214799036260987761959338755143914935872 :: Posit256
  eval "Standard: gamma(phi): " (gamma (phi)) truth
  eval "Fused Gamma: gamma(phi): " (funGammaSeriesFused (phi)) truth
  eval "Ramanujan Gamma: gamma(phi): " (funGammaRamanujan (phi)) truth
  eval "Calc Gamma: gamma(phi): " (funGammaCalc (phi)) truth
  eval "Nemes Gamma: gamma(phi): " (funGammaNemes (phi)) truth
  eval "Yang Gamma: gamma(phi): " (funGammaYang (phi)) truth
  eval "Chen Gamma: gamma(phi): " (funGammaChen (phi)) truth
  eval "Gamma (x - 1): gamma(phi): " (funGammaXminus1 (phi)) truth
  eval "Wolfram alpha: gamma(phi): " truth truth
  let truth = 5.0431656433600286513118821892854247103235901754138463603020001967777869609108929428415187821843384653305404495551887666992776792 :: Posit256
  eval "Standard: exp(phi):" (exp (phi)) truth
  eval "Taylor: exp(phi):" (funExp2 funExpTaylor (phi / log 2)) truth
  eval "Tuma: exp(phi):" (funExp2 funExpTuma (phi / log 2)) truth
  eval "Wolfram Alpha: exp(phi):" truth truth
  let truth = 2.6881171418161354484126255515800135873611118773741922415191608615280287034909564914158871097219845710811670879190576068697e43 :: Posit256
  eval "Standard: exp(100):" (exp (100)) truth
  eval "Taylor: exp(100):" (funExp2 funExpTaylor (100 / log 2)) truth
  eval "Tuma: exp(100):" (funExp2 funExpTuma (100 / log 2))  truth
  eval "Wolfram Alpha: exp(100):" truth truth
  let truth = 3.7200759760208359629596958038631183373588922923767819671206138766632904758958157181571187786422814966019356176423110698002e-44 :: Posit256
  eval "Standard: exp(-100):" (exp (-100)) truth
  eval "Taylor: exp(-100):" (funExp2 funExpTaylor (-100 / log 2)) truth
  eval "Tuma: exp(-100):" (funExp2 funExpTuma (-100 / log 2)) truth
  eval "Wolfram Alpha: exp(-100):" truth truth
  let truth = 1.9700711140170469938888793522433231253169379853238457899528029913850638507824411934749780765630268899309638179875202269359e434 :: Posit256
  eval "Standard: exp(1000):" (exp (1000)) truth
  eval "Taylor: exp(1000):" (funExp2 funExpTaylor (1000 / log 2)) truth
  eval "Tuma: exp(1000):" (funExp2 funExpTuma (1000 / log 2)) truth
  eval "Wolfram Alpha: exp(1000):" truth truth
  let truth = 5.075958897549456765291809479574336919305599282892837361832393845410540542974819175679662169046542867863667106831065285113e-435 :: Posit256
  eval "Standard: exp(-1000):" (exp (-1000)) truth
  eval "Taylor: exp(-1000):" (funExp2 funExpTaylor (-1000 / log 2)) truth
  eval "Tuma: exp(-1000):" (funExp2 funExpTuma (-1000 / log 2)) truth
  eval "Wolfram Alpha: exp(-1000):" truth truth
  let truth = 0.4812118250596034474977589134243684231351843343856605196610181688401638676082217744120094291227234749972318399582936564112725683 :: Posit256
  eval "Standard: log(phi):" (log (phi)) truth
  eval "Taylor: log(phi):" (funLogDomainReduction funLogTaylor (phi)) truth
  eval "Tuma: log(phi):" (funLogDomainReduction funLogTuma (phi)) truth
  eval "Wolfram Alpha: log(phi):" truth truth
  let truth = -4.6051701859880913680359829093687284152022029772575459520666558019351452193547049604719944101791965966839355680845724972668190 :: Posit256
  eval "Standard: log(1/100):" (log (1/100)) truth
  eval "Taylor: log(1/100):" (funLogDomainReduction funLogTaylor (1/100)) truth
  eval "Tuma: log(1/100):" (funLogDomainReduction funLogTuma (1/100)) truth
  eval "Wolfram Alpha: log(1/100):" truth truth
  let truth = -6.9077552789821370520539743640530926228033044658863189280999837029027178290320574407079916152687948950259033521268587459002285 :: Posit256
  eval "Standard: log(1/1000):" (log (1/1000)) truth
  eval "Taylor: log(1/1000):" (funLogDomainReduction funLogTaylor (1/1000)) truth
  eval "Tuma: log(1/1000):" (funLogDomainReduction funLogTuma (1/1000)) truth
  eval "Wolfram Alpha: log(1/1000):" truth truth
  let truth = 4.5347571611551792889915884948567915637887680293971326427244942079650289300980475282698882636812383679690567084677326507550787791 :: Posit256
  eval "Standard: phi^pi:" ((phi) ** pi) truth
  eval "Wolfram Alpha: phi^pi:" truth truth
  let truth = 3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679821480865132823066470938446 :: Posit256
  eval "Standard pi:" pi truth
  eval "Gauss–Legendre algorithm: pi:" funPi1 truth
  eval "Borwein's Quintic algorithm: pi:" funPi2 truth
  eval "Bailey–Borwein–Plouffe (BBP) formula: pi:" funPi3 truth
  eval "Fabrice Bellard improvement on the BBP: pi:" funPi4 truth
  eval "Borwein's Quadradic 1985 formula: pi:" funPi5 truth
  eval "Borwein Cubic: pi:" funPi6 truth
  eval "Wolfram Alpha: pi:" truth truth
  eval "Bailey–Borwein–Plouffe (BBP) formula: but succ pi:" (succ funPi3) truth
--
  -- print $ "Does (1 - 1) == 0 ?: " ++ (1 - 1) == (0 :: Posit256) -- [(1 - 1) == zero | zero = 0 :: Posit es, es <- Z .. V]
  print "Now for Property testing of Posit8... (This should generalize for all other Posit types)"
  print $ "Does associtivity of (+) hold?: " ++ (show assoc8)
  print $ "Does commutitivity of (+) hold?: " ++ (show commutative8)
  print $ "Is `fromInteger 0` the additive identity?: " ++ (show additiveIdent8)
  print $ "Does `negate` give the additive inverse? (excluding NaR): " ++ (show additiveInv8)
  print $ "Does `negate.negate == id`?: " ++ (show nn8)
  print $ "Does associtivity of (*) hold?: " ++ (show assocMult8)
  print $ "Is `fromInteger 1` the multiplicitive identity?: " ++ (show multIdent8)
  print $ "Does Reflexivity of Eq hold?: " ++ (show reflEq8)
  print $ "Does Symmetry of Eq hold?: " ++ (show symEq8)
  print $ "Does Transitivity of Eq hold?: " ++ (show transEq8)
  print $ "Does Extensionality of Eq hold?: " ++ (show extEq8)
  print $ "Does Negation of Eq hold?: " ++ (show negEq8)
  print $ "Does Comparability of Ord hold?: " ++ (show comp8)
  print $ "Does Transitivity of Ord hold?: " ++ (show trans8)
  print $ "Does Reflexivity of Ord hold?: " ++ (show refl8)
  print $ "Does Antisymmetry of Ord hold?: " ++ (show anti8)
  print $ "Does the `abs x * signum x == x` law hold?: " ++ (show absSignumLaw)
  print $ "Is recip a multiplicative inverse?: " ++ (show recipInv8)
  print $ "Are there any `recip.recip == id` values: " ++ (show rr8)
  print $ "Are there any `recip.recip /= id` values: " ++ (show rrne8)
  print $ "Does the distributive property hold with posits all the time?: " ++ (show doesItDistribute)
  print $ "Exaustive Proof... for fused ops recovering the distributeive property... and it turns out to be true."
  print $ "Can fused ops recover the distributive property for `fmms a b (negate a) c == fam b c a` ?: " ++ (show fusedDistribute)



eval :: String -> Posit256 -> Posit256 -> IO ()
eval msg val tru = putStr $ msg ++ "\n" ++ (show val) ++ "\n" ++ "ULP: " ++ (show $ valInt - truInt) ++ "\n"
  where
    valInt = read (displayIntegral val) :: Integer
    truInt = read (displayIntegral tru) :: Integer

-- exaustive testing, enum from to
assoc8 :: Bool
assoc8 = and [(x + y) + z == x + (y + z) | x <- enumFrom (NaR :: Posit8), y <- enumFrom (NaR :: Posit8), z <- enumFrom (NaR :: Posit8)]

commutative8 :: Bool
commutative8 = and [x + y == y + x | x <- enumFrom (NaR :: Posit8), y <- enumFrom (NaR :: Posit8)]

additiveIdent8 :: Bool
additiveIdent8 = and [x + fromInteger 0 == x | x <- enumFrom (NaR :: Posit8)]

additiveInv8 :: Bool
additiveInv8 = and [x + negate x == fromInteger 0 | x <- enumFrom (minBound :: Posit8)]

assocMult8 :: Bool
assocMult8 = and [(x * y) * z == x * (y * z) | x <- enumFrom (minBound :: Posit8), y <- enumFrom (minBound :: Posit8), z <- enumFrom (minBound :: Posit8)]

multIdent8 :: Bool
multIdent8 = and [x * fromInteger 1 == x && fromInteger 1 * x == x | x <- enumFrom (NaR :: Posit8)]

reflEq8 = and [(x == x) | x <- enumFrom (NaR :: Posit8)]

symEq8 = and [y == x | x <- enumFrom (NaR :: Posit8), y <- enumFrom (NaR :: Posit8), (x == y)]

transEq8 = and [x == z | x <- enumFrom (NaR :: Posit8), y <- enumFrom (NaR :: Posit8), z <- enumFrom (NaR :: Posit8), (x == y) && (y == z)]

extEq8 = and [sin x == sin y | x <- enumFrom (NaR :: Posit8), y <- enumFrom (NaR :: Posit8), x == y]

negEq8 = and [not (x == y) | x <- enumFrom (NaR :: Posit8), y <- enumFrom (NaR :: Posit8), x /= y]

comp8 :: Bool
comp8 = and [(x <= y || y <= x) == True | x <- enumFrom (NaR :: Posit8), y <- enumFrom (NaR :: Posit8)]

trans8 :: Bool
trans8 = and [(x <= z) == True | x <- enumFrom (minBound :: Posit8), y <- enumFrom (minBound :: Posit8), z <- enumFrom (minBound :: Posit8), (x <= y && y <= z) == True]

refl8 :: Bool
refl8 = and [(x <= x) == True | x <- enumFrom (NaR :: Posit8)]

anti8 :: Bool
anti8 = and [(x == y) == True | x <- enumFrom (minBound :: Posit8), y <- enumFrom (minBound :: Posit8), (x <= y && y <= x) == True]

nn8 :: Bool
nn8 = and [(negate.negate $ x) == x | x <- enumFrom (NaR :: Posit8)]

-- recip.recip == id 
rr8 :: [Posit8]
rr8 = [x| x <- enumFrom (NaR :: Posit8), (recip.recip $ x) == x]

-- recip.recip /= id
rrne8 :: [Posit8]
rrne8 = [x| x <- enumFrom (NaR :: Posit8), (recip.recip $ x) /= x]

doesItDistribute :: Bool
doesItDistribute = and [a*b + a*c == a*(b+c) | a <- enumFrom (NaR :: Posit8), b <- enumFrom (NaR :: Posit8), c <- enumFrom (NaR :: Posit8)]

fusedDistribute :: Bool
fusedDistribute = and [fmms a b (negate a) c == fam b c a | a <- enumFrom (NaR :: Posit8), b <- enumFrom (NaR :: Posit8), c <- enumFrom (NaR :: Posit8)]

absSignumLaw :: Bool
absSignumLaw = and [abs x * signum x == x | x <- enumFrom (NaR :: Posit8)]

recipInv8 :: Bool
recipInv8 = and [((x * recip x) == fromInteger 1) && ((recip x * x) == fromInteger 1)  | x <- enumFrom (NaR :: Posit8)]


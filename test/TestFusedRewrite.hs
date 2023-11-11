
--------------------------------------------------------------------------------------------
-- | Posit Numbers
--   Copyright   :  (C) 2022-2023 Nathan Waivio
--   License     :  BSD3
--   Maintainer  :  Nathan Waivio <nathan.waivio@gmail.com>
--   Stability   :  Stable
--   Portability :  Portable
--
--   Test Suite for a Library implementing standard Posit Numbers
--   to test the Fused Operations Rewrite Rules.
-- 
---------------------------------------------------------------------------------------------


import Posit

main :: IO () 
main = do
  print $ "== Let's first take a look at fsm, when examined you shall find that (-) doesn't inline early enough to triger fsm =="
  print $ "posit/fsm pi - (exp(1) * goldenRatio) :: Posit256 : " ++ show (pi - ((exp 1 :: Posit256) * goldenRatio))
  print $ "posit/fsmIn pi + negate (exp(1) * goldenRatio) :: Posit256 : " ++ show (pi + (negate (exp 1 :: Posit256) * goldenRatio))
  print $ "posit/fsmInSwaped negate (exp(1) * goldenRatio) + pi :: Posit256 : " ++ show (negate ((exp 1 :: Posit256) * goldenRatio) + pi)
  print $ "posit/fsmDef `fsm pi exp(1) goldenRatio` :: Posit256 : " ++ show (fsm pi (exp 1 :: Posit256) goldenRatio)
  print $ "== Let's now consider the goodness of fma, it all fuses =="
  print $ "posit/fma (pi * exp(1)) + goldenRatio :: Posit256 : " ++ show ((pi * (exp 1 :: Posit256)) + goldenRatio)
  print $ "posit/fmaSwaped goldenRatio + (pi * exp(1)) :: Posit256 : " ++ show (goldenRatio + (pi * (exp 1 :: Posit256)))
  print $ "posit/fmaDef `fma pi exp(1) goldenRatio` :: Posit256 : " ++ show (fma pi (exp 1 :: Posit256) goldenRatio)
  print $ "== Let's now consider the goodness of fam, it all fuses =="
  print $ "posit/fam (pi + (exp 1)) * goldenRatio :: Posit256 : " ++ show ((pi + (exp 1 :: Posit256)) * goldenRatio)
  print $ "posit/famSwaped goldenRatio * (pi + (exp 1)) :: Posit256 : " ++ show (goldenRatio * (pi + (exp 1 :: Posit256)))
  print $ "posit/famDef `fam pi exp(1) goldenRatio` :: Posit256 : " ++ show (fam pi (exp 1 :: Posit256) goldenRatio)
  print $ "== Let's enjoy `fmms` it now fuses! =="
  print $ "posit/fmms (pi * exp(1)) - (goldenRatio * (sqrt 2)) :: Posit256 : " ++ show ((pi * (exp 1 :: Posit256)) - (goldenRatio * (sqrt 2)))
  print $ "posit/fmmsIn (pi * exp(1)) + negate (goldenRatio * (sqrt 2)) :: Posit256 : " ++ show ((pi * (exp 1 :: Posit256)) + negate (goldenRatio * (sqrt 2)))
  print $ "posit/fmmsInSwap (negate (goldenRatio * (sqrt 2)) + (pi * exp(1))) :: Posit256 : " ++ show (negate (goldenRatio * (sqrt 2)) + (pi * (exp 1 :: Posit256)))
  print $ "posit/fmmsDef `fmms pi exp(1) goldenRatio (sqrt 2)` :: Posit256 : " ++ show (fmms pi (exp 1 :: Posit256) goldenRatio (sqrt 2))
  print $ "== Let's see about fused dot products =="
  print $ "posit/fdot3 (pi * exp(1)) + (goldenRatio * (sqrt 2)) + ((sqrt goldenRatio) * (sqrt 5)) :: Posit256 : " ++ show ((pi * (exp 1 :: Posit256)) + (goldenRatio * (sqrt 2)) + ((sqrt goldenRatio) * (sqrt 5)))
  print $ "posit/fdot3Def `fdot3 pi goldenRatio (sqrt goldenRatio) (exp 1) (sqrt 2) (sqrt 5)` :: Posit256 : " ++ show (fdot3 pi goldenRatio (sqrt goldenRatio) (exp 1) (sqrt 2) (sqrt 5) :: Posit256)
  print $ "posit/fdot3 (pi * exp(1)) + (goldenRatio * (sqrt 2)) + ((sqrt goldenRatio) * (sqrt 5)) :: P256 : " ++ show ((pi * (exp 1 :: P256)) + (goldenRatio * (sqrt 2)) + ((sqrt goldenRatio) * (sqrt 5)))
  print $ "posit/fdot3Def `fdot3 pi goldenRatio (sqrt goldenRatio) (exp 1) (sqrt 2) (sqrt 5)` :: P256 : " ++ show (fdot3 pi goldenRatio (sqrt goldenRatio) (exp 1) (sqrt 2) (sqrt 5) :: P256)
  print $ "== Let's see about fused dot products =="
  print $ "posit/fdot4 (pi * exp(1)) + (goldenRatio * (sqrt 2)) + ((sqrt goldenRatio) * (sqrt 5)) + ((recip pi) * (recip goldenRatio)) :: Posit256 : " ++ show ((pi * (exp 1 :: Posit256)) + (goldenRatio * (sqrt 2)) + ((sqrt goldenRatio) * (sqrt 5)) + ((recip pi) * (recip goldenRatio)))
  print $ "posit/fdot4Def `fdot4 pi goldenRatio (sqrt goldenRatio) (recip pi) (exp 1) (sqrt 2) (sqrt 5) (recip goldenRatio)` :: Posit256 : " ++ show (fdot4 pi goldenRatio (sqrt goldenRatio) (recip pi) (exp 1) (sqrt 2) (sqrt 5) (recip goldenRatio) :: Posit256)
  print $ "posit/fdot4 (pi * exp(1)) + (goldenRatio * (sqrt 2)) + ((sqrt goldenRatio) * (sqrt 5)) + ((recip pi) * (recip goldenRatio)) :: P256 : " ++ show ((pi * (exp 1 :: P256)) + (goldenRatio * (sqrt 2)) + ((sqrt goldenRatio) * (sqrt 5)) + ((recip pi) * (recip goldenRatio)))
  print $ "posit/fdot4Def `fdot4 pi goldenRatio (sqrt goldenRatio) (recip pi) (exp 1) (sqrt 2) (sqrt 5) (recip goldenRatio)` :: P256 : " ++ show (fdot4 pi goldenRatio (sqrt goldenRatio) (recip pi) (exp 1) (sqrt 2) (sqrt 5) (recip goldenRatio) :: P256)


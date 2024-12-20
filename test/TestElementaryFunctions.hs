{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.Int (Int16)

import Posit (Posit, P16, Posit16, AltShow(..), pattern NaR)
import Posit.Internal.PositC


import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo


import Control.Concurrent (forkFinally,newEmptyMVar,putMVar,tryTakeMVar,MVar)
import Data.Maybe (catMaybes)

import Data.Time.Clock (getCurrentTime)

main :: IO ()
main = do
  print "Start:"
  print =<< getCurrentTime
  mvar1 <- newEmptyMVar
  mvar2 <- newEmptyMVar
  mvar3 <- newEmptyMVar
  mvar4 <- newEmptyMVar
  mvar5 <- newEmptyMVar
  mvar6 <- newEmptyMVar
  mvar7 <- newEmptyMVar
  mvar8 <- newEmptyMVar
  mvar9 <- newEmptyMVar
  mvar10 <- newEmptyMVar
  mvar11 <- newEmptyMVar
  mvar12 <- newEmptyMVar
  mvar13 <- newEmptyMVar
  forkFinally expPlotP16Posit16 (\_ -> printDone mvar1 "exp")
  forkFinally logPlotP16Posit16 (\_ -> printDone mvar2 "log")
  forkFinally sqrtPlotsP16Posit16 (\_ -> printDone mvar3 "sqrt")
  forkFinally sinPlotsP16Posit16 (\_ -> printDone mvar4 "sin")
  forkFinally cosPlotsP16Posit16 (\_ -> printDone mvar5 "cos")
  forkFinally asinPlotsP16Posit16 (\_ -> printDone mvar6 "asin")
  forkFinally acosPlotsP16Posit16 (\_ -> printDone mvar7 "acos")
  forkFinally atanPlotsP16Posit16 (\_ -> printDone mvar8 "atan")
  forkFinally sinhPlotsP16Posit16 (\_ -> printDone mvar9 "sinh")
  forkFinally coshPlotsP16Posit16 (\_ -> printDone mvar10 "cosh")
  forkFinally asinhPlotsP16Posit16 (\_ -> printDone mvar11 "asinh")
  forkFinally acoshPlotsP16Posit16 (\_ -> printDone mvar12 "acosh")
  forkFinally atanhPlotsP16Posit16 (\_ -> printDone mvar13 "atanh")
  checkToSeeIfDone [mvar1,mvar2,mvar3,mvar4,mvar5,mvar6,mvar7,mvar8,mvar9,mvar10,mvar11,mvar12,mvar13]
--

checkToSeeIfDone :: [MVar ()] -> IO ()
checkToSeeIfDone [] = return ()
checkToSeeIfDone mvars = do
  listMaybeMVars <- mapM filtDone mvars
  checkToSeeIfDone (catMaybes listMaybeMVars)


filtDone :: MVar () -> IO (Maybe (MVar ()))
filtDone mvar = do
  r <- tryTakeMVar mvar
  case r of
    Nothing -> return $ Just mvar
    Just _ -> return $ Nothing

printDone mvar str = do
  t <- getCurrentTime
  putStrLn $ "Completed " ++ str ++ " at: " ++ show t
  putMVar mvar ()


expPlotP16Posit16 = toFile def "./test/Results/Bits Accuracy of exp with P16 and Posit16.png" $ do
    let expP16 = filter (\(_,d) -> not $ nanOrInf d) [(read (displayIntegral p) :: Double, err p (exp p) exp) | p <- enumFrom (NaR :: P16)]
        expPosit16 = filter (\(_,d) -> not $ nanOrInf d) [(read (displayIntegral p) :: Double, err p (exp p) exp) | p <- enumFrom (NaR :: Posit16)]
    layout_title .= "Number of Accurate Bits of  P16 and Posit16 'exp'"
    plot (line "exp P16 error" [expP16])
    plot (line "exp Posit16 error" [expPosit16])

logPlotP16Posit16 = toFile def "./test/Results/Bits Accuracy of log with P16 and Posit16.png" $ do
    let lnP16 = filter (\(_,d) -> not $ nanOrInf d) [(read (displayIntegral p) :: Double, err p (log p) log) | p <- enumFrom (0 :: P16)]
        lnPosit16 = filter (\(_,d) -> not $ nanOrInf d) [(read (displayIntegral p) :: Double, err p (log p) log) | p <- enumFrom (0 :: Posit16)]
    layout_title .= "Number of Accurate Bits of  P16 and Posit16 'log'"
    plot (line "log P16 error" [lnP16])
    plot (line "log Posit16 error" [lnPosit16])

sqrtPlotsP16Posit16 = toFile def "./test/Results/Bits Accuracy of sqrt with P16 and Posit16.png" $ do
    let sqrtP16 = filter (\(_,d) -> not $ nanOrInf d) [(read (displayIntegral p) :: Double, err p (sqrt p) sqrt) | p <- enumFrom (0 :: P16)]
        sqrtPosit16 = filter (\(_,d) -> not $ nanOrInf d) [(read (displayIntegral p) :: Double, err p (sqrt p) sqrt) | p <- enumFrom (0 :: Posit16)]
    layout_title .= "Number of Accurate Bits of P16 and Posit16 'sqrt'"
    plot (line "sqrt P16 error" [sqrtP16])
    plot (line "sqrt Posit16 error" [sqrtPosit16])

sinPlotsP16Posit16 = toFile def "./test/Results/Bits Accuracy of sin with P16 and Posit16.png" $ do
    let sineP16 = filter (\(_,d) -> not $ nanOrInf d) [(read (displayIntegral p) :: Double, err p (sin p) sin) | p <- enumFrom (NaR :: P16)]
        sinePosit16 = filter (\(_,d) -> not $ nanOrInf d) [(read (displayIntegral p) :: Double, err p (sin p) sin) | p <- enumFrom (NaR :: Posit16)]
    layout_title .= "Number of Accurate Bits of P16 and Posit16 'sin'"
    plot (line "sin P16 error" [sineP16])
    plot (line "sin Posit16 error" [sinePosit16])

cosPlotsP16Posit16 = toFile def "./test/Results/Bits Accuracy of cos with P16 and Posit16.png" $ do
    let cosineP16 = filter (\(_,d) -> not $ nanOrInf d) [(read (displayIntegral p) :: Double, err p (cos p) cos) | p <- enumFrom (NaR :: P16)]
        cosinePosit16 = filter (\(_,d) -> not $ nanOrInf d) [(read (displayIntegral p) :: Double, err p (cos p) cos) | p <- enumFrom (NaR :: Posit16)]
    layout_title .= "Number of Accurate Bits of P16 and Posit16 'cos'"
    plot (line "cos P16 error" [cosineP16])
    plot (line "cos Posit16 error" [cosinePosit16])

asinPlotsP16Posit16 = toFile def "./test/Results/Bits Accuracy of asin with P16 and Posit16.png" $ do
    let arcsineP16 = filter (\(_,d) -> not $ nanOrInf d) [(read (displayIntegral p) :: Double, err p (asin p) sin) | p <- enumFromTo (-1 :: P16) 1]
        arcsinePosit16 = filter (\(_,d) -> not $ nanOrInf d) [(read (displayIntegral p) :: Double, err p (asin p) sin) | p <- enumFromTo (-1 :: Posit16) 1]
    layout_title .= "Number of Accurate Bits of P16 and Posit16 'asin'"
    plot (line "asin P16 error" [arcsineP16])
    plot (line "asin Posit16 error" [arcsinePosit16])

acosPlotsP16Posit16 = toFile def "./test/Results/Bits Accuracy of acos with P16 and Posit16.png" $ do
    let arccosineP16 = filter (\(_,d) -> not $ nanOrInf d) [(read (displayIntegral p) :: Double, err p (acos p) acos) | p <- enumFromTo (-1 :: P16) 1]
        arccosinePosit16 = filter (\(_,d) -> not $ nanOrInf d) [(read (displayIntegral p) :: Double, err p (acos p) acos) | p <- enumFromTo (-1 :: Posit16) 1]
    layout_title .= "Number of Accurate Bits of P16 and Posit16 'acos'"
    plot (line "acos P16 error" [arccosineP16])
    plot (line "acos Posit16 error" [arccosinePosit16])

atanPlotsP16Posit16 = toFile def "./test/Results/Bits Accuracy of atan with P16 and Posit16.png" $ do
    let arctangentP16 = filter (\(_,d) -> not $ nanOrInf d) [(read (displayIntegral p) :: Double, err p (atan p) atan) | p <- enumFrom (NaR :: P16)]
        arctangentPosit16 = filter (\(_,d) -> not $ nanOrInf d) [(read (displayIntegral p) :: Double, err p (atan p) atan) | p <- enumFrom (NaR :: Posit16)]
    layout_title .= "Number of Accurate Bits of P16 and Posit16 'atan'"
    plot (line "atan P16 error" [arctangentP16])
    plot (line "atan Posit16 error" [arctangentPosit16])

sinhPlotsP16Posit16 = toFile def "./test/Results/Bits Accuracy of sinh with P16 and Posit16.png" $ do
    let hypsineP16 = filter (\(_,d) -> not $ nanOrInf d) [(read (displayIntegral p) :: Double, err p (sinh p) sinh) | p <- enumFrom (NaR :: P16)]
        hypsinePosit16 = filter (\(_,d) -> not $ nanOrInf d) [(read (displayIntegral p) :: Double, err p (sinh p) sinh) | p <- enumFrom (NaR :: Posit16)]
    layout_title .= "Number of Accurate Bits of P16 and Posit16 'sinh'"
    plot (line "sinh P16 error" [hypsineP16])
    plot (line "sinh Posit16 error" [hypsinePosit16])

coshPlotsP16Posit16 = toFile def "./test/Results/Bits Accuracy of cosh with P16 and Posit16.png" $ do
    let hypcosineP16 = filter (\(_,d) -> not $ nanOrInf d) [(read (displayIntegral p) :: Double, err p (cosh p) cosh) | p <- enumFrom (NaR :: P16)]
        hypcosinePosit16 = filter (\(_,d) -> not $ nanOrInf d) [(read (displayIntegral p) :: Double, err p (cosh p) cosh) | p <- enumFrom (NaR :: Posit16)]
    layout_title .= "Number of Accurate Bits of P16 and Posit16 'cosh'"
    plot (line "cosh P16 error" [hypcosineP16])
    plot (line "cosh Posit16 error" [hypcosinePosit16])

asinhPlotsP16Posit16 = toFile def "./test/Results/Bits Accuracy of asinh with P16 and Posit16.png" $ do
    let archypsineP16 = filter (\(_,d) -> not $ nanOrInf d) [(read (displayIntegral p) :: Double, err p (asinh p) asinh) | p <- enumFrom (NaR :: P16)]
        archypsinePosit16 = filter (\(_,d) -> not $ nanOrInf d) [(read (displayIntegral p) :: Double, err p (asinh p) asinh) | p <- enumFrom (NaR :: Posit16)]
    layout_title .= "Number of Accurate Bits of P16 and Posit16 'asinh'"
    plot (line "asinh P16 error" [archypsineP16])
    plot (line "asinh Posit16 error" [archypsinePosit16])

acoshPlotsP16Posit16 = toFile def "./test/Results/Bits Accuracy of acosh with P16 and Posit16.png" $ do
    let archypcosineP16 = filter (\(_,d) -> not $ nanOrInf d) [(read (displayIntegral p) :: Double, err p (acosh p) acosh) | p <- enumFrom (NaR :: P16)]
        archypcosinePosit16 = filter (\(_,d) -> not $ nanOrInf d) [(read (displayIntegral p) :: Double, err p (acosh p) acosh) | p <- enumFrom (NaR :: Posit16)]
    layout_title .= "Number of Accurate Bits of P16 and Posit16 'acosh'"
    plot (line "acosh P16 error" [archypcosineP16])
    plot (line "acosh Posit16 error" [archypcosinePosit16])

atanhPlotsP16Posit16 = toFile def "./test/Results/Bits Accuracy of atanh with P16 and Posit16.png" $ do
    let archyptangentP16 = filter (\(_,d) -> not $ nanOrInf d) [(read (displayIntegral p) :: Double, err p (atanh p) atanh) | p <- enumFrom (NaR :: P16)]
        archyptangentPosit16 = filter (\(_,d) -> not $ nanOrInf d) [(read (displayIntegral p) :: Double, err p (atanh p) atanh) | p <- enumFrom (NaR :: Posit16)]
    layout_title .= "Number of Accurate Bits of P16 and Posit16 'atanh'"
    plot (line "atanh P16 error" [archyptangentP16])
    plot (line "atanh Posit16 error" [archyptangentPosit16])



err :: PositC es => Posit es -> Posit es -> (Double -> Double) -> Double
err NaR _ _ = 0 / 0
err _ NaR _ = 0 / 0
err p fp f = 
  let pDouble :: Double = fromRational.toRational $ p
      fpDouble :: Double = fromRational.toRational $ fp
      nBitsError = (negate.log.abs $ f pDouble - fpDouble) / log 2
  in max nBitsError 0

nanOrInf :: Double -> Bool
nanOrInf d = isNaN d || isInfinite d





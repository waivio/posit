
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

import Weigh
import Data.Vector.Storable as V

import Posit
import Posit.Internal.PositC

main :: IO ()
main = mainWith $ do
  func' "Posit8 in 1M Vector" vecOf (1.0 :: Posit8)
  func' "Posit16 in 1M Vector" vecOf (1.0 :: Posit16)
  func' "Posit32 in 1M Vector" vecOf (1.0 :: Posit32)
  func' "Posit64 in 1M Vector" vecOf (1.0 :: Posit64)
  func' "Posit128 in 1M Vector" vecOf (1.0 :: Posit128)
  func' "Posit256 in 1M Vector" vecOf (1.0 :: Posit256)
  func' "P8 in 1M Vector" vecOf (1.0 :: P8)
  func' "P16 in 1M Vector" vecOf (1.0 :: P16)
  func' "P32 in 1M Vector" vecOf (1.0 :: P32)
  func' "P64 in 1M Vector" vecOf (1.0 :: P64)
  func' "P128 in 1M Vector" vecOf (1.0 :: P128)
  func' "P256 in 1M Vector" vecOf (1.0 :: P256)


vecOf :: PositC es => Posit es -> V.Vector (Posit es)
vecOf x = V.replicate (1024*1024) x



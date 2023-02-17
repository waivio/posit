import Weigh
import Data.Vector.Storable as V

import Posit
import Posit.Internal.PositC

main :: IO ()
main = mainWith $ do
  func' "Posit8 in 1M Vector" vecOf unitPosit8
  func' "Posit16 in 1M Vector" vecOf unitPosit16
  func' "Posit32 in 1M Vector" vecOf unitPosit32
  func' "Posit64 in 1M Vector" vecOf unitPosit64
  func' "Posit128 in 1M Vector" vecOf unitPosit128
  func' "Posit256 in 1M Vector" vecOf unitPosit256


vecOf :: PositC es => Posit es -> V.Vector (Posit es)
vecOf x = V.replicate (1024*1024) x

unitPosit8 :: Posit8
unitPosit8 = 1

unitPosit16 :: Posit16
unitPosit16 = 1

unitPosit32 :: Posit32
unitPosit32 = 1

unitPosit64 :: Posit64
unitPosit64 = 1

unitPosit128 :: Posit128
unitPosit128 = 1

unitPosit256 :: Posit256
unitPosit256 = 1





import qualified Flat16 as F16
import qualified Flat32 as F32

import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit


main = do
    print $ ((foldl (F16.snoc) F16.empty) $ replicate 100 0) F16.! 200





import qualified Data.TrieVector as TV
import qualified Data.TrieVector.Unboxed as TUV

import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S


tvn :: Int -> ()
tvn n = seq (TV.fromList [1..n]) ()

tuvn :: Int -> ()
tuvn n = seq (TUV.fromList [1..n]) ()

vn :: Int -> ()
vn n = seq (V.fromList [1..n]) ()

uvn :: Int -> ()
uvn n = seq (UV.fromList [1..n]) ()

hn :: Int -> ()
hn n = seq (HM.fromList $ zip [0..n - 1] [0..n - 1]) ()

sn :: Int -> ()
sn n = seq (S.fromList [0..n]) ()

main = do
  print $ sn 1000000









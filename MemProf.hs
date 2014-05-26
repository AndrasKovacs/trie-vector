{-# LANGUAGE BangPatterns #-}

import qualified Unboxed as UV
import qualified Vector as V
import qualified Data.Sequence as Seq
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M 
import qualified Data.Vector.Unboxed as Vector

import Data.Ix
import Control.Monad
import Data.List
import Control.DeepSeq

-- Note : immutable vec takes less memory to construct than Data.Vector.FromList!

main = do
    let !x = V.fromList [0..10000000::Int]
    print $ ()  
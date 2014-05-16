{-# LANGUAGE BangPatterns #-}

import Unboxed(Vector)
import qualified Vector as V
import qualified Data.Sequence as Seq
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M 

import Data.List

main = do
    --let v = foldl' V.snoc V.empty [0..10000000 :: Int]
    --print $ V.length v
    let !s = IM.fromList $ zip [0..10000000::Int] [0..10000000::Int]
    print $ ()
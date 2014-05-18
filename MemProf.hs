{-# LANGUAGE BangPatterns #-}

import qualified Unboxed as UV
import qualified Vector as V
import qualified Data.Sequence as Seq
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M 

import Data.Ix
import Control.Monad
import Data.List

main = do
    let key = [0..10000000::Int]
    let !x = foldl' (UV.snoc) UV.empty key
    print $ ()
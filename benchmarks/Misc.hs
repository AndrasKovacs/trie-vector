{-# LANGUAGE MagicHash, BangPatterns #-}

{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-binds #-}

import GHC.Types
import GHC.Prim

import qualified Data.TrieVector as TV
import qualified Data.TrieVector.Unboxed as TUV

import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S

import qualified Data.TrieVector.Array as A

import Debug.Trace

-- tvnSafe :: Int -> ()
-- tvnSafe n = seq (TV.safeFromList [1..n]) ()

-- tuvnSafe :: Int -> ()
-- tuvnSafe n = seq (TUV.safeFromList [1..n]) ()

tvn :: Int -> ()
tvn n = seq (TV.fromList [1..n]) ()

tvn' :: Int -> ()
tvn' n = seq (TV.snocFromList [1..n]) ()

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
  print $ tvn' 100000

-- main = do
  -- print (TV.snocFromList [0..10])

  -- -- print $ A.toList (A.fromList 5# (error "fl") [0, 1, 2, 3, 4])
  -- print $ toList' 1# (A.snoc 8# 16# 1# (error "snoc") (A.fromList 0# (error "fl") []) (0::Int))




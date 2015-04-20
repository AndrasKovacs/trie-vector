 {-# LANGUAGE BangPatterns, MagicHash, ScopedTypeVariables #-}


import Criterion.Main
import Criterion.Types
import System.Random
import qualified Data.TrieVector as TV
import qualified Data.TrieVector.Unboxed as TUV


import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as S
import qualified Data.Map.Strict as M

import GHC.Prim
import GHC.Types

import Data.List

randix :: Int -> [Int]
randix size = take 100 $ randomRs (0, size - 1) (mkStdGen 1)

r10    = randix 10
r100   = randix 100
r1000  = randix 1000
r10000 = randix 10000
r100k  = randix 100000
r1M    = randix 1000000
r10M   = randix 10000000

tvn :: Int -> TV.Vector Int
tvn n = TV.fromList [1..n]

tuvn :: Int -> TUV.Vector Int
tuvn n = TUV.fromList [1..n]

vn :: Int -> V.Vector Int
vn n = V.fromList [1..n]

hn :: Int -> HM.HashMap Int Int
hn n = HM.fromList $ zip [0..n - 1] [0..n - 1]

sn :: Int -> S.Seq Int
sn n = S.fromList [1..n]


h10    = hn 10
h100   = hn 100
h1000  = hn 1000
h10000 = hn 10000
h100k  = hn 100000
h1M    = hn 1000000
h10M   = hn 10000000

s10    = sn 10
s100   = sn 100
s10000 = sn 10000
s1M    = sn 1000000

v10    = vn 10
v100   = vn 100
v10000 = vn 10000
v1M    = vn 1000000

tv10    = tvn 10
tv100   = tvn 100
tv1000  = tvn 1000
tv10000 = tvn 10000
tv100k  = tvn 100000
tv1M    = tvn 1000000
tv10M   = tvn 10000000

tuv10    = tuvn 10
tuv100   = tuvn 100
tuv1000  = tuvn 1000
tuv10000 = tuvn 10000
tuv100k  = tuvn 100000
tuv1M    = tuvn 1000000
tuv10M   = tuvn 10000000


benchIx :: (Int -> a) -> [Int] -> ()
benchIx f = foldr (\x acc -> seq (f x) acc) ()
{-# INLINE benchIx #-}
 

config :: Config
config = defaultConfig {timeLimit = 3}

main :: IO ()
main = defaultMainWith config [
  bgroup "Boxed" [

     -- bgroup "index" [
     --    bench "10"    $ whnf (benchIx ((TV.!) tv10   )) r10,
     --    bench "100"   $ whnf (benchIx ((TV.!) tv100  )) r100,
     --    bench "1000"  $ whnf (benchIx ((TV.!) tv1000 )) r100,
     --    bench "10000" $ whnf (benchIx ((TV.!) tv10000)) r10000,
     --    bench "100k"  $ whnf (benchIx ((TV.!) tv100k )) r100k,
     --    bench "1M"    $ whnf (benchIx ((TV.!) tv1M   )) r1M,
     --    bench "10M"   $ whnf (benchIx ((TV.!) tv10M  )) r10M
     --    ]
     

     -- bgroup "unsafeIndex" [
     --    bench "10"    $ whnf (benchIx (TV.unsafeIndex tv10   )) r10,
     --    bench "100"   $ whnf (benchIx (TV.unsafeIndex tv100  )) r100,
     --    bench "1000"  $ whnf (benchIx (TV.unsafeIndex tv1000 )) r100,
     --    bench "10000" $ whnf (benchIx (TV.unsafeIndex tv10000)) r10000,
     --    bench "100k"  $ whnf (benchIx (TV.unsafeIndex tv100k )) r100k,
     --    bench "1M"    $ whnf (benchIx (TV.unsafeIndex tv1M   )) r1M,
     --    bench "10M"   $ whnf (benchIx (TV.unsafeIndex tv10M  )) r10M
     --    ]
     
     
     -- bgroup "unsafeIndex" [
     --    bench "10"    $ whnf (benchIx (TV.unsafeIndex tv10   )) r10,
     --    bench "100"   $ whnf (benchIx (TV.unsafeIndex tv100  )) r100,
     --    bench "10000" $ whnf (benchIx (TV.unsafeIndex tv10000)) r10000,
     --    bench "1M"    $ whnf (benchIx (TV.unsafeIndex tv1M   )) r1M
     --    ]
     
     -- bgroup "modify" [        
     --    bench "10"    $ whnf (foldl' (\s k -> TV.modify s k (const 0)) tv10   ) r10,
     --    bench "100"   $ whnf (foldl' (\s k -> TV.modify s k (const 0)) tv100  ) r100,
     --    bench "1000"  $ whnf (foldl' (\s k -> TV.modify s k (const 0)) tv1000 ) r1000,
     --    bench "10000" $ whnf (foldl' (\s k -> TV.modify s k (const 0)) tv10000) r10000,
     --    bench "100k"  $ whnf (foldl' (\s k -> TV.modify s k (const 0)) tv100k ) r100k,
     --    bench "1M"    $ whnf (foldl' (\s k -> TV.modify s k (const 0)) tv1M   ) r1M,
     --    bench "10M"   $ whnf (foldl' (\s k -> TV.modify s k (const 0)) tv10M  ) r10M
     --    ],

     -- bgroup "fromList" [        
     --    bench "10"    $ whnf TV.fromList r10,
     --    bench "100"   $ whnf TV.fromList r100,
     --    bench "1000"  $ whnf TV.fromList r1000,
     --    bench "10000" $ whnf TV.fromList r10000,
     --    bench "100k"  $ whnf TV.fromList r100k,
     --    bench "1M"    $ whnf TV.fromList r1M,
     --    bench "10M"   $ whnf TV.fromList r10M
     --    ],

     -- bgroup "safeFromList" [        
     --    bench "10"    $ whnf TV.safeFromList r10,
     --    bench "100"   $ whnf TV.safeFromList r100,
     --    bench "1000"  $ whnf TV.safeFromList r1000,
     --    bench "10000" $ whnf TV.safeFromList r10000,
     --    bench "100k"  $ whnf TV.safeFromList r100k,
     --    bench "1M"    $ whnf TV.safeFromList r1M,
     --    bench "10M"   $ whnf TV.safeFromList r10M
     --    ]               

     ],

  bgroup "BoxedVector" [
     -- bgroup "unsafeIndex" [
     --    bench "10"    $ whnf (benchIx (V.unsafeIndex v10   )) r10,
     --    bench "100"   $ whnf (benchIx (V.unsafeIndex v100  )) r100,
     --    bench "10000" $ whnf (benchIx (V.unsafeIndex v10000)) r10000,
     --    bench "1M"    $ whnf (benchIx (V.unsafeIndex v1M   )) r1M
     --    ],

     -- bgroup "index" [
     --    bench "10"    $ whnf (benchIx ((V.!) v10   )) r10,
     --    bench "100"   $ whnf (benchIx ((V.!) v100  )) r100,
     --    bench "10000" $ whnf (benchIx ((V.!) v10000)) r10000,
     --    bench "1M"    $ whnf (benchIx ((V.!) v1M   )) r1M
     --    ]
     ],

  bgroup "Unboxed" [

     -- bgroup "index" [
     --    bench "10"    $ whnf (benchIx ((TUV.!) tuv10   )) r10,
     --    bench "100"   $ whnf (benchIx ((TUV.!) tuv100  )) r100,
     --    bench "1000"  $ whnf (benchIx ((TUV.!) tuv1000 )) r100,
     --    bench "10000" $ whnf (benchIx ((TUV.!) tuv10000)) r10000,
     --    bench "100k"  $ whnf (benchIx ((TUV.!) tuv100k )) r100k,
     --    bench "1M"    $ whnf (benchIx ((TUV.!) tuv1M   )) r1M,
     --    bench "10M"   $ whnf (benchIx ((TUV.!) tuv10M  )) r10M
     --    ],

     bgroup "unsafeIndex" [
        bench "10"    $ whnf (benchIx (TUV.unsafeIndex tuv10   )) r10,
        bench "100"   $ whnf (benchIx (TUV.unsafeIndex tuv100  )) r100,
        bench "1000"  $ whnf (benchIx (TUV.unsafeIndex tuv1000 )) r100,
        bench "10000" $ whnf (benchIx (TUV.unsafeIndex tuv10000)) r10000,
        bench "100k"  $ whnf (benchIx (TUV.unsafeIndex tuv100k )) r100k,
        bench "1M"    $ whnf (benchIx (TUV.unsafeIndex tuv1M   )) r1M,
        bench "10M"   $ whnf (benchIx (TUV.unsafeIndex tuv10M  )) r10M
        ]         
    
     
     -- bgroup "unsafeModify" [
     --    bench "10"    $ whnf (benchIx (flip (TUV.unsafeModify tuv10   ) (const 0) )) r10,
     --    bench "100"   $ whnf (benchIx (flip (TUV.unsafeModify tuv100  ) (const 0) )) r100,
     --    bench "10000" $ whnf (benchIx (flip (TUV.unsafeModify tuv10000) (const 0) )) r10000,
     --    bench "1M"    $ whnf (benchIx (flip (TUV.unsafeModify tuv1M   ) (const 0) )) r1M
     --    ]      
     
     ],

   bgroup "HashMap" [
    
     -- bgroup "lookup" [
     --    bench "10"    $ whnf (benchIx (flip HM.lookup h10   )) r10,
     --    bench "100"   $ whnf (benchIx (flip HM.lookup h100  )) r100,
     --    bench "1000"  $ whnf (benchIx (flip HM.lookup h1000 )) r100,
     --    bench "10000" $ whnf (benchIx (flip HM.lookup h10000)) r10000,
     --    bench "100k"  $ whnf (benchIx (flip HM.lookup h100k )) r100k,
     --    bench "1M"    $ whnf (benchIx (flip HM.lookup h1M   )) r1M,
     --    bench "10M"   $ whnf (benchIx (flip HM.lookup h10M  )) r10M
     --    ]

  --    bgroup "update" [
  --       bench "10"    $ whnf (benchIx (\k -> HM.insert k (k - 1) h10   )) r10,
  --       bench "100"   $ whnf (benchIx (\k -> HM.insert k (k - 1) h100  )) r100,
  --       bench "10000" $ whnf (benchIx (\k -> HM.insert k (k - 1) h10000)) r10000,
  --       bench "1M"    $ whnf (benchIx (\k -> HM.insert k (k - 1) h1M   )) r1M
  --       ]

     -- bgroup "modify" [        
     --    bench "10"    $ whnf (foldl' (\s k -> HM.adjust (const 0) k s) h10   ) r10,
     --    bench "100"   $ whnf (foldl' (\s k -> HM.adjust (const 0) k s) h100  ) r100,
     --    bench "10000" $ whnf (foldl' (\s k -> HM.adjust (const 0) k s) h10000) r10000,
     --    bench "1M"    $ whnf (foldl' (\s k -> HM.adjust (const 0) k s) h1M   ) r1M
     --    ]  
    ],

  bgroup "Seq" [
    
     -- bgroup "lookup" [
     --    bench "10"    $ whnf (benchIx (S.index s10   )) r10,
     --    bench "100"   $ whnf (benchIx (S.index s100  )) r100,
     --    bench "10000" $ whnf (benchIx (S.index s10000)) r10000,
     --    bench "1M"    $ whnf (benchIx (S.index s1M   )) r1M
     --    ]

     -- bgroup "update" [
     --    bench "10"    $ whnf (foldl' (\s k -> S.update k k s) s10   ) r10,
     --    bench "100"   $ whnf (foldl' (\s k -> S.update k k s) s100  ) r100,
     --    bench "10000" $ whnf (foldl' (\s k -> S.update k k s) s10000) r10000,
     --    bench "1M"    $ whnf (foldl' (\s k -> S.update k k s) s1M   ) r1M
     --    ]
     ],   

    bench "nop_ix" $ whnf (benchIx id) r100
  ]
































-- import GHC.Prim
-- import GHC.Types
-- import Data.Word

-- import qualified Vector as Vec
-- import qualified Unboxed as UVec

-- import Criterion.Main
-- import Criterion.Config

-- import qualified Data.Vector as V
-- import qualified Data.Vector.Mutable as MV
-- import qualified Data.Sequence as Seq
-- import qualified Data.HashMap.Strict as HM
-- import qualified Data.IntMap.Strict as IM
-- import qualified Data.Map.Strict as M 
-- import qualified Deque as Deque 

-- import System.Random
-- import Control.Monad
-- import Control.Applicative
-- import Data.Hashable
-- import Data.List
-- import Control.DeepSeq
-- import Control.Monad.ST.Strict

--config = defaultConfig {cfgSamples = ljust 8}

{-- RAM usage, fromList with 10 million elements
    map     : 1384 mb
    hashmap : 1696 mb 
    intmap  : 1224 mb 
    seq     : 605  mb 
    vec     : 275  mb
    vecubox : 127  mb
--}


{-- 
    relative runtime (low = 1k, high = 10m, in size)

    modify
        intmap   : 2     - 3
        hashmap  : 1,7   - 2
        sequence : 2     - 3
        map      : 3     - 6
        mvector  : 1/18  - 1/11,5

    read
        vector   : 0,7   - 0,45
        intmap   : 4     - 20
        hashmap  : 2,5   - 7,3
        sequence : 14,5  - 25
        map      : 6,5   - 27

    snoc
        sequence : 0,95  - 0,82

--}


{-- Deque vs Vector 
    read same
    modify ~5% slower
    snoc 15-20% slower
        snoc much faster if we box the two vectors (so currently they're boxed)
--}

{-- Trying to optimize tail snoc doesn't work; inline copy is just better #-}


-- main = do
--     let 
--         r1k    = force $ rs 1000
--         r10k   = force $ rs 10000
--         r100k  = force $ rs 100000
--         r1m    = force $ rs 1000000
--         r10m   = force $ rs 10000000

        --au_1k   = aun 1000
        --au_10k  = aun 10000
        --au_100k = aun 100000
        --au_1m   = aun 1000000
        --au_10m  = aun 10000000

        --a16_1k   = a16n 1000
        --a16_10k  = a16n 10000
        --a16_100k = a16n 100000
        --a16_1m   = a16n 1000000
        --a16_10m  = a16n 10000000

        --v1k   = V.fromList [0..1000 ::Int]
        --v10k  = V.fromList [0..10000 ::Int]
        --v100k = V.fromList [0..100000 ::Int]
        --v1m   = V.fromList [0..1000000 ::Int]
        --v10m  = V.fromList [0..10000000 ::Int]

        --seq1k   = Seq.fromList [0..1000 :: Int]
        --seq10k  = Seq.fromList [0..10000 ::Int]
        --seq100k = Seq.fromList [0..100000 ::Int]
        --seq1m   = Seq.fromList [0..1000000 ::Int]
        --seq10m  = Seq.fromList [0..10000000 ::Int]

        -- !deq1k   = Deque.fromList [0..1000 :: Int]
        -- !deq10k  = Deque.fromList [0..10000 ::Int]
        -- !deq100k = Deque.fromList [0..100000 ::Int]
        -- !deq1m   = Deque.fromList [0..1000000 ::Int]
        -- !deq10m  = Deque.fromList [0..10000000 ::Int]

        --im1k    = IM.fromList $ zip [0..1000     :: Int] [0..1000     :: Int]
        --im10k   = IM.fromList $ zip [0..10000    :: Int] [0..10000    :: Int]
        --im100k  = IM.fromList $ zip [0..100000   :: Int] [0..100000   :: Int]
        --im1m    = IM.fromList $ zip [0..1000000  :: Int] [0..1000000  :: Int]
        --im10m   = IM.fromList $ zip [0..10000000 :: Int] [0..10000000 :: Int]

        --m1k    = M.fromList $ zip [0..1000     :: Int] [0..1000     :: Int]
        --m10k   = M.fromList $ zip [0..10000    :: Int] [0..10000    :: Int]
        --m100k  = M.fromList $ zip [0..100000   :: Int] [0..100000   :: Int]
        --m1m    = M.fromList $ zip [0..1000000  :: Int] [0..1000000  :: Int]
        --m10m   = M.fromList $ zip [0..10000000 :: Int] [0..10000000 :: Int]

        --hm1k    = HM.fromList $ zip [0..1000     :: Int] [0..1000     :: Int]
        --hm10k   = HM.fromList $ zip [0..10000    :: Int] [0..10000    :: Int]
        --hm100k  = HM.fromList $ zip [0..100000   :: Int] [0..100000   :: Int]
        --hm1m    = HM.fromList $ zip [0..1000000  :: Int] [0..1000000  :: Int]
        --hm10m   = HM.fromList $ zip [0..10000000 :: Int] [0..10000000 :: Int]

        --list1k   = [0..1000 :: Int]
        --list10k  = [0..10000 ::Int]
        --list100k = [0..100000 ::Int]
        --list1m   = [0..1000000 ::Int]
        --list10m  = [0..10000000 ::Int]


    -- defaultMainWith config (return ()) [


        --bench "snoc_a16_1k "  $ whnf (a16snoc a16_1k   ) r1k  ,
        --bench "snoc_a16_10k " $ whnf (a16snoc a16_10k  ) r10k ,
        --bench "snoc_a16_100k" $ whnf (a16snoc a16_100k ) r100k,
        --bench "snoc_a16_1m  " $ whnf (a16snoc a16_1m   ) r1m  ,
        --bench "snoc_a16_10m " $ whnf (a16snoc a16_10m  ) r10m 

        --bench "snoc_a16_1k "  $ whnf (a16snoc a16_1k   ) r1k  ,
        --bench "snoc_a16_10k " $ whnf (a16snoc a16_10k  ) r10k ,
        --bench "snoc_a16_100k" $ whnf (a16snoc a16_100k ) r100k,
        --bench "snoc_a16_1m  " $ whnf (a16snoc a16_1m   ) r1m  ,
        --bench "snoc_a16_10m " $ whnf (a16snoc a16_10m  ) r10m ,

        -- bench "append_deq"  $ whnf (foldr Deque.safeAppend Deque.empty) (replicate 10 deq100k)

        --bench "snoc_deq_1k "  $ whnf (deqsnoc deq1k   ) r1k  ,
        --bench "snoc_deq_10k " $ whnf (deqsnoc deq10k  ) r10k ,
        --bench "snoc_deq_100k" $ whnf (deqsnoc deq100k ) r100k,
        --bench "snoc_deq_1m  " $ whnf (deqsnoc deq1m   ) r1m  ,
        --bench "snoc_deq_10m " $ whnf (deqsnoc deq10m  ) r10m 

        --bench "modvec_1k "  $ whnf (modvec v1k   ) r1k  ,
        --bench "modvec_10k " $ whnf (modvec v10k  ) r10k ,
        --bench "modvec_100k" $ whnf (modvec v100k ) r100k,
        --bench "modvec_1m  " $ whnf (modvec v1m   ) r1m  ,
        --bench "modvec_10m " $ whnf (modvec v10m  ) r10m ,

        --bench "modm_1k "  $ whnf (modm m1k   ) r1k  ,
        --bench "modm_10k " $ whnf (modm m10k  ) r10k ,
        --bench "modm_100k" $ whnf (modm m100k ) r100k,
        --bench "modm_1m  " $ whnf (modm m1m   ) r1m  ,
        --bench "modm_10m " $ whnf (modm m10m  ) r10m ,

        --bench "snoc_seq_1k "  $ whnf (seqsnoc seq1k   ) r1k  ,
        --bench "snoc_seq_10k " $ whnf (seqsnoc seq10k  ) r10k ,
        --bench "snoc_seq_100k" $ whnf (seqsnoc seq100k ) r100k,
        --bench "snoc_seq_1m  " $ whnf (seqsnoc seq1m   ) r1m  ,
        --bench "snoc_seq_10m " $ whnf (seqsnoc seq10m  ) r10m 

        --bench "ixa16_1k "  $ whnf (ixa16 a16_1k   ) r1k  ,
        --bench "ixa16_10k " $ whnf (ixa16 a16_10k  ) r10k ,
        --bench "ixa16_100k" $ whnf (ixa16 a16_100k ) r100k,
        --bench "ixa16_1m  " $ whnf (ixa16 a16_1m   ) r1m  ,
        --bench "ixa16_10m " $ whnf (ixa16 a16_10m  ) r10m 

        --bench "ixdeq_1k "  $ whnf (ixdeq deq1k   ) r1k  ,
        --bench "ixdeq_10k " $ whnf (ixdeq deq10k  ) r10k ,
        --bench "ixdeq_100k" $ whnf (ixdeq deq100k ) r100k,
        --bench "ixdeq_1m  " $ whnf (ixdeq deq1m   ) r1m  ,
        --bench "ixdeq_10m " $ whnf (ixdeq deq10m  ) r10m 

        --bench "ixseq_1k "  $ whnf (ixseq seq1k   ) r1k  ,
        --bench "ixseq_10k " $ whnf (ixseq seq10k  ) r10k ,
        --bench "ixseq_100k" $ whnf (ixseq seq100k ) r100k,
        --bench "ixseq_1m  " $ whnf (ixseq seq1m   ) r1m  ,
        --bench "ixseq_10m " $ whnf (ixseq seq10m  ) r10m 

        --bench "ixm_1k "  $ whnf (ixm m1k   ) r1k  ,
        --bench "ixm_10k " $ whnf (ixm m10k  ) r10k ,
        --bench "ixm_100k" $ whnf (ixm m100k ) r100k,
        --bench "ixm_1m  " $ whnf (ixm m1m   ) r1m  ,
        --bench "ixm_10m " $ whnf (ixm m10m  ) r10m 

        --bench "ixhm_1k "  $ whnf (ixhm hm1k   ) r1k  ,
        --bench "ixhm_10k " $ whnf (ixhm hm10k  ) r10k ,
        --bench "ixhm_100k" $ whnf (ixhm hm100k ) r100k,
        --bench "ixhm_1m  " $ whnf (ixhm hm1m   ) r1m  ,
        --bench "ixhm_10m " $ whnf (ixhm hm10m  ) r10m 

        --bench "ixau_1k "  $ whnf (ixau au_1k  )  r1k  ,
        --bench "ixau_10k " $ whnf (ixau au_10k )  r10k , 
        --bench "ixau_100k" $ whnf (ixau au_100k)  r100k, 
        --bench "ixau_1m  " $ whnf (ixau au_1m  )  r1m  , 
        --bench "ixau_10m " $ whnf (ixau au_10m )  r10m 

        --bench "ixvec_1k  " $ whnf (ixvec v1k   ) r1k  ,
        --bench "ixvec_10k " $ whnf (ixvec v10k  ) r10k ,
        --bench "ixvec_100k" $ whnf (ixvec v100k ) r100k,
        --bench "ixvec_1m  " $ whnf (ixvec v1m   ) r1m  ,
        --bench "ixvec_10m " $ whnf (ixvec v10m  ) r10m 

        --bench "modau_10k " $ whnf (modau au_10k)  r10k , 
        --bench "modau_100k" $ whnf (modau au_100k) r100k, 
        --bench "modau_1m  " $ whnf (modau au_1m )  r1m  , 
        --bench "modau_10m " $ whnf (modau au_10m)  r10m ,

        --bench "moda16_1k  " $ whnf (moda16 a16_1k )  r1k  , 
        --bench "moda16_10k " $ whnf (moda16 a16_10k)  r10k , 
        --bench "moda16_100k" $ whnf (moda16 a16_100k) r100k, 
        --bench "moda16_1m  " $ whnf (moda16 a16_1m )  r1m  , 
        --bench "moda16_10m " $ whnf (moda16 a16_10m)  r10m ,

        --bench "moddeq_1k  " $ whnf (moddeq deq1k )  r1k  , 
        --bench "moddeq_10k " $ whnf (moddeq deq10k)  r10k , 
        --bench "moddeq_100k" $ whnf (moddeq deq100k) r100k, 
        --bench "moddeq_1m  " $ whnf (moddeq deq1m )  r1m  , 
        --bench "moddeq_10m " $ whnf (moddeq deq10m)  r10m 

        --bench "modim_10k"  $ whnf (modim im10k  ) r10k,
        --bench "modim_100k" $ whnf (modim im100k ) r100k,
        --bench "modim_1m  " $ whnf (modim im1m   ) r1m  ,
        --bench "modim_10m " $ whnf (modim im10m  ) r10m 

        --bench "modseq_1k"   $ whnf (modseq seq1k   ) r1k  , 
        --bench "modseq_10k"  $ whnf (modseq seq10k  ) r10k ,
        --bench "modseq_100k" $ whnf (modseq seq100k ) r100k,
        --bench "modseq_1m  " $ whnf (modseq seq1m   ) r1m  ,
        --bench "modseq_10m " $ whnf (modseq seq10m  ) r10m ,

        --bench "inslist_1k"   $ whnf (inslist list1k   ) r1k  , 
        --bench "inslist_10k"  $ whnf (inslist list10k  ) r10k ,
        --bench "inslist_100k" $ whnf (inslist list100k ) r100k,
        --bench "inslist_1m  " $ whnf (inslist list1m   ) r1m  ,
        --bench "inslist_10m " $ whnf (inslist list10m  ) r10m ,

        --bench "insseq_1k"   $ whnf (insseq seq1k   ) r1k  , 
        --bench "insseq_10k"  $ whnf (insseq seq10k  ) r10k ,
        --bench "insseq_100k" $ whnf (insseq seq100k ) r100k,
        --bench "insseq_1m  " $ whnf (insseq seq1m   ) r1m  ,
        --bench "insseq_10m " $ whnf (insseq seq10m  ) r10m 

        -- ]





-- rs n = take 100 $ randomRs (0, n - 1 :: Int) (mkStdGen 0)
-- a16n n = foldl' Vec.snoc Vec.empty $ [0..n::Int]
-- aun  n = foldl' UVec.snoc  UVec.empty  $ [0..n::Int]

-- ixa16 a [i]    = a Vec.! i 
-- ixa16 a (i:is) = seq (a Vec.! i) (ixa16 a is)
-- ixa16 _ _      = undefined 

-- ixau a [i] = a UVec.! i 
-- ixau a (i:is) = seq (a UVec.! i) (ixau a is)
-- ixau _ _ = undefined 

-- ixvec a [i] = a V.! i 
-- ixvec a (i:is) = seq (a V.! i) (ixvec a is)
-- ixvec _ _ = undefined 

-- ixhm a [i] = a HM.! i 
-- ixhm a (i:is) = seq (a HM.! i) (ixhm a is)
-- ixhm _ _ = undefined 

-- ixim a [i] = a IM.! i 
-- ixim a (i:is) = seq (a IM.! i) (ixim a is)
-- ixim _ _ = undefined 

-- ixm a [i] = a M.! i 
-- ixm a (i:is) = seq (a M.! i) (ixm a is)
-- ixm _ _ = undefined 

-- ixdeq a [i] = Deque.unsafeIndex a i
-- ixdeq a (i:is) = seq (Deque.unsafeIndex a i) (ixdeq a is)
-- ixdeq _ _ = undefined

-- ixseq a [i] = Seq.index a i
-- ixseq a (i:is) = seq (Seq.index a i) (ixseq a is)
-- ixseq _ _ = undefined 

-- moddeq !a [] = a
-- moddeq !a (i:is) = moddeq (Deque.modify a i (const i)) is

-- modseq !a [] = a
-- modseq !a (i:is) = modseq (Seq.adjust (const i) i a) is

-- moda16 !a [] = a
-- moda16 !a (i:is) = moda16 (Vec.modify a i (const i)) is

-- insseq !a [] = a
-- insseq !a (i:is) = insseq (let (!a', !b') = Seq.splitAt i a in (a' Seq.|> i) Seq.>< b') is

-- modau !a [] = a
-- modau !a (i:is) = modau (UVec.modify a i (const i)) is

-- modhm !a [] = a
-- modhm !a (i:is) = modhm (HM.adjust (const i) i a) is

-- modim !a [] = a
-- modim !a (i:is) = modim (IM.adjust (const i) i a) is

-- modm !a [] = a
-- modm !a (i:is) = modm (M.adjust (const i) i a) is

-- modvec :: V.Vector Int -> [Int] -> V.Vector Int
-- modvec v is = runST $ do
--     v <- V.unsafeThaw v
--     let go !v []     = return ()
--         go !v (i:is) = do
--             MV.write v i i
--             go v is 
--     go v is
--     V.unsafeFreeze v 


-- seqsnoc !a [] = a
-- seqsnoc !a (i:is) = seqsnoc (a Seq.|> i) is

-- deqsnoc !a [] = a
-- deqsnoc a (i:is) = deqsnoc (a Deque.|> i) is

-- a16snoc !a [] = a
-- a16snoc !a (i:is) = a16snoc (Vec.snoc a i) is


-- inslist a [] = last a
-- inslist a (i:is) = inslist (go i i a) is where
--     go 0 y xs = y:xs
--     go i y (x:xs) = x: go (i - 1) y xs
--     go _ _ xs = xs 







-- CLOJURE BENCHMARKS:

    -- made with Criterium

    -- vec-ix : 1k 10k 100k 1m 10m
    -- vec-mod : 1k 10k 100k 1m 10m
    -- vec-snoc : 1k 10k 100k 1m 10m


--vec-ix
--WARNING: JVM argument TieredStopAtLevel=1 is active, and may lead to unexpected results as JIT C2 compiler may not be active. See http://www.slideshare.net/CharlesNutter/javaone-2012-jvm-jit-for-dummies.
--WARNING: Final GC required 8.712648670421844 % of runtime
--WARNING: Final GC required 72.52774639432937 % of runtime
--Evaluation count : 21132 in 6 samples of 3522 calls.
--             Execution time mean : 28.491241 µs
--    Execution time std-deviation : 159.734660 ns
--   Execution time lower quantile : 28.274707 µs ( 2.5%)
--   Execution time upper quantile : 28.671091 µs (97.5%)
--                   Overhead used : 102.476148 ns

--Found 2 outliers in 6 samples (33.3333 %)
--    low-severe   1 (16.6667 %)
--    low-mild     1 (16.6667 %)
-- Variance from outliers : 13.8889 % Variance is moderately inflated by outliers
--WARNING: JVM argument TieredStopAtLevel=1 is active, and may lead to unexpected results as JIT C2 compiler may not be active. See http://www.slideshare.net/CharlesNutter/javaone-2012-jvm-jit-for-dummies.n
--WARNING: Final GC required 77.58702035540591 % of runtime
--Evaluation count : 21174 in 6 samples of 3529 calls.
--             Execution time mean : 29.379213 µs
--    Execution time std-deviation : 550.722759 ns
--   Execution time lower quantile : 28.696341 µs ( 2.5%)
--   Execution time upper quantile : 30.197772 µs (97.5%)
--                   Overhead used : 102.476148 ns

--Found 1 outliers in 6 samples (16.6667 %)
--    low-severe   1 (16.6667 %)
-- Variance from outliers : 13.8889 % Variance is moderately inflated by outliers
--WARNING: JVM argument TieredStopAtLevel=1 is active, and may lead to unexpected results as JIT C2 compiler may not be active. See http://www.slideshare.net/CharlesNutter/javaone-2012-jvm-jit-for-dummies.
--WARNING: Final GC required 61.93731722081877 % of runtime
--Evaluation count : 20148 in 6 samples of 3358 calls.
--             Execution time mean : 29.150456 µs
--    Execution time std-deviation : 349.536441 ns
--   Execution time lower quantile : 28.758595 µs ( 2.5%)
--   Execution time upper quantile : 29.635453 µs (97.5%)
--                   Overhead used : 102.476148 ns
--WARNING: JVM argument TieredStopAtLevel=1 is active, and may lead to unexpected results as JIT C2 compiler may not be active. See http://www.slideshare.net/CharlesNutter/javaone-2012-jvm-jit-for-dummies.
--WARNING: Final GC required 66.13241730743006 % of runtime
--Evaluation count : 20778 in 6 samples of 3463 calls.
--             Execution time mean : 29.166131 µs
--    Execution time std-deviation : 400.956557 ns
--   Execution time lower quantile : 28.703371 µs ( 2.5%)
--   Execution time upper quantile : 29.636564 µs (97.5%)
--                   Overhead used : 102.476148 ns
--WARNING: JVM argument TieredStopAtLevel=1 is active, and may lead to unexpected results as JIT C2 compiler may not be active. See http://www.slideshare.net/CharlesNutter/javaone-2012-jvm-jit-for-dummies.
--WARNING: Final GC required 68.15514958622104 % of runtime
--Evaluation count : 18738 in 6 samples of 3123 calls.
--             Execution time mean : 31.594904 µs
--    Execution time std-deviation : 381.566143 ns
--   Execution time lower quantile : 31.043042 µs ( 2.5%)
--   Execution time upper quantile : 31.992377 µs (97.5%)
--                   Overhead used : 102.476148 ns

--Found 2 outliers in 6 samples (33.3333 %)
--    low-severe   1 (16.6667 %)
--    low-mild     1 (16.6667 %)
-- Variance from outliers : 13.8889 % Variance is moderately inflated by outliers



--vec-mod
--WARNING: JVM argument TieredStopAtLevel=1 is active, and may lead to unexpected results as JIT C2 compiler may not be active. See http://www.slideshare.net/CharlesNutter/javaone-2012-jvm-jit-for-dummies.
--WARNING: Final GC required 52.214644128053145 % of runtime
--Evaluation count : 23862 in 6 samples of 3977 calls.
--             Execution time mean : 28.385615 µs
--    Execution time std-deviation : 8.754492 µs
--   Execution time lower quantile : 24.560843 µs ( 2.5%)
--   Execution time upper quantile : 43.565555 µs (97.5%)
--                   Overhead used : 102.476148 ns

--Found 1 outliers in 6 samples (16.6667 %)
--    low-severe   1 (16.6667 %)
-- Variance from outliers : 81.4321 % Variance is severely inflated by outliers
--WARNING: JVM argument TieredStopAtLevel=1 is active, and may lead to unexpected results as JIT C2 compiler may not be active. See http://www.slideshare.net/CharlesNutter/javaone-2012-jvm-jit-for-dummies.
--WARNING: Final GC required 59.10677387999761 % of runtime
--Evaluation count : 17376 in 6 samples of 2896 calls.
--             Execution time mean : 34.179722 µs
--    Execution time std-deviation : 270.474529 ns
--   Execution time lower quantile : 33.850682 µs ( 2.5%)
--   Execution time upper quantile : 34.471739 µs (97.5%)
--                   Overhead used : 102.476148 ns
--WARNING: JVM argument TieredStopAtLevel=1 is active, and may lead to unexpected results as JIT C2 compiler may not be active. See http://www.slideshare.net/CharlesNutter/javaone-2012-jvm-jit-for-dummies.
--WARNING: Final GC required 58.43123855432015 % of runtime
--Evaluation count : 14166 in 6 samples of 2361 calls.
--             Execution time mean : 42.134162 µs
--    Execution time std-deviation : 375.971682 ns
--   Execution time lower quantile : 41.746280 µs ( 2.5%)
--   Execution time upper quantile : 42.551080 µs (97.5%)
--                   Overhead used : 102.476148 ns
--WARNING: JVM argument TieredStopAtLevel=1 is active, and may lead to unexpected results as JIT C2 compiler may not be active. See http://www.slideshare.net/CharlesNutter/javaone-2012-jvm-jit-for-dummies.
--WARNING: Final GC required 64.25197610565603 % of runtime
--Evaluation count : 13632 in 6 samples of 2272 calls.
--             Execution time mean : 43.894714 µs
--    Execution time std-deviation : 763.343983 ns
--   Execution time lower quantile : 43.338979 µs ( 2.5%)
--   Execution time upper quantile : 45.086894 µs (97.5%)
--                   Overhead used : 102.476148 ns
--WARNING: JVM argument TieredStopAtLevel=1 is active, and may lead to unexpected results as JIT C2 compiler may not be active. See http://www.slideshare.net/CharlesNutter/javaone-2012-jvm-jit-for-dummies.
--WARNING: Final GC required 61.03329648678262 % of runtime
--Evaluation count : 11670 in 6 samples of 1945 calls.
--             Execution time mean : 52.320144 µs
--    Execution time std-deviation : 896.150726 ns
--   Execution time lower quantile : 51.489890 µs ( 2.5%)
--   Execution time upper quantile : 53.743962 µs (97.5%)
--                   Overhead used : 102.476148 ns

--Found 1 outliers in 6 samples (16.6667 %)
--    low-severe   1 (16.6667 %)
-- Variance from outliers : 13.8889 % Variance is moderately inflated by outliers



--vec-snoc

--WARNING: JVM argument TieredStopAtLevel=1 is active, and may lead to unexpected results as JIT C2 compiler may not be active. See http://www.slideshare.net/CharlesNutter/javaone-2012-jvm-jit-for-dummies.
--WARNING: Final GC required 64.4505780859485 % of runtime
--Evaluation count : 69378 in 6 samples of 11563 calls.
--             Execution time mean : 8.605363 µs
--    Execution time std-deviation : 32.416549 ns
--   Execution time lower quantile : 8.574331 µs ( 2.5%)
--   Execution time upper quantile : 8.640635 µs (97.5%)
--                   Overhead used : 102.476148 ns
--WARNING: JVM argument TieredStopAtLevel=1 is active, and may lead to unexpected results as JIT C2 compiler may not be active. See http://www.slideshare.net/CharlesNutter/javaone-2012-jvm-jit-for-dummies.
--WARNING: Final GC required 66.04796348663042 % of runtime
--Evaluation count : 67470 in 6 samples of 11245 calls.
--             Execution time mean : 8.812530 µs
--    Execution time std-deviation : 21.837156 ns
--   Execution time lower quantile : 8.796731 µs ( 2.5%)
--   Execution time upper quantile : 8.848125 µs (97.5%)
--                   Overhead used : 102.476148 ns

--Found 1 outliers in 6 samples (16.6667 %)
--    low-severe   1 (16.6667 %)
-- Variance from outliers : 13.8889 % Variance is moderately inflated by outliers
--WARNING: JVM argument TieredStopAtLevel=1 is active, and may lead to unexpected results as JIT C2 compiler may not be active. See http://www.slideshare.net/CharlesNutter/javaone-2012-jvm-jit-for-dummies.
--WARNING: Final GC required 63.03190982745803 % of runtime
--Evaluation count : 63612 in 6 samples of 10602 calls.
--             Execution time mean : 9.437749 µs
--    Execution time std-deviation : 106.769535 ns
--   Execution time lower quantile : 9.356776 µs ( 2.5%)
--   Execution time upper quantile : 9.620215 µs (97.5%)
--                   Overhead used : 102.476148 ns

--Found 1 outliers in 6 samples (16.6667 %)
--    low-severe   1 (16.6667 %)
-- Variance from outliers : 13.8889 % Variance is moderately inflated by outliers
--WARNING: JVM argument TieredStopAtLevel=1 is active, and may lead to unexpected results as JIT C2 compiler may not be active. See http://www.slideshare.net/CharlesNutter/javaone-2012-jvm-jit-for-dummies.
--WARNING: Final GC required 64.08002813449524 % of runtime
--Evaluation count : 63126 in 6 samples of 10521 calls.
--             Execution time mean : 9.724675 µs
--    Execution time std-deviation : 543.899142 ns
--   Execution time lower quantile : 9.277733 µs ( 2.5%)
--   Execution time upper quantile : 10.642660 µs (97.5%)
--                   Overhead used : 102.476148 ns

--Found 1 outliers in 6 samples (16.6667 %)
--    low-severe   1 (16.6667 %)
-- Variance from outliers : 14.3542 % Variance is moderately inflated by outliers
--WARNING: JVM argument TieredStopAtLevel=1 is active, and may lead to unexpected results as JIT C2 compiler may not be active. See http://www.slideshare.net/CharlesNutter/javaone-2012-jvm-jit-for-dummies.
--WARNING: Final GC required 62.13127867898165 % of runtime
--Evaluation count : 60582 in 6 samples of 10097 calls.
--             Execution time mean : 9.788229 µs
--    Execution time std-deviation : 92.192510 ns
--   Execution time lower quantile : 9.706949 µs ( 2.5%)
--   Execution time upper quantile : 9.929750 µs (97.5%)
--                   Overhead used : 102.476148 ns

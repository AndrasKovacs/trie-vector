 {-# LANGUAGE BangPatterns, MagicHash, ScopedTypeVariables #-}


import GHC.Prim
import GHC.Types
import Data.Word

import qualified Vector as Vec
import qualified Unboxed as UVec

import Criterion.Main
import Criterion.Config

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Sequence as Seq
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M 

import System.Random
import Control.Monad
import Control.Applicative
import Data.Hashable
import Data.List
import Control.DeepSeq
import Control.Monad.ST.Strict

rs n = take 100 $ randomRs (0, n - 1 :: Int) (mkStdGen 0)

a16n n = foldl' Vec.snoc Vec.empty $ [0..n::Int]
aun  n = foldl' UVec.snoc  UVec.empty  $ [0..n::Int]

config = defaultConfig {cfgSamples = ljust 8}


ixa16 a [i]    = a Vec.! i 
ixa16 a (i:is) = seq (a Vec.! i) (ixa16 a is)
ixa16 _ _      = undefined 

ixau a [i] = a UVec.! i 
ixau a (i:is) = seq (a UVec.! i) (ixau a is)
ixau _ _ = undefined 

ixvec a [i] = a V.! i 
ixvec a (i:is) = seq (a V.! i) (ixvec a is)
ixvec _ _ = undefined 

ixhm a [i] = a HM.! i 
ixhm a (i:is) = seq (a HM.! i) (ixhm a is)
ixhm _ _ = undefined 

ixim a [i] = a IM.! i 
ixim a (i:is) = seq (a IM.! i) (ixim a is)
ixim _ _ = undefined 

ixm a [i] = a M.! i 
ixm a (i:is) = seq (a M.! i) (ixm a is)
ixm _ _ = undefined 

ixseq a [i] = Seq.index a i
ixseq a (i:is) = seq (Seq.index a i) (ixseq a is)
ixseq _ _ = undefined 

modseq !a [] = a
modseq !a (i:is) = modseq (Seq.adjust (const i) i a) is

moda16 !a [] = a
moda16 !a (i:is) = moda16 (Vec.modify a i (const i)) is

modau !a [] = a
modau !a (i:is) = modau (UVec.modify a i (const i)) is

modhm !a [] = a
modhm !a (i:is) = modhm (HM.adjust (const i) i a) is

modim !a [] = a
modim !a (i:is) = modim (IM.adjust (const i) i a) is

modm !a [] = a
modm !a (i:is) = modm (M.adjust (const i) i a) is

modvec :: V.Vector Int -> [Int] -> V.Vector Int
modvec v is = runST $ do
    v <- V.unsafeThaw v
    let go !v []     = return ()
        go !v (i:is) = do
            MV.write v i i
            go v is 
    go v is
    V.unsafeFreeze v 


seqsnoc !a [] = a
seqsnoc !a (i:is) = seqsnoc (a Seq.|> i) is

a16snoc !a [] = a
a16snoc !a (i:is) = a16snoc (Vec.snoc a i) is

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

main = do
    let 
        r1k    = force $ rs 1000
        r10k   = force $ rs 10000
        r100k  = force $ rs 100000
        r1m    = force $ rs 1000000
        r10m   = force $ rs 10000000

        !au_1k   = aun 1000
        !au_10k  = aun 10000
        !au_100k = aun 100000
        !au_1m   = aun 1000000
        !au_10m  = aun 10000000

        a16_1k   = a16n 1000
        a16_10k  = a16n 10000
        a16_100k = a16n 100000
        a16_1m   = a16n 1000000
        a16_10m  = a16n 10000000

        v1k   = V.fromList [0..1000 ::Int]
        v10k  = V.fromList [0..10000 ::Int]
        v100k = V.fromList [0..100000 ::Int]
        v1m   = V.fromList [0..1000000 ::Int]
        v10m  = V.fromList [0..10000000 ::Int]

        seq1k   = Seq.fromList [0..1000 :: Int]
        seq10k  = Seq.fromList [0..10000 ::Int]
        seq100k = Seq.fromList [0..100000 ::Int]
        seq1m   = Seq.fromList [0..1000000 ::Int]
        seq10m  = Seq.fromList [0..10000000 ::Int]

        im1k    = IM.fromList $ zip [0..1000     :: Int] [0..1000     :: Int]
        im10k   = IM.fromList $ zip [0..10000    :: Int] [0..10000    :: Int]
        im100k  = IM.fromList $ zip [0..100000   :: Int] [0..100000   :: Int]
        im1m    = IM.fromList $ zip [0..1000000  :: Int] [0..1000000  :: Int]
        im10m   = IM.fromList $ zip [0..10000000 :: Int] [0..10000000 :: Int]

        m1k    = M.fromList $ zip [0..1000     :: Int] [0..1000     :: Int]
        m10k   = M.fromList $ zip [0..10000    :: Int] [0..10000    :: Int]
        m100k  = M.fromList $ zip [0..100000   :: Int] [0..100000   :: Int]
        m1m    = M.fromList $ zip [0..1000000  :: Int] [0..1000000  :: Int]
        m10m   = M.fromList $ zip [0..10000000 :: Int] [0..10000000 :: Int]

        hm1k    = HM.fromList $ zip [0..1000     :: Int] [0..1000     :: Int]
        hm10k   = HM.fromList $ zip [0..10000    :: Int] [0..10000    :: Int]
        hm100k  = HM.fromList $ zip [0..100000   :: Int] [0..100000   :: Int]
        hm1m    = HM.fromList $ zip [0..1000000  :: Int] [0..1000000  :: Int]
        hm10m   = HM.fromList $ zip [0..10000000 :: Int] [0..10000000 :: Int]

    defaultMainWith config (return ()) [


        --bench "snoc_a16_1k "  $ whnf (a16snoc a16_1k   ) r1k  ,
        --bench "snoc_a16_10k " $ whnf (a16snoc a16_10k  ) r10k ,
        --bench "snoc_a16_100k" $ whnf (a16snoc a16_100k ) r100k,
        --bench "snoc_a16_1m  " $ whnf (a16snoc a16_1m   ) r1m  ,
        --bench "snoc_a16_10m " $ whnf (a16snoc a16_10m  ) r10m ,

        --bench "snoc_a16_1k "  $ whnf (a16snoc a16_1k   ) r1k  ,
        --bench "snoc_a16_10k " $ whnf (a16snoc a16_10k  ) r10k ,
        --bench "snoc_a16_100k" $ whnf (a16snoc a16_100k ) r100k,
        --bench "snoc_a16_1m  " $ whnf (a16snoc a16_1m   ) r1m  ,
        --bench "snoc_a16_10m " $ whnf (a16snoc a16_10m  ) r10m ,

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
        --bench "ixa16_10m " $ whnf (ixa16 a16_10m  ) r10m ,

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

        --bench "modim_10k"  $ whnf (modim im10k  ) r10k,
        --bench "modim_100k" $ whnf (modim im100k ) r100k,
        --bench "modim_1m  " $ whnf (modim im1m   ) r1m  ,
        --bench "modim_10m " $ whnf (modim im10m  ) r10m 

        --bench "modseq_10k"  $ whnf (modseq seq10k  ) r10k,
        --bench "modseq_100k" $ whnf (modseq seq100k ) r100k,
        --bench "modseq_1m  " $ whnf (modseq seq1m   ) r1m  ,
        --bench "modseq_10m " $ whnf (modseq seq10m  ) r10m 

        ]









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
--WARNING: JVM argument TieredStopAtLevel=1 is active, and may lead to unexpected results as JIT C2 compiler may not be active. See http://www.slideshare.net/CharlesNutter/javaone-2012-jvm-jit-for-dummies.
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

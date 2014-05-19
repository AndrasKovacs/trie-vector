{-# LANGUAGE BangPatterns, MagicHash #-}


import GHC.Prim
import GHC.Types

import qualified Flat16 as F16
import qualified Flat32 as F32
import qualified Flat16Bottom as F16B

import Criterion.Main
import Criterion.Config
import qualified Data.Vector as V
import qualified Data.Sequence as Seq
import qualified Data.HashMap.Strict as HM
import System.Random
import Control.Monad
import Control.Applicative
import Data.Hashable

rs n = take n $ randomRs (0, n::Int) (mkStdGen 0)

f16nzeros  n = foldl F16.snoc F16.empty $ replicate n (0 :: Int)
f16bnzeros n = foldl F16B.snoc F16B.empty $ replicate n (0 :: Int)
f32nzeros  n = foldl F32.snoc F32.empty $ replicate n (0 :: Int)



config = defaultConfig {cfgSamples = ljust 20}


main :: IO ()
main = do
    -- !r10    <- rs 10
    -- !r100   <- rs 100
    -- !r1k    <- rs 1000
    -- !r10k   <- rs 10000
    -- !r100k  <- rs 100000
    -- !r1m    <- rs 1000000

    let !f16_10   = f16nzeros 10
        !f16_100  = f16nzeros 100
        !f16_1k   = f16nzeros 1000
        !f16_10k  = f16nzeros 10000
        !f16_100k = f16nzeros 100000
        !f16_1m   = f16nzeros 1000000
        !f16_10m  = f16nzeros 10000000

    let !f16b_10   = f16bnzeros 10
        !f16b_100  = f16bnzeros 100
        !f16b_1k   = f16bnzeros 1000
        !f16b_10k  = f16bnzeros 10000
        !f16b_100k = f16bnzeros 100000
        !f16b_1m   = f16bnzeros 1000000
        !f16b_10m  = f16bnzeros 10000000

    --let !f32_10   = f32nzeros 10
    --    !f32_100  = f32nzeros 100
    --    !f32_1k   = f32nzeros 1000
    --    !f32_10k  = f32nzeros 10000
    --    !f32_100k = f32nzeros 100000
    --    !f32_1m   = f32nzeros 1000000
    --    !f32_10m  = f32nzeros 10000000

    let !v1k   = V.replicate 1000   (0 :: Int)
        !v10k  = V.replicate 10000  (0 :: Int)
        !v100k = V.replicate 100000  (0 :: Int)
        !v1m   = V.replicate 1000000 (0 :: Int)
        !v10m  = V.replicate 10000000 (0 :: Int)
 
    --let !seq10   = Seq.replicate 10 (0 :: Int)
    --    !seq100  = Seq.replicate 100 (0 :: Int)
    --    !seq1k   = Seq.replicate 1000 (0 :: Int)
    --    !seq10k  = Seq.replicate 10000 (0 :: Int)
    --    !seq100k = Seq.replicate 100000 (0 :: Int)
    --    !seq1m   = Seq.replicate 1000000 (0 :: Int)
    --    !seq10m  = Seq.replicate 10000000 (0 :: Int)

    let !hm100k  = HM.fromList $ zip [0..1000000 :: Int] (repeat (0::Int))
        -- !hm1m    = HM.fromList $ zip [0..10000000 :: Int] (repeat (0::Int))
        !hm10m    = HM.fromList $ zip [0..10000000 :: Int] (repeat (0::Int))



    defaultMainWith config (return ()) [
        --bench "ixflat16_10  " $ nf (f16_10   F16.!) 0,
        --bench "ixflat16_100 " $ nf (f16_100  F16.!) 0,
        --bench "ixflat16_1k  " $ nf (f16_1k   F16.!) 500,
        --bench "ixflat16_10k " $ nf (f16_10k  F16.!) 500,
        --bench "ixflat16_100k" $ nf (f16_100k F16.!) 50000,
        --bench "ixflat16_1m  " $ nf (f16_1m   F16.!) 500000,
        --bench "ixflat16_10m " $ nf (f16_10m  F16.!) 5000000,


        --bench "ixflat16b_10  " $ nf (f16b_10   F16B.!) 0,
        --bench "ixflat16b_100 " $ nf (f16b_100  F16B.!) 0,
        bench "ixflat16b_1k  " $ nf (f16b_1k   F16B.!) 500,
        --bench "ixflat16b_10k " $ nf (f16b_10k  F16B.!) 500,
        bench "ixflat16b_100k" $ nf (f16b_100k F16B.!) 50000,
        --bench "ixflat16b_1m  " $ nf (f16b_1m   F16B.!) 500000,
        bench "ixflat16b_10m " $ nf (f16b_10m  F16B.!) 5000000,

        bench "inthash" $ nf hash (0::Int),


        --bench "ixvec_1k"      $ nf (v1k   `V.unsafeIndex`) 500,
        --bench "ixvec_10k"     $ nf (v10k  `V.unsafeIndex`) 500,
        --bench "ixvec_100k"    $ nf (v100k `V.unsafeIndex`) 50000,
        --bench "ixvec_1m"      $ nf (v1m   `V.unsafeIndex`) 500000

        --bench "ixflat32_10  " $ nf (f32_10   F32.!) 0,
        --bench "ixflat32_100 " $ nf (f32_100  F32.!) 0,
        --bench "ixflat32_1k  " $ nf (f32_1k   F32.!) 0,
        --bench "ixflat32_10k " $ nf (f32_10k  F32.!) 0,
        --bench "ixflat32_100k" $ nf (f32_100k F32.!) 50000,
        --bench "ixflat32_1m  " $ nf (f32_1m   F32.!) 500000,
        --bench "ixflat32_10m " $ nf (f32_10m  F32.!) 5000000

        --bench "ixseq_10  " $ nf (seq10   `Seq.index`) 0,
        --bench "ixseq_100 " $ nf (seq100  `Seq.index`) 0,
        --bench "ixseq_1k  " $ nf (seq1k   `Seq.index`) 0,
        --bench "ixseq_10k " $ nf (seq10k  `Seq.index`) 0,
        --bench "ixseq_100k" $ nf (seq100k `Seq.index`) 50000,
        --bench "ixseq_1m  " $ nf (seq1m   `Seq.index`) 500000,
        --bench "ixseq_10m"  $ nf (seq10m  `Seq.index`) 5000000,
        bench "ixhm_10m"  $ nf (hm100k  HM.!) 0

        ]




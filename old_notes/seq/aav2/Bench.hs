{-# LANGUAGE BangPatterns, MagicHash #-}

import qualified ArrayArray16 as A16
import qualified ArrayArray32 as A32

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

a16nzeros n = foldl A16.snoc A16.empty $ replicate n (0 :: Int)
a32nzeros n = foldl A32.snoc A32.empty $ replicate n (0 :: Int)

config = defaultConfig {cfgSamples = ljust 10}


main :: IO ()
main = do
    -- !r10    <- rs 10
    -- !r100   <- rs 100
    -- !r1k    <- rs 1000
    -- !r10k   <- rs 10000
    -- !r100k  <- rs 100000
    -- !r1m    <- rs 1000000

    let a16_10   = a16nzeros 10
        a16_100  = a16nzeros 100
        a16_1k   = a16nzeros 1000
        a16_10k  = a16nzeros 10000
        a16_100k = a16nzeros 100000
        a16_1m   = a16nzeros 1000000
        a16_10m  = a16nzeros 10000000

    let !a32_10   = a32nzeros 10
        !a32_100  = a32nzeros 100
        !a32_1k   = a32nzeros 1000
        !a32_10k  = a32nzeros 10000
        !a32_100k = a32nzeros 100000
        !a32_1m   = a32nzeros 1000000
        !a32_10m  = a32nzeros 10000000

    let !v1k   = V.replicate 1000 (0 :: Int)
        !v10k  = V.replicate 10000 (0 :: Int)
        !v100k = V.replicate 100000 (0 :: Int)
        !v1m   = V.replicate 1000000 (0 :: Int)
        !v10m  = V.replicate 10000000 (0 :: Int)
 
    let seq10   = Seq.replicate 10 (0 :: Int)
        seq100  = Seq.replicate 100 (0 :: Int)
        seq1k   = Seq.replicate 1000 (0 :: Int)
        seq10k  = Seq.replicate 10000 (0 :: Int)
        seq100k = Seq.replicate 100000 (0 :: Int)
        seq1m   = Seq.replicate 1000000 (0 :: Int)
        seq10m  = Seq.replicate 10000000 (0 :: Int)

    let hm100k  = HM.fromList $ zip [0..100000   :: Int] (repeat (0::Int))
        hm1m    = HM.fromList $ zip [0..1000000  :: Int] (repeat (0::Int))
        hm10m   = HM.fromList $ zip [0..10000000 :: Int] (repeat (0::Int))

    defaultMainWith config (return ()) [
        --bench "ixa16_10  " $ nf (a16_10   A16.!) 0,
        --bench "ixa16_100 " $ nf (a16_100  A16.!) 0,
        --bench "ixa16_1k  " $ nf (a16_1k   A16.!) 500,
        --bench "ixa16_10k " $ nf (a16_10k  A16.!) 500,
        --bench "ixa16_100k" $ nf (a16_100k A16.!) 50000,
        --bench "ixa16_1m  " $ nf (a16_1m   A16.!) 500000,
        --bench "ixa16_10m " $ nf (a16_10m  A16.!) 5000000,

        bench "ixa32_10  " $ nf (a32_10   A32.!) 5,
        bench "ixa32_100 " $ nf (a32_100  A32.!) 50,
        bench "ixa32_1k  " $ nf (a32_1k   A32.!) 500,
        bench "ixa32_10k " $ nf (a32_10k  A32.!) 5000,
        bench "ixa32_100k" $ nf (a32_100k A32.!) 50000,
        bench "ixa32_1m  " $ nf (a32_1m   A32.!) 500000,
        bench "ixa32_10m " $ nf (a32_10m  A32.!) 5000000,

        --bench "inthash" $ nf hash (0::Int)

        bench "ixvec_1k"      $ nf (v1k   V.!) 500,
        bench "ixvec_10k"     $ nf (v10k  V.!) 500,
        bench "ixvec_100k"    $ nf (v100k V.!) 50000,
        bench "ixvec_1m"      $ nf (v1m   V.!) 500000

        --bench "ixseq_10  " $ nf (seq10   `Seq.index`) 0,
        --bench "ixseq_100 " $ nf (seq100  `Seq.index`) 0,
        --bench "ixseq_1k  " $ nf (seq1k   `Seq.index`) 0,
        --bench "ixseq_10k " $ nf (seq10k  `Seq.index`) 0,
        --bench "ixseq_100k" $ nf (seq100k `Seq.index`) 50000,
        --bench "ixseq_1m  " $ nf (seq1m   `Seq.index`) 500000,
        --bench "ixseq_10m"  $ nf (seq10m  `Seq.index`) 5000000,

        --bench "ixhm_10m"  $ nf (hm100k  HM.!) 0

        ]




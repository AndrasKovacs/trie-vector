{-# LANGUAGE BangPatterns, MagicHash #-}


import GHC.Prim
import GHC.Types
import Data.Word

import qualified ArrayArray16 as A16
import qualified ArrayArray32 as A32
import qualified ArrayArrayUnboxed as AU

import Criterion.Main
import Criterion.Config
import qualified Data.Vector as V
import qualified Data.Sequence as Seq
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict as IM
import System.Random
import Control.Monad
import Control.Applicative
import Data.Hashable
import Data.List

rs n = take n $ randomRs (0, n::Int) (mkStdGen 0)

a16nzeros n = foldl' A16.snoc A16.empty $ replicate n (0 :: Int)
a32nzeros n = foldl' A32.snoc A32.empty $ replicate n (0 :: Int)
aunzeros  n = foldl' AU.snoc  AU.empty $ replicate n (0 :: Int)

config = defaultConfig {cfgSamples = ljust 15}

snoca16 arr (I# n) = go arr 0# n where
    go !arr i n = case i ==# n of
        1# -> arr
        _  -> go (A16.snoc arr 0) (i +# 1#) n


main :: IO ()
main = do
    -- !r10    <- rs 10
    -- !r100   <- rs 100
    -- !r1k    <- rs 1000
    -- !r10k   <- rs 10000
    -- !r100k  <- rs 100000
    -- !r1m    <- rs 1000000

    let !a16_10   = a16nzeros 10
        !a16_100  = a16nzeros 100
        !a16_1k   = a16nzeros 1000
        !a16_10k  = a16nzeros 10000
        !a16_100k = a16nzeros 100000
        !a16_1m   = a16nzeros 1000000
        !a16_10m  = a16nzeros 10000000

    let !au_10   = aunzeros 10
        !au_100  = aunzeros 100
        !au_1k   = aunzeros 1000
        !au_10k  = aunzeros 10000
        !au_100k = aunzeros 100000
        !au_1m   = aunzeros 1000000
        !au_10m  = aunzeros 10000000

    --let !a32_10   = a32nzeros 10
    --    !a32_100  = a32nzeros 100
    --    !a32_1k   = a32nzeros 1000
    --    !a32_10k  = a32nzeros 10000
    --    !a32_100k = a32nzeros 100000
    --    !a32_1m   = a32nzeros 1000000
    --    !a32_10m  = a32nzeros 10000000

    --let !v1k   = V.replicate 1000 (0 :: Int)
    --    !v10k  = V.replicate 10000 (0 :: Int)
    --    !v100k = V.replicate 100000 (0 :: Int)
    --    !v1m   = V.replicate 1000000 (0 :: Int)
    --    !v10m  = V.replicate 10000000 (0 :: Int)
 
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

    --let !im100k  = IM.fromList $ zip [0..100000   :: Int] (repeat (0::Int))
    --    !im1m    = IM.fromList $ zip [0..1000000  :: Int] (repeat (0::Int))
    --    !im10m   = IM.fromList $ zip [0..10000000 :: Int] (repeat (0::Int))

    defaultMainWith config (return ()) [
        --bench "ixa16_10  " $ nf (a16_10   A16.!) 0,
        --bench "ixa16_100 " $ nf (a16_100  A16.!) 0,
        --bench "ixa16_1k  " $ nf (a16_1k   A16.!) 500,
        --bench "ixa16_10k " $ nf (a16_10k  A16.!) 500,
        --bench "ixa16_100k" $ nf (a16_100k A16.!) 50000,
        --bench "ixa16_1m  " $ nf (a16_1m   A16.!) 500000,
        --bench "ixa16_10m " $ nf (a16_10m  A16.!) 5000000,

        --bench "ixau_10  " $ nf (au_10   AU.!) 0,
        --bench "ixau_100 " $ nf (au_100  AU.!) 0,
        --bench "ixau_1k  " $ nf (au_1k   AU.!) 500,
        --bench "ixau_10k " $ nf (au_10k  AU.!) 500,
        --bench "ixau_100k" $ nf (au_100k AU.!) 50000,
        --bench "ixau_1m  " $ nf (au_1m   AU.!) 500000,
        --bench "ixau_10m " $ nf (au_10m  AU.!) 5000000

        --bench "snocau_10  " $ whnf (au_10   `AU.snoc`) 0,
        --bench "snocau_100 " $ whnf (au_100  `AU.snoc`) 0,
        --bench "snocau_1k  " $ whnf (au_1k   `AU.snoc`) 500,
        --bench "snocau_10k " $ whnf (au_10k  `AU.snoc`) 500,
        --bench "snocau_100k" $ whnf (au_100k `AU.snoc`) 50000,
        --bench "snocau_1m  " $ whnf (au_1m   `AU.snoc`) 500000,
        --bench "snocau_10m " $ whnf (au_10m  `AU.snoc`) 5000000,

        --bench "bounddscheck"  $ nf (\i -> i >= 0 && i <= 1000) 5,
        --bench "bounddscheckprim"  $ nf (\(I# i) -> I# (andI# (i >=# 0#) (i <# 1000#))) 5,

        --bench "bounddscheck"      $ nf (\i -> i < (1000 ::  Word)) 5,
        --bench "bounddscheckprim"  $ nf (\(W# i) -> I# (leWord# i 1000##)) (5 :: Word) 


        --bench "snoca32_10  " $ whnf (A32.snoc a32_10  ) 5,
        --bench "snoca32_100 " $ whnf (A32.snoc a32_100 ) 50,
        --bench "snoca32_1k  " $ whnf (A32.snoc a32_1k  ) 500,
        --bench "snoca32_10k " $ whnf (A32.snoc a32_10k ) 5000,
        --bench "snoca32_100k" $ whnf (A32.snoc a32_100k) 50000,
        --bench "snoca32_1m  " $ whnf (A32.snoc a32_1m  ) 500000,
        --bench "snoca32_10m " $ whnf (A32.snoc a32_10m ) 5000000

        bench "snoca16_10  " $ whnf (snoca16 a16_10  ) 200,
        bench "snoca16_100 " $ whnf (snoca16 a16_100 ) 200,
        bench "snoca16_1k  " $ whnf (snoca16 a16_1k  ) 200,
        bench "snoca16_10k " $ whnf (snoca16 a16_10k ) 200,
        bench "snoca16_100k" $ whnf (snoca16 a16_100k) 200,
        bench "snoca16_1m  " $ whnf (snoca16 a16_1m  ) 200,
        bench "snoca16_10m " $ whnf (snoca16 a16_10m ) 200

        --bench "ixa32_10  " $ nf (a32_10   `A32.unsafeIndex`) 5,
        --bench "ixa32_100 " $ nf (a32_100  `A32.unsafeIndex`) 50,
        --bench "ixa32_1k  " $ nf (a32_1k   `A32.unsafeIndex`) 500,
        --bench "ixa32_10k " $ nf (a32_10k  `A32.unsafeIndex`) 5000,
        --bench "ixa32_100k" $ nf (a32_100k `A32.unsafeIndex`) 50000,
        --bench "ixa32_1m  " $ nf (a32_1m   `A32.unsafeIndex`) 500000,
        --bench "ixa32_10m " $ nf (a32_10m  `A32.unsafeIndex`) 5000000,

        --bench "inthash" $ nf hash (0::Int)

        --bench "ixvec_1k"      $ nf (v1k   V.!) 500,
        --bench "ixvec_10k"     $ nf (v10k  V.!) 500,
        --bench "ixvec_100k"    $ nf (v100k V.!) 50000,
        --bench "ixvec_1m"      $ nf (v1m   V.!) 500000

        --bench "ixseq_10  " $ nf (seq10   `Seq.index`) 0,
        --bench "ixseq_100 " $ nf (seq100  `Seq.index`) 0,
        --bench "ixseq_1k  " $ nf (seq1k   `Seq.index`) 0,
        --bench "ixseq_10k " $ nf (seq10k  `Seq.index`) 0,
        --bench "ixseq_100k" $ nf (seq100k `Seq.index`) 50000,
        --bench "ixseq_1m  " $ nf (seq1m   `Seq.index`) 500000,
        --bench "ixseq_10m"  $ nf (seq10m  `Seq.index`) 5000000,

        --bench "inshm_100k"  $ whnf (\v -> HM.insert 50000 v hm100k ) 50000,
        --bench "inshm_1m"    $ whnf (\v -> HM.insert 50000 v hm1m   ) 50000,
        --bench "inshm_10m"   $ whnf (\v -> HM.insert 50000 v hm10m  ) 50000

        --bench "insim_100k"  $ whnf (\v -> IM.insert 50000 v im100k ) 50000,
        --bench "insim_1m"    $ whnf (\v -> IM.insert 50000 v im1m   ) 50000,
        --bench "insim_10m"   $ whnf (\v -> IM.insert 50000 v im10m  ) 50000,

        --bench "ixim_100k"  $ whnf (im100k IM.!) 50000,
        --bench "ixim_1m"    $ whnf (im1m   IM.!) 50000,
        --bench "ixim_10m"   $ whnf (im10m  IM.!) 50000,

        --bench "modifya16_10  " $ whnf (A16.modify a16_10   0) succ,
        --bench "modifya16_100 " $ whnf (A16.modify a16_100  0) succ,
        --bench "modifya16_1k  " $ whnf (A16.modify a16_1k   0) succ,
        --bench "modifya16_10k " $ whnf (A16.modify a16_10k  0) succ,
        --bench "modifya16_100k" $ whnf (A16.modify a16_100k 0) succ,
        --bench "modifya16_1m  " $ whnf (A16.modify a16_1m   0) succ,
        --bench "modifya16_10m " $ whnf (A16.modify a16_10m  0) succ
  
        --bench "modifyau_10  " $ whnf (AU.modify au_10   0) succ,
        --bench "modifyau_100 " $ whnf (AU.modify au_100  0) succ,
        --bench "modifyau_1k  " $ whnf (AU.modify au_1k   0) succ,
        --bench "modifyau_10k " $ whnf (AU.modify au_10k  0) succ,
        --bench "modifyau_100k" $ whnf (AU.modify au_100k 0) succ,
        --bench "modifyau_1m  " $ whnf (AU.modify au_1m   0) succ,
        --bench "modifyau_10m " $ whnf (AU.modify au_10m  0) succ

        ]




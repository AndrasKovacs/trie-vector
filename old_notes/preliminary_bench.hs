{-# LANGUAGE MagicHash, UnboxedTuples #-}

import Criterion.Main
import Criterion.Config
import GHC.Exts
import GHC.ST

data Array a = Array (Array# a)

arrayToList :: Array a -> [a]
arrayToList (Array arr) = go 0# where
    size = sizeofArray# arr
    go i = case i >=# size of
        1# -> []
        _  -> case indexArray# arr i of (# a #) -> a : go (i +# 1#)

instance Show a => Show (Array a) where
    show = show . arrayToList

snoc :: Array a -> a -> Array a
snoc (Array arr) a = runSTRep go where
    size = sizeofArray# arr
    go s = case newArray# (size +# 1#) undefined s of
        (# s, marr #) -> case copyArray# arr 0# marr 0# size s of
            s -> case writeArray# marr size a s of
                s -> case unsafeFreezeArray# marr s of
                    (# s, arr #) -> (# s, Array arr #)

badSnoc :: Array a -> a -> Array a
badSnoc (Array arr) a = runSTRep go where
    size = sizeofArray# arr
    go s = case thawArray# arr 0# (size +# 1#) s of
        (# s, marr #) -> case writeArray# marr size a s of
            s -> case unsafeFreezeArray# marr s of
                (# s, arr #) -> (# s, Array arr #)

arrN :: Int# -> Array Int
arrN n = runSTRep $ \s -> 
    case newArray# n 0 s of
        (# s, marr #) -> case unsafeFreezeArray# marr s of
            (#s, arr #) -> (# s, Array arr #)

arr16 = arrN 16#
arr32 = arrN 32#
arr64 = arrN 64#
arr128 = arrN 128#

main = defaultMain  [
    bgroup "correct snoc" [
        bench "16"  $ whnf (snoc arr16) 0,
        bench "32"  $ whnf (snoc arr32) 0,
        bench "64"  $ whnf (snoc arr64) 0,
        bench "128" $ whnf (snoc arr128) 0],
    bgroup "bad snoc" [
        bench "16"  $ whnf (badSnoc arr16) 0,
        bench "32"  $ whnf (badSnoc arr32) 0,
        bench "64"  $ whnf (badSnoc arr64) 0,
        bench "128" $ whnf (badSnoc arr128) 0]]


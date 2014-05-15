{-# LANGUAGE MagicHash, UnboxedTuples, Rank2Types, BangPatterns #-}
{-# OPTIONS_GHC -fno-full-laziness #-}

module Array (
      run
    , update
    , modify
    , modify'
    , index
    , new
    , toList
    , fromList
    , init1
    , init2
    , Array.foldr
    , Array.foldl
    , Array.map 
    , foldl' ) where

import GHC.Base  (realWorld#)
import GHC.Prim (
    MutableArray#, Array#, State#, Int#, (<#), (+#), unsafeCoerce#,
    sizeofArray#, thawArray#, writeArray#, readArray#, indexArray#, newArray#,
    unsafeFreezeArray# )
import GHC.Types (Int(..))

run :: (forall s. State# s -> (# State# s, Array# a #)) -> Array# a
run strep = case strep realWorld# of
    (# _, arr #) -> arr
{-# INLINE [0] run #-}

update :: Int# -> Array# a -> Int# -> a -> Array# a
update size arr i a = run $ \s ->
    case thawArray# arr 0# size s of
        (# s, marr #) -> case writeArray# marr i a s of
            s -> unsafeFreezeArray# marr s
{-# INLINE update #-}

modify :: Int# -> Array# a -> Int# -> (a -> a) -> Array# a
modify size arr i f = run $ \s ->
    case thawArray# arr 0# size s of
        (# s, marr #) -> case readArray# marr i s of
            (# s, a #) -> case writeArray# marr i (f a) s of
                s -> unsafeFreezeArray# marr s
{-# INLINE modify #-}

modify' :: Int# -> Array# a -> Int# -> (a -> a) -> Array# a
modify' size arr i f = run $ \s ->
    case thawArray# arr 0# size s of
        (# s, marr #) -> case readArray# marr i s of
            (# s, a #) -> let !val = f a in case writeArray# marr i val s of
                s -> unsafeFreezeArray# marr s
{-# INLINE modify' #-}

map :: Int# -> (a -> b) ->  Array# a -> Array# b
map size f = \arr ->
    let go i marr size s = case i <# size of
            1# -> case writeArray# marr i (f (index arr i)) s of
                s -> go (i +# 1#) marr size s
            _  -> s
    in run $ \s ->
        case thawArray# arr 0# size s of
            (# s, marr #) -> case go 0# (unsafeCoerce# marr) size s of
                s -> unsafeFreezeArray# (unsafeCoerce# marr) s
{-# INLINE map #-}

index :: Array# a -> Int# -> a
index arr i = case indexArray# arr i of 
    (# a #) -> a
{-# INLINE index #-} 

new :: Int# -> a -> Array# a
new n a = run $ \s -> case newArray# n a s of
    (# s, marr #) -> unsafeFreezeArray# marr s
{-# INLINE new #-}

foldr :: Int# -> (a -> b -> b) -> b -> Array# a -> b
foldr size f = \z arr -> go 0# size z arr where
    go i s z arr = case i <# s of
        1# -> f (index arr i) (go (i +# 1#) s z arr)
        _  -> z 
{-# INLINE foldr #-}

foldl' :: Int# -> (b -> a -> b) -> b -> Array# a -> b
foldl' size f = \z arr -> go 0# size z arr  where
    go i s !z arr = case i <# s of
        1# -> go (i +# 1#) s (f z (index arr i)) arr
        _  -> z
{-# INLINE foldl' #-}

foldl :: Int# -> (b -> a -> b) -> b -> Array# a -> b
foldl size f = \z arr -> go 0# size z arr  where
    go i s z arr = case i <# s of
        1# -> go (i +# 1#) s (f z (index arr i)) arr
        _  -> z
{-# INLINE foldl #-}
 
fromList :: Int# -> [a] -> Array# a
fromList size xs = run $ \s -> 
    case newArray# size undefined s of
        (# s, marr #) -> go xs 0# s where
            go (x:xs) i s = case writeArray# marr i x s of s -> go xs (i +# 1#) s
            go _      _ s = unsafeFreezeArray# marr s 
{-# INLINE fromList #-}

toList :: Array# a -> [a]
toList arr = Array.foldr (sizeofArray# arr) (:) [] arr
{-# INLINE toList #-}

init1 :: Int# -> a -> a -> Array# a
init1 size a def = run $ \s ->
    case newArray# size def s of
        (# s, marr #) -> case writeArray# marr 0# a s of
            s -> unsafeFreezeArray# marr s
{-# INLINE init1 #-}

init2 :: Int# -> a -> a -> a -> Array# a
init2 size a1 a2 def = run $ \s ->
    case newArray# size def s of
        (# s, marr #) -> case writeArray# marr 0# a1 s of
            s -> case writeArray# marr 1# a2 s of
                s -> unsafeFreezeArray# marr s
{-# INLINE init2 #-}


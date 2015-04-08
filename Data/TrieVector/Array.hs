{-# LANGUAGE MagicHash, UnboxedTuples, Rank2Types, BangPatterns, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-full-laziness #-}

module Data.TrieVector.Array (
      A.run
    , A.sizeof
    , update
    , modify
    , modify'
    , index
    , new
    , toList
    , fromList
    , init1
    , init2
    , foldr
    , map 
    , foldl'
    , rfoldl'
    , rfoldr
    , Array
    , MArray
    ) where

import qualified Data.TrieVector.ArrayPrimWrap as A

import Prelude hiding (foldr, map)
import GHC.Prim 
import GHC.Types

type Array a = A.Array a
type MArray s a = A.MArray s a

update :: Int# -> Array a -> Int# -> a -> Array a
update size arr i a = A.run $ \s ->
    case A.thaw arr 0# size s of
        (# s, marr #) -> case A.write marr i a s of
            s -> A.unsafeFreeze marr s
{-# INLINE update #-}

modify :: Int# -> Array a -> Int# -> (a -> a) -> Array a
modify size arr i f = A.run $ \s ->
    case A.thaw arr 0# size s of
        (# s, marr #) -> case A.read marr i s of
            (# s, a #) -> case A.write marr i (f a) s of
                s -> A.unsafeFreeze marr s
{-# INLINE modify #-}

modify' :: Int# -> Array a -> Int# -> (a -> a) -> Array a
modify' size arr i f = A.run $ \s ->
    case A.thaw arr 0# size s of
        (# s, marr #) -> case A.read marr i s of
            (# s, a #) -> let !val = f a in case A.write marr i val s of
                s -> A.unsafeFreeze marr s
{-# INLINE modify' #-}

map :: forall a b. Int# -> (a -> b) ->  Array a -> Array b
map size f = \arr ->
    let go :: Int# -> MArray s b -> Int# -> State# s -> State# s
        go i marr size s = case i <# size of
            1# -> case A.write marr i (f (index arr i)) s of
                s -> go (i +# 1#) marr size s
            _  -> s
    in A.run $ \s ->
        case A.new size undefined s of
            (# s, marr #) -> case go 0# marr size s of
                s -> A.unsafeFreeze marr s
{-# INLINE map #-}

index :: Array a -> Int# -> a
index arr i = case A.index arr i of 
    (# a #) -> a
{-# INLINE index #-} 

new :: Int# -> a -> Array a
new n a = A.run $ \s -> case A.new n a s of
    (# s, marr #) -> A.unsafeFreeze marr s
{-# INLINE new #-}

foldr :: Int# -> (a -> b -> b) -> b -> Array a -> b
foldr size f = \z arr -> go 0# size z arr where
    go i s z arr = case i <# s of
        1# -> f (index arr i) (go (i +# 1#) s z arr)
        _  -> z 
{-# INLINE foldr #-}

rfoldr :: Int# -> (a -> b -> b) -> b -> Array a -> b
rfoldr size f = \z arr -> go (size -# 1#) z arr where
    go i z arr = case i >=# 0# of 
        1# -> f (index arr i) (go (i -# 1#) z arr)
        _  -> z 
{-# INLINE rfoldr #-}

foldl' :: Int# -> (b -> a -> b) -> b -> Array a -> b
foldl' size f = \z arr -> go 0# size z arr  where
    go i s !z arr = case i <# s of
        1# -> go (i +# 1#) s (f z (index arr i)) arr
        _  -> z
{-# INLINE foldl' #-}

rfoldl' :: Int# -> (b -> a -> b) -> b -> Array a -> b
rfoldl' size f = \z arr -> go (size -# 1#) z arr where
    go i !z arr = case i >=# 0# of
        1# -> go (i -# 1#) (f z (index arr i)) arr
        _  -> z
{-# INLINE rfoldl' #-}
 
fromList :: Int# -> [a] -> Array a
fromList size xs = A.run $ \s -> 
    case A.new size undefined s of
        (# s, marr #) -> go xs 0# s where
            go (x:xs) i s = case A.write marr i x s of s -> go xs (i +# 1#) s
            go _      _ s = A.unsafeFreeze marr s 
{-# INLINE fromList #-}

toList :: Array a -> [a]
toList arr = foldr (A.sizeof arr) (:) [] arr
{-# INLINE toList #-}

init1 :: Int# -> a -> a -> Array a
init1 size a def = A.run $ \s ->
    case A.new size def s of
        (# s, marr #) -> case A.write marr 0# a s of
            s -> A.unsafeFreeze marr s
{-# INLINE init1 #-}

init2 :: Int# -> a -> a -> a -> Array a
init2 size a1 a2 def = A.run $ \s ->
    case A.new size def s of
        (# s, marr #) -> case A.write marr 0# a1 s of
            s -> case A.write marr 1# a2 s of
                s -> A.unsafeFreeze marr s
{-# INLINE init2 #-}

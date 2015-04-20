{-# LANGUAGE MagicHash, UnboxedTuples, Rank2Types, BangPatterns, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-warn-name-shadowing #-}

-- Note : all indexing and sizes are relative to the sizes of the element types.
-- except for "thaw', which expects size in bytes

-- Exported functions using the functions below should be INLINABLE
-- to enable specialization of the Prim class. 

module Data.TrieVector.ByteArray (
      run
    , thaw
    , modify'
    , map
    , index
    , foldr
    , foldl'
    , rfoldl'
    , rfoldr
    , fromList
    , fromList'
    , toList
    , update
    , new
    , init1
    , init2
    , ByteArray
    ) where

import Prelude hiding (foldr, map)
import Data.Primitive.Types
import GHC.Prim 


type ByteArray = ByteArray#

run :: (forall s. State# s -> (# State# s, ByteArray# #)) -> ByteArray#
run strep = case strep realWorld# of
    (# _, arr #) -> arr
{-# INLINE [0] run #-}

-- Size in bytes here!
thaw :: Int# -> ByteArray# -> State# s -> (# State# s, MutableByteArray# s #)
thaw size arr s =
    case newByteArray# size s of
        (# s, marr #) -> case copyByteArray# arr 0# marr 0# size s of
            s -> (# s, marr #)

update :: forall a. Prim a => Int# -> ByteArray# -> Int# -> a -> ByteArray#
update size arr i a = run $ \s ->
    case thaw (size *# sizeOf# (undefined :: a)) arr s of 
        (# s, marr #) -> case writeByteArray# marr i a s of
            s -> unsafeFreezeByteArray# marr s 
{-# INLINE update #-}

modify' :: forall a. Prim a => Int# -> ByteArray# -> Int# -> (a -> a) -> ByteArray#
modify' size arr i f = run $ \s ->
    case thaw (size *# sizeOf# (undefined :: a)) arr s of
        (# s, marr #) -> case writeByteArray# marr i (f (index arr i)) s of
            s -> unsafeFreezeByteArray# marr s 
{-# INLINE modify' #-}

map :: forall a b. (Prim a, Prim b) => Int# -> (a -> b) -> ByteArray# -> ByteArray#
map size f = \arr ->
    let go :: Int# -> MutableByteArray# s -> Int# -> State# s -> State# s
        go i marr size s = case i <# size of
            1# -> case writeByteArray# marr i (f (index arr i)) s of
                s -> go (i +# 1#) marr size s
            _  -> s
    in run $ \s ->
        case newByteArray# (size *# (sizeOf# (undefined :: b))) s of 
            (# s, marr #) -> case go 0# marr size s of
                s -> unsafeFreezeByteArray# marr s
{-# INLINE map #-}

new :: Int# -> ByteArray#
new n = run $ \s -> case newByteArray# n s of
    (# s, marr #) -> unsafeFreezeByteArray# marr s
{-# INLINE new #-}

index :: Prim a => ByteArray# -> Int# -> a
index = indexByteArray#  
{-# INLINE index #-} 

foldr :: forall a b. Prim a => Int# -> (a -> b -> b) -> b -> ByteArray# -> b
foldr size f = \z arr -> go 0# size z arr where
    go i s z arr = case i <# s of
        1# -> f (index arr i :: a) (go (i +# 1#) s z arr)
        _  -> z 
{-# INLINE foldr #-}

rfoldr :: Prim a => Int# -> (a -> b -> b) -> b -> ByteArray# -> b
rfoldr size f = \z arr -> go (size -# 1#) z arr where
    go i z arr = case i >=# 0# of 
        1# -> f (index arr i) (go (i -# 1#) z arr)
        _  -> z 
{-# INLINE rfoldr #-}

foldl' :: Prim a => Int# -> (b -> a -> b) -> b -> ByteArray# -> b
foldl' size f = \z arr -> go 0# size z arr  where
    go i s !z arr = case i <# s of
        1# -> go (i +# 1#) s (f z (index arr i)) arr
        _  -> z
{-# INLINE foldl' #-}

rfoldl' :: Prim a => Int# -> (b -> a -> b) -> b -> ByteArray# -> b
rfoldl' size f = \z arr -> go (size -# 1#) z arr where
    go i !z arr = case i >=# 0# of
        1# -> go (i -# 1#) (f z (index arr i)) arr
        _  -> z
{-# INLINE rfoldl' #-}

fromList :: forall a. Prim a => Int# -> [a] -> ByteArray#
fromList size xs = run $ \s ->
    case newByteArray# (size *# sizeOf# (undefined :: a)) s of
        (# s, marr #) -> go xs 0# s where
            go (x:xs) i s = case writeByteArray# marr i x s of s -> go xs (i +# 1#) s
            go _      _ s = unsafeFreezeByteArray# marr s 
{-# INLINE fromList #-}

runFromList' :: Prim a =>
  (forall s. State# s -> (# State# s, ByteArray, [a], Int# #)) -> (# ByteArray, [a], Int# #)
runFromList' strep = case strep realWorld# of
  (# s, arr, xs, read #) -> (# arr, xs, read #)
{-# INLINE [0] runFromList' #-}

-- | Returns: ByteArray, rest of the input list, number of elems consumed
fromList' :: forall a. Prim a => Int# -> [a] -> (# ByteArray, [a], Int# #)
fromList' size xs = runFromList' $ \s ->
  case newByteArray# (size *# sizeOf# (undefined :: a)) s of
    (# s, marr #) -> case go xs 0# s of
      (# s, xs, read #) -> case unsafeFreezeByteArray# marr s of
        (# s, arr #) -> (# s, arr, xs, read #)
      where
        go xs i s = case i ==# size of
          1# -> (# s, xs, i #)
          _  -> case xs of
            x:xs -> case writeByteArray# marr i x s of s -> go xs (i +# 1#) s
            []   -> (# s, [], i #)
{-# INLINE fromList' #-}  

toList :: forall a. Prim a => ByteArray# -> [a]
toList arr = foldr (quotInt# (sizeofByteArray# arr) (sizeOf# (undefined :: a))) (:) [] arr
{-# INLINE toList #-}

init1 :: forall a. Prim a => Int# -> a -> ByteArray#
init1 size a = run $ \s ->
    case newByteArray# (size *# sizeOf# (undefined :: a)) s of
        (# s, marr #) -> case writeByteArray# marr 0# a s of
            s -> unsafeFreezeByteArray# marr s
{-# INLINE init1 #-}

init2 :: forall a. Prim a => Int# -> a -> a -> ByteArray#
init2 size a b = run $ \s ->
    case newByteArray# (size *# sizeOf# (undefined :: a)) s of
        (# s, marr #) -> case writeByteArray# marr 0# a s of
            s -> case writeByteArray# marr 1# b s of
                s -> unsafeFreezeByteArray# marr s
{-# INLINE init2 #-}

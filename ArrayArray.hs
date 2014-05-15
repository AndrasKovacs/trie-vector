
{-# LANGUAGE MagicHash, UnboxedTuples, Rank2Types, BangPatterns #-}
{-# OPTIONS_GHC -fno-full-laziness #-}

module ArrayArray (
      run
    , update
    , modify
    , modify'
    , map'
    , mapInitLast'
    , index
    , new
    , init1
    , init2
    , ArrayArray.foldl
    , ArrayArray.foldr 
    , foldl' ) where

import qualified Array as A
import GHC.Prim 

-- QUESTION : Does this mess up the inlining of the runSTRep tricK? (So far so good).
run :: (forall s. State# s -> (# State# s, ArrayArray# #)) -> ArrayArray#
run = unsafeCoerce# A.run
{-# INLINE run #-}

-- ThawArray should be preferably given a statically known length parameter,
-- hence the length param in every function that calls it. 
update :: Int# -> ArrayArray# -> Int# -> ArrayArray# -> ArrayArray#
update size arr i a  = run $ \s ->
    case thawArray# (unsafeCoerce# arr :: Array# Any) 0# size s of
        (# s, marr #) -> case writeArrayArrayArray# (unsafeCoerce# marr) i a s of
            s -> unsafeFreezeArrayArray# (unsafeCoerce# marr) s
{-# INLINE update #-}

modify :: Int# -> ArrayArray# -> Int# -> (ArrayArray# -> ArrayArray#) -> ArrayArray#
modify size arr i f  = run $ \s ->
    case thawArray# (unsafeCoerce# arr :: Array# Any) 0# size s of
        (# s, marr #) -> case readArrayArrayArray# (unsafeCoerce# marr) i s of
            (# s, a #) -> case writeArrayArrayArray# (unsafeCoerce# marr) i (f a) s of
                s -> unsafeFreezeArrayArray# (unsafeCoerce# marr) s
{-# INLINE modify #-}

modify' :: Int# -> ArrayArray# -> Int# -> (ArrayArray# -> ArrayArray#) -> ArrayArray#
modify' size arr i f = run $ \s ->
    case thawArray# (unsafeCoerce# arr :: Array# Any) 0# size s of
        (# s, marr #) -> case readArrayArrayArray# (unsafeCoerce# marr) i s of
            (# s, a #) -> let !val = f a in case writeArrayArrayArray# (unsafeCoerce# marr) i val s of
                s -> unsafeFreezeArrayArray# (unsafeCoerce# marr) s
{-# INLINE modify' #-}

map :: Int# -> (ArrayArray# -> ArrayArray#) ->  ArrayArray# -> ArrayArray#
map size f = \arr ->
    let go i marr size s = case i <# size of
            1# -> case writeArrayArrayArray# marr i (f (index arr i)) s of
                s -> go (i +# 1#) marr size s
            _  -> s
    in run $ \s ->
        case thawArray# (unsafeCoerce# arr :: Array# Any) 0# size s of
            (# s, marr #) -> case go 0# (unsafeCoerce# marr) size s of
                s -> unsafeFreezeArrayArray# (unsafeCoerce# marr) s
{-# INLINE map #-}

map' :: Int# -> (ArrayArray# -> ArrayArray#) ->  ArrayArray# -> ArrayArray#
map' size f = \arr ->
    let go i marr size s = case i <# size of
            1# -> let !val = f (index arr i) in case writeArrayArrayArray# marr i val s of
                s -> go (i +# 1#) marr size s
            _  -> s
    in run $ \s ->
        case thawArray# (unsafeCoerce# arr :: Array# Any) 0# size s of
            (# s, marr #) -> case go 0# (unsafeCoerce# marr) size s of
                s -> unsafeFreezeArrayArray# (unsafeCoerce# marr) s
{-# INLINE map' #-}

mapInitLast' :: Int# -> (ArrayArray# -> ArrayArray#) -> (ArrayArray# -> ArrayArray#) -> ArrayArray# -> ArrayArray#
mapInitLast' lasti f g = \arr ->
    let go i marr lasti s = case i <# lasti of
            1# -> let !val = f (index arr i) in case writeArrayArrayArray# marr i val s of
                s -> go (i +# 1#) marr lasti s
            _  -> let !val = g (index arr lasti) in writeArrayArrayArray# marr lasti val s
    in run $ \s ->
        case thawArray# (unsafeCoerce# arr :: Array# Any) 0# (lasti +# 1#) s of
            (# s, marr #) -> case go 0# (unsafeCoerce# marr) lasti s of
                s -> unsafeFreezeArrayArray# (unsafeCoerce# marr) s
{-# INLINE mapInitLast' #-}

foldr :: Int# -> (ArrayArray# -> b -> b) -> b -> ArrayArray# -> b
foldr size f = \z arr -> go 0# size z arr where
    go i s z arr = case i <# s of
        1# -> f (index arr i) (go (i +# 1#) s z arr)
        _  -> z 
{-# INLINE foldr #-}

foldl :: Int# -> (b -> ArrayArray# -> b) -> b -> ArrayArray# -> b
foldl size f = \z arr -> go 0# size z arr  where
    go i s z arr = case i <# s of
        1# -> go (i +# 1#) s (f z (index arr i)) arr
        _  -> z
{-# INLINE foldl #-}

foldl' :: Int# -> (b -> ArrayArray# -> b) -> b -> ArrayArray# -> b
foldl' size f = \z arr -> go 0# size z arr  where
    go i s !z arr = case i <# s of
        1# -> go (i +# 1#) s (f z (index arr i)) arr
        _  -> z
{-# INLINE foldl' #-}

index :: ArrayArray# -> Int# -> ArrayArray#
index = indexArrayArrayArray#
{-# INLINE index #-} 

new :: Int# -> ArrayArray#
new n = run $ \s -> case newArrayArray# n s of
    (# s, marr #) -> unsafeFreezeArrayArray# marr s
{-# INLINE new #-}

-- | Create a new array with a given element in the front and the rest filled with a default element. 
init1 :: Int# -> ArrayArray# -> a -> ArrayArray#
init1 size a def = run $ \s ->
    case newArray# size def s of
        (# s, marr #) -> case writeArrayArrayArray# (unsafeCoerce# marr) 0# a s of
            s -> unsafeFreezeArrayArray# (unsafeCoerce# marr) s
{-# INLINE init1 #-}

-- | Create a new array with a two given elements in the front and the rest filled with a default element. 
init2 :: Int# -> ArrayArray# -> ArrayArray# -> a -> ArrayArray#
init2 size a1 a2 def = run $ \s ->
    case newArray# size def s of
        (# s, marr #) -> case writeArrayArrayArray# (unsafeCoerce# marr) 0# a1 s of
            s -> case writeArrayArrayArray# (unsafeCoerce# marr) 1# a2 s of
                s -> unsafeFreezeArrayArray# (unsafeCoerce# marr) s
{-# INLINE init2 #-}
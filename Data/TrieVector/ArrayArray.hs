
{-# LANGUAGE MagicHash, UnboxedTuples, Rank2Types, BangPatterns, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-warn-name-shadowing #-}

module Data.TrieVector.ArrayArray (
      A.run
    , A.sizeof
    , A.thaw
    , A.unsafeThaw
    , A.write
    , ptrEq
    , update
    , modify
    , noCopyModify
    , index
    , new
    , newM
    , init1
    , init2
    , foldr
    , map
    , mapInitLast
    , foldl'
    , rfoldl'
    , rfoldr
    , read
    , AArray
    , MAArray
    ) where

import Prelude hiding (foldr, read, map)
import GHC.Prim

import qualified Data.TrieVector.ArrayPrimWrap as A

type AArray = A.Array Any
type MAArray s = A.MArray s Any

-- ThawArray should be preferably given a statically known length parameter,
-- hence the length param in every function that calls it. 
update :: Int# -> AArray -> Int# -> AArray -> AArray
update size arr i a  = A.run $ \s ->
    case A.thaw arr 0# size s of
        (# s, marr #) -> case write marr i a s of
            s -> A.unsafeFreeze marr s
{-# INLINE update #-}

ptrEq :: AArray -> AArray -> Int#
ptrEq a b = reallyUnsafePtrEquality#
  (unsafeCoerce# a :: Any) (unsafeCoerce# b :: Any)
{-# INLINE ptrEq #-}  

modify :: Int# -> AArray -> Int# -> (AArray -> AArray) -> AArray
modify size arr i f = A.run $ \s ->
    case A.thaw arr 0# size s of
        (# s, marr #) -> case read marr i s of
            (# s, a #) -> case write marr i (f a) s of
                s -> A.unsafeFreeze marr s
{-# INLINE modify #-}

noCopyModify :: Int# -> AArray -> Int# -> (AArray -> AArray) -> AArray
noCopyModify size arr i f = let
  a  = index arr i
  a' = f a
  in case reallyUnsafePtrEquality# (unsafeCoerce# a :: Any) (unsafeCoerce# a' :: Any) of
      1# -> arr
      _  -> update size arr i a'
{-# INLINE noCopyModify #-}      

map :: Int# -> (AArray -> AArray) -> AArray -> AArray
map size f = \arr ->
    let go :: Int# -> MAArray s -> Int# -> State# s -> State# s
        go i marr size s = case i <# size of
            1# -> case write marr i (f (index arr i)) s of
                s -> go (i +# 1#) marr size s
            _  -> s
    in A.run $ \s ->
        case newM size arr s of
            (# s, marr #) -> case go 0# marr size s of
                s -> A.unsafeFreeze marr s
{-# INLINE map #-}

mapInitLast :: Int# -> (AArray -> AArray) -> (AArray -> AArray) -> AArray -> AArray
mapInitLast lasti f g = \arr ->
    let go :: Int# -> MAArray s -> Int# -> State# s -> State# s 
        go i marr lasti s = case i <# lasti of
            1# -> case write marr i (f (index arr i)) s of
                s -> go (i +# 1#) marr lasti s
            _  -> write marr lasti (g (index arr lasti)) s
    in A.run $ \s ->
        case newM (lasti +# 1#) arr s of
            (# s, marr #) -> case go 0# marr lasti s of
                s -> A.unsafeFreeze marr s
{-# INLINE mapInitLast #-}

foldr :: Int# -> (AArray -> b -> b) -> b -> AArray -> b
foldr size f = \z arr -> go 0# size z arr where
    go i s z arr = case i <# s of
        1# -> f (index arr i) (go (i +# 1#) s z arr)
        _  -> z 
{-# INLINE foldr #-}

rfoldr :: Int# -> (AArray -> b -> b) -> b -> AArray -> b
rfoldr size f = \z arr -> go (size -# 1#) z arr where
    go i z arr = case i >=# 0# of 
        1# -> f (index arr i) (go (i -# 1#) z arr)
        _  -> z 
{-# INLINE rfoldr #-}

foldl' :: Int# -> (b -> AArray -> b) -> b -> AArray -> b
foldl' size f = \z arr -> go 0# size z arr  where
    go i s !z arr = case i <# s of
        1# -> go (i +# 1#) s (f z (index arr i)) arr
        _  -> z
{-# INLINE foldl' #-}

rfoldl' :: Int# -> (b -> AArray -> b) -> b -> AArray -> b
rfoldl' size f = \z arr -> go (size -# 1#) z arr where
    go i !z arr = case i >=# 0# of
        1# -> go (i -# 1#) (f z (index arr i)) arr
        _  -> z
{-# INLINE rfoldl' #-}

index :: AArray -> Int# -> AArray
index arr i = case A.index arr i of
  (# a #) -> unsafeCoerce# a
{-# INLINE index #-}

read :: MAArray s -> Int# -> State# s -> (# State# s, AArray #)
read marr i s = unsafeCoerce# (A.read marr i s)
{-# INLINE read #-}

write :: MAArray s -> Int# -> AArray -> State# s -> State# s
write marr i a s = A.write marr i (unsafeCoerce# a) s
{-# INLINE write #-}

new :: Int# -> a -> AArray
new size def = A.run $ \s ->
  case A.new size def s of
    (# s, marr #) -> A.unsafeFreeze (unsafeCoerce# marr) s
{-# INLINE new #-}    

newM :: Int# -> AArray -> State# s -> (# State# s, MAArray s #)
newM size def s = A.new size (unsafeCoerce# def) s
{-# INLINE newM #-}

-- | Create a new array with a given element in the front and the rest filled with a default element.
-- Here we allow an "a" default parameter mainly for error-throwing undefined elements.
init1 :: Int# -> AArray -> AArray -> AArray
init1 size arr def = A.run $ \s ->
    case newM size (unsafeCoerce# def) s of
        (# s, marr #) -> case write marr 0# arr s of
            s -> A.unsafeFreeze marr s
{-# INLINE init1 #-}

-- | Create a new array with a two given elements in the front and the rest filled with a default element.
-- Here we allow an "a" default parameter mainly for error-throwing undefined elements.
init2 :: Int# -> AArray -> AArray -> AArray -> AArray
init2 size a1 a2 def = A.run $ \s ->
    case newM size (unsafeCoerce# def) s of
        (# s, marr #) -> case write marr 0# a1 s of
            s -> case write marr 1# a2 s of
                s -> A.unsafeFreeze marr s
{-# INLINE init2 #-}

{-# LANGUAGE MagicHash, UnboxedTuples, Rank2Types, BangPatterns, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-warn-name-shadowing #-}

module Data.TrieVector.Array (
      Array
    , MArray
    , A.run
    , A.read
    , A.sizeof
    , A.thaw
    , A.unsafeFreeze
    , A.unsafeThaw
    , A.write
    , foldl'
    , foldr
    , fromList
    , fromList'
    , index
    , init1
    , init2
    , map
    , modify
    , modifyTrim
    , modify'
    , modifyTrim'
    , new
    , noCopyModify'
    , noCopyModifyTrim'
    , pop
    , rfoldl'
    , rfoldr
    , snoc
    , toList
    , trim
    , update
    , updateTrim
    ) where

import qualified Data.TrieVector.ArrayPrimWrap as A

import Prelude hiding (foldr, map)
import GHC.Prim

type Array a = A.Array a
type MArray s a = A.MArray s a

update :: Int# -> Array a -> Int# -> a -> Array a
update size arr i a = A.run $ \s ->
    case A.thaw arr 0# size s of
        (# s, marr #) -> case A.write marr i a s of
            s -> A.unsafeFreeze marr s
{-# INLINE update #-}

trimSize :: Int# -> Int# -> Int# -> Int#
trimSize small large actual = case actual <=# small of
  1# -> actual
  _  -> large
{-# INLINE trimSize #-}

updateTrim :: Int# -> Int# -> Int# -> Array a -> Int# -> a -> Array a
updateTrim small large elems arr i a = A.run $ \s ->
  case A.thaw arr 0# (trimSize small large elems) s of
    (# s, marr #) -> case A.write marr i a s of
      s -> A.unsafeFreeze marr s
{-# INLINE updateTrim #-}

trim :: Int# -> Int# -> Array a -> Array a
trim small size arr = case size <=# small of
  1# -> A.clone arr 0# size
  _  -> arr
{-# INLINE trim #-}

-- | Optimized snoccing. Invariant: if "size" > "small", then the
--   size of the array is "large". If "size" <= "small", then the
--   size of the array is trimmed to the number of elements.
snoc :: Int# -> Int# -> Int# -> a -> Array a -> a -> Array a
snoc small large elems undefElem arr a = case elems <# small of
  1# -> A.snoc arr a
  _  -> case elems ==# small of
    1# -> A.snocWithPadding (large -# (elems +# 1#)) undefElem arr a
    _  -> update large arr elems a
{-# INLINE snoc #-}

-- | Pop with the same invariant described above in "snoc".
pop :: Int# -> Int# -> Int# -> a -> Array a -> Array a
pop small large elems undefElem arr = case elems <=# (small +# 1#) of
  1# -> A.clone arr 0# (elems -# 1#)
  _  -> update large arr (elems -# 1#) undefElem
{-# INLINE pop #-}

modify :: Int# -> Array a -> Int# -> (a -> a) -> Array a
modify size arr i f = A.run $ \s ->
    case A.thaw arr 0# size s of
        (# s, marr #) -> case A.read marr i s of
            (# s, a #) -> case A.write marr i (f a) s of
                s -> A.unsafeFreeze marr s
{-# INLINE modify #-}

modifyTrim :: Int# -> Int# -> Int# -> Array a -> Int# -> (a -> a) -> Array a
modifyTrim small large elems arr i f = A.run $ \s ->
    case A.thaw arr 0# (trimSize small large elems) s of
        (# s, marr #) -> case A.read marr i s of
            (# s, a #) -> case A.write marr i (f a) s of
                s -> A.unsafeFreeze marr s
{-# INLINE modifyTrim #-}

modify' :: Int# -> Array a -> Int# -> (a -> a) -> Array a
modify' size arr i f = A.run $ \s ->
    case A.thaw arr 0# size s of
        (# s, marr #) -> case A.read marr i s of
            (# s, a #) -> let !val = f a in case A.write marr i val s of
                s -> A.unsafeFreeze marr s
{-# INLINE modify' #-}

modifyTrim' :: Int# -> Int# -> Int# -> Array a -> Int# -> (a -> a) -> Array a
modifyTrim' small large elems arr i f = A.run $ \s ->
    case A.thaw arr 0# (trimSize small large elems) s of
        (# s, marr #) -> case A.read marr i s of
            (# s, a #) -> let !val = f a in case A.write marr i val s of
                s -> A.unsafeFreeze marr s
{-# INLINE modifyTrim' #-}

noCopyModify' :: Int# -> Array a -> Int# -> (a -> a) -> Array a
noCopyModify' size arr i f = let
  a   = index arr i
  !a' = f a
  in case reallyUnsafePtrEquality# a a' of
      1# -> arr
      _  -> update size arr i a'
{-# INLINE noCopyModify' #-}

noCopyModifyTrim' :: Int# -> Int# -> Int# -> Array a -> Int# -> (a -> a) -> Array a
noCopyModifyTrim' small large elems arr i f = let
  a   = index arr i
  !a' = f a
  in case reallyUnsafePtrEquality# a a' of
      1# -> arr
      _  -> updateTrim small large elems arr i a'
{-# INLINE noCopyModifyTrim' #-}

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

fromList :: Int# -> a -> [a] -> Array a
fromList size def xs = A.run $ \s ->
    case A.new size def s of
        (# s, marr #) -> go xs 0# s where
            go (x:xs) i s = case A.write marr i x s of s -> go xs (i +# 1#) s
            go _      _ s = A.unsafeFreeze marr s
{-# INLINE fromList #-}

runFromList' ::
  (forall s. State# s -> (# State# s, Array a, [a], Int# #)) -> (# Array a, [a], Int# #)
runFromList' strep = case strep realWorld# of
  (# _, arr, xs, read #) -> (# arr, xs, read #)
{-# INLINE [0] runFromList' #-}

-- | Returns: Array, rest of the input list, number of elems consumed
fromList' :: Int# -> a -> [a] -> (# Array a, [a], Int# #)
fromList' size def xs = runFromList' $ \s ->
  case A.new size def s of
    (# s, marr #) -> case go xs 0# s of
      (# s, xs, read #) -> case A.unsafeFreeze marr s of
        (# s, arr #) -> (# s, arr, xs, read #)
      where
        go xs i s = case i ==# size of
          1# -> (# s, xs, i #)
          _  -> case xs of
            x:xs -> case A.write marr i x s of s -> go xs (i +# 1#) s
            []   -> (# s, [], i #)
{-# INLINE fromList' #-}

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


{-# LANGUAGE MagicHash, UnboxedTuples, CPP, RankNTypes #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-warn-missing-signatures #-}

module Data.TrieVector.ArrayPrimWrap (
      Array
    , MArray
    , cas
    , clone
    , cloneMut
    , copy
    , copyMut
    , freeze
    , index
    , new
    , read
    , run
    , sameMut
    , sizeof
    , sizeofMut
    , snoc
    , snocWithPadding
    , thaw
    , unsafeFreeze
    , unsafeThaw
    , write
    ) where

import GHC.Prim

import Prelude hiding (read)

#if __GLASGOW_HASKELL__ >= 709
import GHC.Prim.SmallArray

type Array = SmallArray#
type MArray = SmallMutableArray#

cas = casSmallArray#
clone = cloneSmallArray#
cloneMut = cloneSmallMutableArray#
copy = copySmallArray#
copyMut = copySmallMutableArray#
freeze = freezeSmallArray#
index = indexSmallArray#
new = newSmallArray#
read = readSmallArray#
sameMut = sameSmallMutableArray#
sizeof = sizeofSmallArray#
sizeofMut = sizeofSmallMutableArray#
snoc = snocSmallArray#
snocWithPadding = snocSmallArrayWithPadding#
thaw = thawSmallArray#
unsafeFreeze = unsafeFreezeSmallArray#
unsafeThaw = unsafeThawSmallArray#
write = writeSmallArray#

#else
import GHC.Prim.Array

type Array = Array#
type MArray = MutableArray#

cas = casArray#p
clone = cloneArray#
cloneMut = cloneMutableArray#
copy = copyArray#
copyMut = copyMutableArray#
freeze = freezeArray#
index = indexArray#
new = newArray#
read = readArray#
sameMut = sameMutableArray#
sizeof = sizeofArray#
sizeofMut = sizeofMutableArray#
snoc = snocArray#
snocWithPadding = snocArrayWithPadding#
thaw = thawArray#
unsafeFreeze = unsafeFreezeArray#
unsafeThaw = unsafeThawArray#
write = writeArray#

#endif

run :: (forall s. State# s -> (# State# s, Array a #)) -> Array a
run strep = case strep realWorld# of
  (# _, arr #) -> arr
{-# INLINE [0] run #-}

{-# INLINE cas #-}
{-# INLINE clone #-}
{-# INLINE cloneMut #-}
{-# INLINE copy #-}
{-# INLINE copyMut #-}
{-# INLINE freeze #-}
{-# INLINE index #-}
{-# INLINE new #-}
{-# INLINE read #-}
{-# INLINE sameMut #-}
{-# INLINE sizeof #-}
{-# INLINE sizeofMut #-}
{-# INLINE thaw #-}
{-# INLINE unsafeFreeze #-}
{-# INLINE unsafeThaw #-}
{-# INLINE write #-}

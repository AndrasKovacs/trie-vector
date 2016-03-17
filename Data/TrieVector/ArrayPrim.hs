{-# LANGUAGE MagicHash, UnboxedTuples, CPP, RankNTypes #-}
{-# OPTIONS_GHC -fno-full-laziness #-}

module Data.TrieVector.ArrayPrim (
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
import GHC.Prim.Array
import GHC.Types 
import GHC.Base (realWorld#)

import Prelude hiding (read)

type Array = Array#
type MArray = MutableArray#

cas = casArray#
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

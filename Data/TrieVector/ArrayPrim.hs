{-# LANGUAGE MagicHash, UnboxedTuples, CPP, RankNTypes #-}
{-# OPTIONS_GHC -fno-full-laziness #-}

module Data.TrieVector.ArrayPrim (
      Array
    , MArray
    , new
    , write
    , read
    , thaw
    , index
    , freeze
    , unsafeFreeze
    , unsafeThaw
    , sizeof
    , sizeofMut
    , copy
    , copyMut
    , clone
    , cloneMut
    , cas
    , sameMut
    , run
    ) where

import GHC.Prim  
import GHC.Types 
import GHC.Base (realWorld#)

import Prelude hiding (read)

type Array = Array#
type MArray = MutableArray#

new = newArray#
write = writeArray#
read = readArray#
thaw = thawArray#
index = indexArray#
freeze = freezeArray#
unsafeFreeze = unsafeFreezeArray#
unsafeThaw = unsafeThawArray#
sizeof = sizeofArray#
sizeofMut = sizeofMutableArray#
copy = copyArray#
copyMut = copyMutableArray#
clone = cloneArray#
cloneMut = cloneMutableArray#
cas = casArray#
sameMut = sameMutableArray#

run :: (forall s. State# s -> (# State# s, Array a #)) -> Array a
run strep = case strep realWorld# of
  (# _, arr #) -> arr
{-# INLINE [0] run #-}

{-# INLINE new #-}
{-# INLINE write #-}
{-# INLINE read #-}
{-# INLINE thaw #-}
{-# INLINE index #-}
{-# INLINE freeze #-}
{-# INLINE unsafeFreeze #-}
{-# INLINE unsafeThaw #-}
{-# INLINE sizeof #-}
{-# INLINE sizeofMut #-}
{-# INLINE copy #-}
{-# INLINE copyMut #-}
{-# INLINE clone #-}
{-# INLINE cloneMut #-}
{-# INLINE cas #-}
{-# INLINE sameMut #-}

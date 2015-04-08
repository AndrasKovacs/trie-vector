{-# LANGUAGE MagicHash, UnboxedTuples, CPP, RankNTypes #-}
{-# OPTIONS_GHC -fno-full-laziness #-}

module Data.TrieVector.SmallArrayPrim (
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

type Array = SmallArray#
type MArray = SmallMutableArray#

new = newSmallArray#
write = writeSmallArray#
read = readSmallArray#
thaw = thawSmallArray#
index = indexSmallArray#
freeze = freezeSmallArray#
unsafeFreeze = unsafeFreezeSmallArray#
unsafeThaw = unsafeThawSmallArray#
sizeof = sizeofSmallArray#
sizeofMut = sizeofSmallMutableArray#
copy = copySmallArray#
copyMut = copySmallMutableArray#
clone = cloneSmallArray#
cloneMut = cloneSmallMutableArray#
cas = casSmallArray#
sameMut = sameSmallMutableArray#

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

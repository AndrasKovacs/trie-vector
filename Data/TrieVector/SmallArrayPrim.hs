{-# LANGUAGE MagicHash, UnboxedTuples, CPP, RankNTypes #-}
{-# OPTIONS_GHC -fno-full-laziness #-}

module Data.TrieVector.SmallArrayPrim (
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
import GHC.Prim.SmallArray
import GHC.Types 
import GHC.Base (realWorld#)

import Prelude hiding (read)

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


{-# LANGUAGE
  MagicHash, BangPatterns, UnboxedTuples,
  RoleAnnotations, CPP, RankNTypes, ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-full-laziness #-}

module Data.TrieVector.Unboxed (
      Vector(..)
    , (|>)
    , (!#)
    , unsafeIndex#
    , (!)
    , unsafeIndex
    , snoc
    , Data.TrieVector.Unboxed.foldr
    , Data.TrieVector.Unboxed.foldl'
    , rfoldr
    , rfoldl'
    , Data.TrieVector.Unboxed.map
    , modify
    , unsafeModify
    , modify#
    , unsafeModify#
    , singleton
    , empty
    , Data.TrieVector.Unboxed.length
    , toList
    , fromList 
    ) where


import GHC.Prim
import GHC.Types
import Data.Primitive.Types

import Data.TrieVector.ByteArray (ByteArray)
import qualified Data.TrieVector.ByteArray as A

import Data.TrieVector.ArrayArray (AArray)
import qualified Data.TrieVector.ArrayArray as AA

import Data.List

#define NODE_WIDTH 16#
#define KEY_BITS 4#
#define KEY_MASK 15#


type role Vector nominal

data Vector a = Vector {
    _size, _level :: Int#,
    _init :: AArray,
    _tail :: ByteArray}

instance (Show a, Prim a) => Show (Vector a) where
    show seq = "fromList " ++ show (toList seq)    

(|>) :: Prim a => Vector a -> a -> Vector a
(|>) = snoc
infixl 5 |>
{-# INLINABLE (|>) #-}

(!) :: forall a. Prim a => Vector a -> Int -> a
(!) v (I# i) = v !# i
{-# INLINABLE (!) #-}

(!#) :: forall a. Prim a => Vector a -> Int# -> a
(!#) (Vector size level arr tail) i = case i >=# 0# of 
    1# -> let
        tailSize = andI# size KEY_MASK
        initSize = size -# tailSize
        in case i <# initSize of
            1# -> go i level arr
            _  -> case i <# size of
                1# -> A.index tail (i -# initSize)
                _  -> error "Vector.!: out of bounds"
    _  -> error "Vector.!: out of bounds"
    where go i level arr = case level ># 0# of
            1# -> go i (next level) (AA.index arr (index i level))
            _  -> A.index (aa2ba arr) (index i level)
infixl 5 !
{-# INLINABLE (!#) #-}

unsafeIndex :: Prim a => Vector a -> Int -> a
unsafeIndex v (I# i) = unsafeIndex# v i
{-# INLINABLE unsafeIndex #-}

unsafeIndex# :: forall a. Prim a => Vector a -> Int# -> a
unsafeIndex# (Vector size level arr tail) i = let
    tailSize = andI# size KEY_MASK
    initSize = size -# tailSize
    in case i <# initSize of
        1# -> go i level arr
        _  -> A.index tail (i -# initSize)
    where go i level arr = case level ># 0# of
            1# -> go i (next level) (AA.index arr (index i level))
            _  -> A.index (aa2ba arr) (index i level)
{-# INLINABLE unsafeIndex# #-}

snoc :: forall a. Prim a => Vector a -> a -> Vector a
snoc (Vector size level init tail) v = let
    tailSize  = andI# size KEY_MASK
    initSize  = size -# tailSize
    size'     = size +# 1#
    tail'     = A.update width tail tailSize v
    width     = NODE_WIDTH

    insertArr :: AArray -> Int# -> Int# -> Int# -> AArray -> AArray
    insertArr arr mask i level init = case level ># 0# of
        1# -> case andI# i mask ==# 0# of 
            0# -> AA.modify NODE_WIDTH init (index i level) (insertArr arr (nextMask mask) i (next level))
            _  -> init1AA (insertArr arr (nextMask mask) i (next level) (_init (empty :: Vector a)))
        _ -> arr

    in case tailSize ==# KEY_MASK of
        0# -> Vector size' level init tail'
        _  -> let
            mask      = maxSize -# 1#
            prevLevel = level +# KEY_BITS
            maxSize   = uncheckedIShiftL# 1# prevLevel
            init'     = insertArr (ba2aa tail') mask initSize level init
            in case initSize ==# maxSize of
                0# -> Vector size' level init' (_tail (empty :: Vector a))
                _  -> Vector size' prevLevel (init2AA init init') (_tail (empty :: Vector a))
{-# INLINABLE snoc #-}


fromList :: Prim a => [a] -> Vector a
fromList = Data.List.foldl' snoc empty 
{-# INLINABLE fromList #-}

foldr :: forall a b. Prim a => (a -> b -> b) -> b -> Vector a -> b 
foldr f z (Vector size level init tail) = case initSize ==# 0# of
    0# -> notfull (initSize -# 1#) level init tailRes
    _  -> tailRes
    where
        tailSize = andI# size KEY_MASK
        initSize = size -# tailSize
        tailRes = A.foldr tailSize f z tail 

        notfull :: Int# -> Int# -> AArray -> b -> b 
        notfull lasti level arr z = case level ># 0# of
            1# -> AA.foldr lasti' (full level') (notfull lasti level' (AA.index arr lasti') z) arr
            _  -> A.foldr NODE_WIDTH f z (aa2ba arr)
            where lasti' = index lasti level
                  level' = next level

        full :: Int# -> AArray -> b -> b
        full level arr z = case level ># 0# of
            1# -> AA.foldr NODE_WIDTH (full (next level)) z arr
            _  -> A.foldr NODE_WIDTH f z (aa2ba arr)
{-# INLINABLE foldr #-}


rfoldr :: forall a b. Prim a => (a -> b -> b) -> b -> Vector a -> b 
rfoldr f z (Vector size level init tail) = case initSize ==# 0# of
    0# -> A.rfoldr tailSize f (notfull (initSize -# 1#) level init z) tail
    _  -> A.rfoldr tailSize f z tail 
    where
        tailSize = andI# size KEY_MASK
        initSize = size -# tailSize

        notfull :: Int# -> Int# -> AArray -> b -> b 
        notfull lasti level arr z = case level ># 0# of
            1# -> notfull lasti level' (AA.index arr lasti') (AA.rfoldr lasti' (full level') z arr)
            _  -> A.rfoldr NODE_WIDTH f z (aa2ba arr)
            where lasti' = index lasti level
                  level' = next level

        full :: Int# -> AArray -> b -> b
        full level arr z = case level ># 0# of
            1# -> AA.rfoldr NODE_WIDTH (full (next level)) z arr
            _  -> A.rfoldr NODE_WIDTH f z (aa2ba arr)
{-# INLINABLE rfoldr #-}

foldl' :: forall a b. Prim a => (b -> a -> b) -> b -> Vector a -> b 
foldl' f z (Vector size level init tail) = case initSize ==# 0# of
    0# -> A.foldl' tailSize f (notfull (initSize -# 1#) level init z) tail  
    _  -> A.foldl' tailSize f z tail 
    where
        tailSize = andI# size KEY_MASK
        initSize = size -# tailSize

        notfull :: Int# -> Int# -> AArray -> b -> b 
        notfull lasti level arr z = case level ># 0# of
            1# -> notfull lasti level' (AA.index arr lasti') (AA.foldl' lasti' (full level') z arr)
            _  -> A.foldl' width f z (aa2ba arr)
            where lasti' = index lasti level
                  level' = next level
                  width = NODE_WIDTH

        full :: Int# -> b -> AArray -> b
        full level z arr = case level ># 0# of
            1# -> AA.foldl' width (full (next level)) z arr
            _  -> A.foldl' width f z (aa2ba arr)
            where width = NODE_WIDTH
{-# INLINABLE foldl' #-}


rfoldl' :: forall a b. Prim a => (b -> a -> b) -> b -> Vector a -> b 
rfoldl' f z (Vector size level init tail) = case initSize ==# 0# of
    0# -> notfull (initSize -# 1#) level init (A.rfoldl' tailSize f z tail) 
    _  -> A.rfoldl' tailSize f z tail 
    where
        tailSize = andI# size KEY_MASK
        initSize = size -# tailSize

        notfull :: Int# -> Int# -> AArray -> b -> b 
        notfull lasti level arr z = case level ># 0# of
            1# -> AA.rfoldl' lasti' (full level') (notfull lasti level' (AA.index arr lasti') z) arr
            _  -> A.rfoldl' width f z (aa2ba arr)
            where lasti' = index lasti level
                  level' = next level
                  width = NODE_WIDTH

        full :: Int# -> b -> AArray -> b
        full level z arr = case level ># 0# of
            1# -> AA.rfoldl' width (full (next level)) z arr
            _  -> A.rfoldl' width f z (aa2ba arr)
            where width = NODE_WIDTH
{-# INLINABLE rfoldl' #-}


map :: forall a b. (Prim a, Prim b) => (a -> b) -> Vector a -> Vector b 
map _ s@(Vector 0# _     _    _   ) = unsafeCoerce# s
map f (Vector size level init tail) = Vector size level init' tail' where

    tailSize = andI# size KEY_MASK
    initSize = size -# tailSize
    init' = notfull (initSize -# 1#) level init
    tail' = A.map tailSize f tail

    notfull :: Int# -> Int# -> AArray -> AArray
    notfull lasti level arr = case level ># 0# of
        1# -> AA.mapInitLast lasti' (full level') (notfull lasti level') arr 
        _  -> ba2aa (A.map NODE_WIDTH f (aa2ba arr))
        where lasti' = index lasti level
              level' = next level

    full :: Int# -> AArray -> AArray
    full level arr = case level ># 0# of
        1# -> AA.map width (full (next level)) arr
        _  -> ba2aa (A.map width f (aa2ba arr))
        where width = NODE_WIDTH
{-# INLINABLE map #-}

modify# :: forall a. Prim a => Vector a -> Int# -> (a -> a) -> Vector a 
modify# (Vector size level init tail) i f = case i >=# 0# of 
    1# -> let
        tailSize = andI# size KEY_MASK
        initSize = size -# tailSize
        width = NODE_WIDTH
        in case i <# initSize of
            1# -> Vector size level (go i level init) tail
            _  -> case i <# size of
                1# -> Vector size level init (A.modify' width tail (i -# initSize) f)
                _  -> error "Vector.!: out of bounds"
    _  -> error "Vector.!: out of bounds"
    where go i level arr = case level ># 0# of
            1# -> AA.modify width arr (index i level) (go i (next level))
            _  -> ba2aa (A.modify' width (aa2ba arr) (index i level) f)
            where width = NODE_WIDTH
{-# INLINABLE modify# #-}

unsafeModify# :: forall a. Prim a => Vector a -> Int# -> (a -> a) -> Vector a 
unsafeModify# (Vector size level init tail) i f = 
    let tailSize = andI# size KEY_MASK
        initSize = size -# tailSize
        width = NODE_WIDTH
    in case i <# initSize of
        1# -> Vector size level (go i level init) tail
        _  -> Vector size level init (A.modify' width tail (i -# initSize) f)
    where go i level arr = case level ># 0# of
            1# -> AA.modify width arr (index i level) (go i (next level))
            _  -> ba2aa (A.modify' width (aa2ba arr) (index i level) f)
            where width = NODE_WIDTH
{-# INLINABLE unsafeModify# #-}

modify :: forall a. Prim a => Vector a -> Int -> (a -> a) -> Vector a 
modify v (I# i) f = modify# v i f
{-# INLINABLE modify #-}

unsafeModify :: forall a. Prim a => Vector a -> Int -> (a -> a) -> Vector a
unsafeModify v (I# i) f = unsafeModify# v i f
{-# INLINABLE unsafeModify #-}

empty :: Prim a => Vector a
empty = Vector 0# 0# (_getArr emptyAA) (A.new NODE_WIDTH)
{-# INLINABLE empty #-}

-- | This is needed so we can have a non-polymorphic static value
-- for the empty AArray
data AAWrap = AAWrap {_getArr :: AArray}
emptyAA = AAWrap (AA.new 0# undefElem)
{-# NOINLINE emptyAA #-}

singleton :: forall a. Prim a => a -> Vector a
singleton a = Vector 1# 0# (_init (empty :: Vector a)) (init1BA a)
{-# INLINABLE singleton #-}

length :: Vector a -> Int
length (Vector size _ _ _) = I# size
{-# INLINABLE length #-}

toList :: Prim a => Vector a -> [a]
toList = Data.TrieVector.Unboxed.foldr (:) []
{-# INLINABLE toList #-}


-- Internals -----------------------------------------------------------------


next :: Int# -> Int#
next level = level -# KEY_BITS
{-# INLINE next #-}

nextMask :: Int# -> Int#
nextMask mask = uncheckedIShiftRL# mask KEY_BITS
{-# INLINE nextMask #-}

index :: Int# -> Int# -> Int#
index i level = andI# (uncheckedIShiftRL# i level) KEY_MASK
{-# INLINE index #-}

aa2ba :: AArray -> ByteArray#
aa2ba = unsafeCoerce#
{-# INLINE aa2ba #-}

ba2aa :: ByteArray# -> AArray
ba2aa = unsafeCoerce#
{-# INLINE ba2aa #-}

undefElem :: a
undefElem = error "Vector: undefined element"
{-# NOINLINE undefElem #-}

init1BA :: Prim a => a -> ByteArray#
init1BA a = A.init1 NODE_WIDTH a
{-# INLINE init1BA #-}

init1AA :: AArray -> AArray
init1AA a = AA.init1 NODE_WIDTH a (_getArr emptyAA)
{-# INLINE init1AA #-}

init2AA :: AArray -> AArray -> AArray
init2AA a1 a2 = AA.init2 NODE_WIDTH a1 a2 (_getArr emptyAA)
{-# INLINE init2AA #-}


#undef NODE_WIDTH 
#undef KEY_BITS 
#undef KEY_MASK 


{-# LANGUAGE
  MagicHash, BangPatterns, CPP, RankNTypes,
  RoleAnnotations, UnboxedTuples, ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-full-laziness #-}

module Data.TrieVector (
      Vector(..)
    , (|>)
    , (!#)
    , safeAppend
    , safeRappend
    , unsafeIndex#
    , (!)
    , unsafeIndex
    , snoc
    , Data.TrieVector.foldr
    , Data.TrieVector.foldl'
    , rfoldr
    , rfoldl'
    , Data.TrieVector.map
    , modify
    , unsafeModify
    , modify#
    , unsafeModify#
    , singleton
    , empty
    , Data.TrieVector.length
    , toList
    , fromList 
    , pop
    ) where

import qualified Data.Foldable as F
import qualified Data.Traversable as T 

import GHC.Prim
import GHC.Types
import Debug.Trace
import Data.List

import Data.TrieVector.Array (Array)
import qualified Data.TrieVector.Array as A

import Data.TrieVector.ArrayArray (AArray, MAArray)
import qualified Data.TrieVector.ArrayArray as AA


#define NODE_WIDTH 16#
#define KEY_BITS 4#
#define KEY_MASK 15#


type role Vector nominal

data Vector a = Vector {
    _size, _level :: Int#,
    _init :: AArray,
    _tail :: Array a}

instance Functor Vector where
    fmap = Data.TrieVector.map
    {-# INLINABLE fmap #-}

instance Show a => Show (Vector a) where
    show seq = "fromList " ++ show (toList seq)

instance F.Foldable Vector where
    foldr = Data.TrieVector.foldr
    {-# INLINE foldr #-}
    foldl = Data.TrieVector.foldl'
    {-# INLINE foldl #-}
    foldl' = Data.TrieVector.foldl'
    {-# INLINE foldl' #-}
    length = Data.TrieVector.length
    {-# INLINE length #-}
    null v = Data.TrieVector.length v == 0
    {-# INLINE null #-}

safeAppend :: Vector a -> Vector a -> Vector a
safeAppend a b = Data.TrieVector.foldl' snoc a b 
{-# INLINE safeAppend #-}

safeRappend :: Vector a -> Vector a -> Vector a
safeRappend a b = Data.TrieVector.rfoldl' snoc a b 
{-# INLINE safeRappend #-}

(|>) :: Vector a -> a -> Vector a
(|>) = snoc
infixl 5 |>
{-# INLINE (|>) #-}


(!) :: forall a. Vector a -> Int -> a
(!) v (I# i) = v !# i
{-# INLINE (!) #-}

(!#) :: forall a. Vector a -> Int# -> a
(!#) (Vector size level init tail) i = case i >=# 0# of 
    1# -> let
        tailSize = andI# size KEY_MASK
        initSize = size -# tailSize
        in case i <# initSize of
            1# -> go i level init
            _  -> case i <# size of
                1# -> A.index tail (i -# initSize)
                _  -> error "Vector.!: out of bounds"
    _  -> error "Vector.!: out of bounds"
    where go i level init = case level ># 0# of 
            1# -> go i (next level) (AA.index init (index i level))
            _  -> A.index (aa2a init) (index i level)
infixl 5 !
{-# INLINE (!#) #-}

unsafeIndex :: Vector a -> Int -> a
unsafeIndex v (I# i) = unsafeIndex# v i
{-# INLINE unsafeIndex #-}

unsafeIndex# :: forall a. Vector a -> Int# -> a
unsafeIndex# (Vector size level init tail) i = let
    tailSize = andI# size KEY_MASK
    initSize = size -# tailSize
    in case i <# initSize of
        1# -> go i level init
        _  -> A.index tail (i -# initSize)
    where go i level init = case level ># 0# of
            1# -> go i (next level) (AA.index init (index i level))
            _  -> A.index (aa2a init) (index i level)
{-# INLINE unsafeIndex# #-}


snocArr :: AArray -> Int# -> Int# -> Int# -> AArray -> AArray
snocArr arr mask i level init = case level ># 0# of
    1# -> case andI# i mask ==# 0# of 
        0# -> AA.modify width init (index i level) (snocArr arr (nextMask mask) i (next level))
        _  -> init1AA (snocArr arr (nextMask mask) i (next level) (_init empty))
        where width = NODE_WIDTH
    _ -> arr

snoc :: forall a. Vector a -> a -> Vector a
snoc (Vector size level init tail) v = let
    tailSize  = andI# size KEY_MASK
    initSize  = size -# tailSize
    size'     = size +# 1#
    width     = NODE_WIDTH
    tail'     = A.update width tail tailSize v

    in case tailSize ==# KEY_MASK of
        0# -> Vector size' level init tail'
        _  -> let
            mask      = maxSize -# 1#
            prevLevel = level +# KEY_BITS
            maxSize   = uncheckedIShiftL# 1# prevLevel
            init'     = snocArr (a2aa tail') mask initSize level init
            in case initSize ==# maxSize of
                0# -> Vector size' level init' (_tail empty)
                _  -> Vector size' prevLevel (init2AA init init') (_tail empty)
{-# INLINE snoc #-}

popArray :: Int# -> Int# -> Int# -> AArray -> (# Array a, AArray #)
popArray mask i level init = case level ># 0# of
    1# -> case popArray (nextMask mask) i (next level) (AA.index init ix) of
        (# popped, newElem #) -> case andI# i mask ==# 0# of
            0# -> (# popped, let w = NODE_WIDTH in AA.update width init ix newElem #)
            _  -> (# popped, _init empty #)
        where ix = index i level
              width = NODE_WIDTH
    _ -> (# aa2a init, _init empty #)
{-# INLINE popArray #-}

pop :: forall a. Vector a -> (Vector a, a)
pop (Vector 0#   _      _    _   ) = error "Vector.pop: empty vector"
pop (Vector size level init tail) = let
    tailSize  = andI# size KEY_MASK
    initSize  = size -# tailSize
    size'     = size -# 1#
    width     = NODE_WIDTH

    in case tailSize ==# 0# of
        0# -> let lasti = tailSize -# 1# in 
            (Vector size' level init (A.update width tail lasti undefElem), A.index tail lasti)
        _  -> let
            prevLevel = level +# KEY_BITS
            mask      = (uncheckedIShiftL# 1# prevLevel) -# 1#
            (# popped, init' #) = popArray mask size' level init
            in case index size' level ==# 0# of
                0# -> (Vector size' level init' popped, A.index popped (width -# 1#))
                _  -> (Vector size' (next level) (AA.index init' 0#) popped, A.index popped (width -# 1#))
{-# INLINE pop #-}

fromList :: [a] -> Vector a
fromList = Data.List.foldl' snoc empty

foldr :: forall a b. (a -> b -> b) -> b -> Vector a -> b 
foldr f z (Vector size level arr tail) = case initSize ==# 0# of
    0# -> notfull (initSize -# 1#) level arr tailRes
    _  -> tailRes
    where
        tailSize = andI# size KEY_MASK
        initSize = size -# tailSize
        tailRes = A.foldr tailSize f z tail 

        notfull :: Int# -> Int# -> AArray -> b -> b 
        notfull lasti level arr z = case level ># 0# of
            1# -> AA.foldr lasti' (full level') (notfull lasti level' (AA.index arr lasti') z) arr
            _  -> A.foldr NODE_WIDTH f z (aa2a arr)
            where lasti' = index lasti level
                  level' = next level

        full :: Int# -> AArray -> b -> b
        full level arr z = case level ># 0# of
            1# -> AA.foldr NODE_WIDTH (full (next level)) z arr
            _  -> A.foldr NODE_WIDTH f z (aa2a arr)
{-# INLINE foldr #-}


rfoldr :: forall a b. (a -> b -> b) -> b -> Vector a -> b 
rfoldr f z (Vector size level init tail) = case initSize ==# 0# of
    0# -> A.rfoldr tailSize f (notfull (initSize -# 1#) level init z) tail
    _  -> A.rfoldr tailSize f z tail 
    where
        tailSize = andI# size KEY_MASK
        initSize = size -# tailSize

        notfull :: Int# -> Int# -> AArray -> b -> b 
        notfull lasti level arr z = case level ># 0# of
            1# -> notfull lasti level' (AA.index arr lasti') (AA.rfoldr lasti' (full level') z arr)
            _  -> A.rfoldr NODE_WIDTH f z (aa2a arr)
            where lasti' = index lasti level
                  level' = next level

        full :: Int# -> AArray -> b -> b
        full level arr z = case level ># 0# of
            1# -> AA.rfoldr NODE_WIDTH (full (next level)) z arr
            _  -> A.rfoldr NODE_WIDTH f z (aa2a arr)
{-# INLINE rfoldr #-}

foldl' :: forall a b. (b -> a -> b) -> b -> Vector a -> b 
foldl' f z (Vector size level init tail) = case initSize ==# 0# of
    0# -> A.foldl' tailSize f (notfull (initSize -# 1#) level init z) tail  
    _  -> A.foldl' tailSize f z tail 
    where
        tailSize = andI# size KEY_MASK
        initSize = size -# tailSize

        notfull :: Int# -> Int# -> AArray -> b -> b 
        notfull lasti level arr z = case level ># 0# of
            1# -> notfull lasti level' (AA.index arr lasti') (AA.foldl' lasti' (full level') z arr)
            _  -> A.foldl' width f z (aa2a arr)
            where lasti' = index lasti level
                  level' = next level
                  width = NODE_WIDTH

        full :: Int# -> b -> AArray -> b
        full level z arr = case level ># 0# of
            1# -> AA.foldl' width (full (next level)) z arr
            _  -> A.foldl' width f z (aa2a arr)
            where width = NODE_WIDTH
{-# INLINE foldl' #-}

rfoldl' :: forall a b. (b -> a -> b) -> b -> Vector a -> b 
rfoldl' f z (Vector size level init tail) = case initSize ==# 0# of
    0# -> notfull (initSize -# 1#) level init (A.rfoldl' tailSize f z tail)  
    _  -> A.rfoldl' tailSize f z tail 
    where
        tailSize = andI# size KEY_MASK
        initSize = size -# tailSize

        notfull :: Int# -> Int# -> AArray -> b -> b 
        notfull lasti level arr z = case level ># 0# of
            1# -> AA.rfoldl' lasti' (full level') (notfull lasti level' (AA.index arr lasti') z) arr
            _  -> A.rfoldl' width f z (aa2a arr)
            where lasti' = index lasti level
                  level' = next level
                  width = NODE_WIDTH

        full :: Int# -> b -> AArray -> b
        full level z arr = case level ># 0# of
            1# -> AA.rfoldl' width (full (next level)) z arr
            _  -> A.rfoldl' width f z (aa2a arr)
            where width = NODE_WIDTH
{-# INLINE rfoldl' #-}

map :: forall a b. (a -> b) -> Vector a -> Vector b 
map f s@(Vector 0# _      _    _  ) = unsafeCoerce# s
map f (Vector size level init tail) = Vector size level init' tail' where

    tailSize = andI# size KEY_MASK
    initSize = size -# tailSize
    init' = notfull (initSize -# 1#) level init
    tail' = A.map tailSize f tail

    notfull :: Int# -> Int# -> AArray -> AArray
    notfull lasti level arr = case level ># 0# of
        1# -> AA.mapInitLast lasti' (full level') (notfull lasti level') arr 
        _  -> a2aa (A.map NODE_WIDTH f (aa2a arr))
        where lasti' = index lasti level
              level' = next level

    full :: Int# -> AArray -> AArray
    full level arr = case level ># 0# of
        1# -> AA.map width (full (next level)) arr
        _  -> a2aa (A.map width f (aa2a arr))
        where width = NODE_WIDTH
{-# INLINE map #-}

modify# :: forall a. Vector a -> Int# -> (a -> a) -> Vector a 
modify# (Vector size level init tail) i f = case i >=# 0# of 
    1# -> let
        tailSize = andI# size KEY_MASK
        initSize = size -# tailSize
        in case i <# initSize of
            1# -> Vector size level (go i level init) tail
            _  -> case i <# size of
                1# -> Vector size level init (A.modify NODE_WIDTH tail (i -# initSize) f)
                _  -> error "Vector.!: out of bounds"
    _  -> error "Vector.!: out of bounds"
    where go i level arr = case level ># 0# of
            1# -> AA.modify width arr (index i level) (go i (next level))
            _  -> a2aa (A.modify NODE_WIDTH (aa2a arr) (index i level) f)
            where width = NODE_WIDTH
{-# INLINE modify# #-}

unsafeModify# :: forall a. Vector a -> Int# -> (a -> a) -> Vector a 
unsafeModify# (Vector size level init tail) i f = 
    let tailSize = andI# size KEY_MASK
        initSize = size -# tailSize
    in case i <# initSize of
        1# -> Vector size level (go i level init) tail
        _  -> Vector size level init (A.modify NODE_WIDTH tail (i -# initSize) f)
    where go i level arr = case level ># 0# of
            1# -> AA.modify width arr (index i level) (go i (next level))
            _  -> a2aa (A.modify NODE_WIDTH (aa2a arr) (index i level) f)
            where width = NODE_WIDTH
{-# INLINE unsafeModify# #-}

modify :: forall a. Vector a -> Int -> (a -> a) -> Vector a 
modify v (I# i) f = modify# v i f
{-# INLINE modify #-}

unsafeModify :: forall a. Vector a -> Int -> (a -> a) -> Vector a
unsafeModify v (I# i) f = unsafeModify# v i f
{-# INLINE unsafeModify #-}

empty :: Vector a
empty = Vector 0# 0# emptyAA emptyTail where
    !emptyAA = AA.new 0# undefElem
    !emptyTail = A.new NODE_WIDTH undefElem

singleton :: a -> Vector a
singleton a = Vector 1# 0# (_init empty) (init1A a)
{-# INLINE singleton #-}

length :: Vector a -> Int
length (Vector size _ _ _) = I# size
{-# INLINE length #-}

toList :: Vector a -> [a]
toList = Data.TrieVector.foldr (:) []
{-# INLINE toList #-}



-- Internals -----------------------------------------------------------------


-- TODO: we don't need some of this with the new ArrayArray API
-- And some can be shared with Unboxed (along with a large chunk of the implementation
-- concerning AArrays

next :: Int# -> Int#
next level = level -# KEY_BITS
{-# INLINE next #-}

nextMask :: Int# -> Int#
nextMask mask = uncheckedIShiftRL# mask KEY_BITS
{-# INLINE nextMask #-}

index :: Int# -> Int# -> Int#
index i level = andI# (uncheckedIShiftRL# i level) KEY_MASK
{-# INLINE index #-}

aa2a :: AArray -> Array a
aa2a = unsafeCoerce#
{-# INLINE aa2a #-}

a2aa :: Array a -> AArray
a2aa = unsafeCoerce#
{-# INLINE a2aa #-}

undefElem :: a
undefElem = error "Vector: undefined element"
{-# NOINLINE undefElem #-}

init1A :: a -> Array a
init1A a = A.init1 NODE_WIDTH a undefElem
{-# INLINE init1A #-}

init1AA :: AArray -> AArray
init1AA a = AA.init1 NODE_WIDTH a (_tail empty)
{-# INLINE init1AA #-}

init2AA :: AArray -> AArray -> AArray
init2AA a1 a2 = AA.init2 NODE_WIDTH a1 a2 (_tail empty)
{-# INLINE init2AA #-}


#undef NODE_WIDTH 
#undef KEY_BITS 
#undef KEY_MASK 

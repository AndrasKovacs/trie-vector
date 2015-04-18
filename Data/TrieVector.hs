
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

infixl 5 !
(!) :: forall a. Vector a -> Int -> a
(!) v (I# i) = v !# i
{-# INLINE (!) #-}


indexAA :: Int# -> Int# -> AArray -> a
indexAA i 0#    init = A.index (aa2a init) (index i 0#)
indexAA i level init = indexAA i (next level) (AA.index init (index i level))

(!#) :: forall a. Vector a -> Int# -> a
(!#) (Vector size level init tail) i = case i >=# 0# of 
    1# -> let
        tailSize = andI# size KEY_MASK
        initSize = size -# tailSize
        in case i <# initSize of
            1# -> indexAA i level init
            _  -> case i <# size of
                1# -> A.index tail (i -# initSize)
                _  -> boundsError
    _  -> boundsError
{-# INLINE (!#) #-}


unsafeIndex :: Vector a -> Int -> a
unsafeIndex v (I# i) = unsafeIndex# v i
{-# INLINE unsafeIndex #-}

unsafeIndex# :: forall a. Vector a -> Int# -> a
unsafeIndex# (Vector size level init tail) i = let
    tailSize = andI# size KEY_MASK
    initSize = size -# tailSize
    in case i <# initSize of
        1# -> indexAA i level init
        _  -> A.index tail (i -# initSize)
{-# INLINE unsafeIndex# #-}

snocArr :: AArray -> Int# -> Int# -> Int# -> AArray -> AArray
snocArr arr mask i 0#    init = arr
snocArr arr mask i level init = case andI# i mask of
  0# -> init1AA (snocArr arr (nextMask mask) i (next level) (_init empty))
  _  -> AA.modify NODE_WIDTH init (index i level) (snocArr arr (nextMask mask) i (next level))

snoc :: forall a. Vector a -> a -> Vector a
snoc (Vector size level init tail) v = let
    tailSize  = andI# size KEY_MASK
    initSize  = size -# tailSize
    size'     = size +# 1#
    tail'     =
      A.update NODE_WIDTH tail tailSize v

    in case tailSize of
        KEY_MASK -> let
          mask      = maxSize -# 1#
          prevLevel = level +# KEY_BITS
          maxSize   = uncheckedIShiftL# 1# prevLevel
          init'     = snocArr (a2aa tail') mask initSize level init
          in case initSize ==# maxSize of
              1# -> Vector size' prevLevel (init2AA init init') (_tail empty)          
              _  -> Vector size' level init' (_tail empty)
        _ -> Vector size' level init tail'
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
pop (Vector 0#   _     _    _   ) = popError
pop (Vector size level init tail) = let
    tailSize  = andI# size KEY_MASK
    initSize  = size -# tailSize
    size'     = size -# 1#
    width     = NODE_WIDTH

    in case tailSize of
        0# -> let
          prevLevel = level +# KEY_BITS
          mask      = (uncheckedIShiftL# 1# prevLevel) -# 1#
          (# popped, init' #) = popArray mask size' level init
          in case index size' level ==# 0# of
              0# -> (Vector size' level init' popped, A.index popped (width -# 1#))
              _  -> (Vector size' (next level) (AA.index init' 0#) popped, A.index popped (width -# 1#))
        _ -> let lasti = tailSize -# 1# in 
          (Vector size' level init (A.update width tail lasti undefElem), A.index tail lasti)
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
        notfull lasti 0#    arr z = A.foldr NODE_WIDTH f z (aa2a arr)
        notfull lasti level arr z =
            AA.foldr lasti' (full level') (notfull lasti level' (AA.index arr lasti') z) arr
            where lasti' = index lasti level
                  level' = next level

        full :: Int# -> AArray -> b -> b
        full 0#    arr z = A.foldr NODE_WIDTH f z (aa2a arr)
        full level arr z = AA.foldr NODE_WIDTH (full (next level)) z arr
{-# INLINE foldr #-}


rfoldr :: forall a b. (a -> b -> b) -> b -> Vector a -> b 
rfoldr f z (Vector size level init tail) = case initSize ==# 0# of
    0# -> A.rfoldr tailSize f (notfull (initSize -# 1#) level init z) tail
    _  -> A.rfoldr tailSize f z tail 
    where
        tailSize = andI# size KEY_MASK
        initSize = size -# tailSize

        notfull :: Int# -> Int# -> AArray -> b -> b 
        notfull lasti level arr z = case level of
            0# -> A.rfoldr NODE_WIDTH f z (aa2a arr)
            _  -> notfull lasti level' (AA.index arr lasti') (AA.rfoldr lasti' (full level') z arr) 
            where lasti' = index lasti level
                  level' = next level

        full :: Int# -> AArray -> b -> b
        full 0#    arr z = A.rfoldr NODE_WIDTH f z (aa2a arr)
        full level arr z = AA.rfoldr NODE_WIDTH (full (next level)) z arr
{-# INLINE rfoldr #-}

foldl' :: forall a b. (b -> a -> b) -> b -> Vector a -> b 
foldl' f z (Vector size level init tail) = case initSize ==# 0# of
    0# -> A.foldl' tailSize f (notfull (initSize -# 1#) level init z) tail  
    _  -> A.foldl' tailSize f z tail 
    where
        tailSize = andI# size KEY_MASK
        initSize = size -# tailSize

        notfull :: Int# -> Int# -> AArray -> b -> b 
        notfull lasti level arr z = case level of
            0#  -> A.foldl' width f z (aa2a arr)          
            _   -> notfull lasti level' (AA.index arr lasti') (AA.foldl' lasti' (full level') z arr)
            where lasti' = index lasti level
                  level' = next level
                  width  = NODE_WIDTH

        full :: Int# -> b -> AArray -> b
        full level z arr = case level of
            0# -> A.foldl' width f z (aa2a arr)          
            _  -> AA.foldl' width (full (next level)) z arr
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
    notfull lasti level arr = case level of
        0# -> a2aa (A.map NODE_WIDTH f (aa2a arr))      
        _  -> AA.mapInitLast lasti' (full level') (notfull lasti level') arr 
        where lasti' = index lasti level
              level' = next level

    full :: Int# -> AArray -> AArray
    full 0#    arr = a2aa (A.map NODE_WIDTH f (aa2a arr))
    full level arr = AA.map NODE_WIDTH (full (next level)) arr
{-# INLINE map #-}


modifyAA :: Int# -> Int# -> (a -> a) -> AArray -> AArray
modifyAA i 0#    f arr = a2aa (A.modify NODE_WIDTH (aa2a arr) (index i 0#) f)
modifyAA i level f arr = AA.modify NODE_WIDTH arr (index i level) (modifyAA i (next level) f)  

modify# :: forall a. Vector a -> Int# -> (a -> a) -> Vector a 
modify# (Vector size level init tail) i f = case i >=# 0# of 
    1# -> let
        tailSize = andI# size KEY_MASK
        initSize = size -# tailSize
        in case i <# initSize of
            1# -> Vector size level (modifyAA i level f init) tail
            _  -> case i <# size of
                1# -> Vector size level init (A.modify NODE_WIDTH tail (i -# initSize) f)
                _  -> boundsError
    _  -> boundsError
{-# INLINE modify# #-}

unsafeModify# :: forall a. Vector a -> Int# -> (a -> a) -> Vector a 
unsafeModify# (Vector size level init tail) i f = 
    let tailSize = andI# size KEY_MASK
        initSize = size -# tailSize
    in case i <# initSize of
        1# -> Vector size level (modifyAA i level f init) tail
        _  -> Vector size level init (A.modify NODE_WIDTH tail (i -# initSize) f)
{-# INLINE unsafeModify# #-}

modify :: forall a. Vector a -> Int -> (a -> a) -> Vector a 
modify v (I# i) f = modify# v i f
{-# INLINE modify #-}

unsafeModify :: forall a. Vector a -> Int -> (a -> a) -> Vector a
unsafeModify v (I# i) f = unsafeModify# v i f
{-# INLINE unsafeModify #-}

empty :: Vector a
empty = Vector 0# 0# emptyAA emptyTail where
    !emptyAA   = AA.new 0# undefElem
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

boundsError :: a
boundsError = error "TrieVector: index out of bounds"
{-# NOINLINE boundsError #-}

undefElem :: a
undefElem = error "Vector: undefined element"
{-# NOINLINE undefElem #-}

popError :: a
popError = error "TrieVector: can't pop from empty vector"
{-# NOINLINE popError #-}

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

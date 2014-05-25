
{-# LANGUAGE MagicHash, BangPatterns, UnboxedTuples, CPP, RankNTypes, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-full-laziness #-}

module Unboxed (
      Vector(..)
    , (|>)
    , (!#)
    , unsafeIndex#
    , (!)
    , unsafeIndex
    , snoc
    , Unboxed.foldr
    , foldl'
    , rfoldr
    , rfoldl'
    , Unboxed.map
    , modify
    , modify#
    , unsafeModify#
    , unsafeModify
    , singleton
    , empty
    , Unboxed.length
    , toList
    --, fromList 
    ) where

import qualified Data.Foldable as F
import qualified Data.Traversable as T 
import GHC.Prim
import GHC.Types
import Data.Primitive.Types

import qualified ByteArray as A
import qualified ArrayArray as AA

#define NODE_WIDTH 16#
#define KEY_BITS 4#
#define KEY_MASK 15#


main = do
    let r = [0..10 :: Int ]
        s = Prelude.foldl snoc empty r
    print $ modify s 0 (+500)

data Vector a = Vector {
    _size, _level :: Int#,
    _init :: ArrayArray#,
    _tail :: ByteArray#}

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

    insertArr :: ArrayArray# -> Int# -> Int# -> Int# -> ArrayArray# -> ArrayArray#
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

-- not yet right

--unsafeSnoc :: forall a. Prim a => (# Int#, Int#, ArrayArray#, ByteArray# #) -> a -> (# Int#, Int#, ArrayArray#, ByteArray# #)
--unsafeSnoc (# size, level, init, tail #) v = let
--    tailSize  = andI# size KEY_MASK
--    initSize  = size -# tailSize
--    size'     = size +# 1#
--    tail'     = A.unsafeUpdate tail tailSize v

--    unsafeSnocArr :: ArrayArray# -> Int# -> Int# -> Int# -> ArrayArray# -> ArrayArray#
--    unsafeSnocArr arr mask i level init = case level ># 0# of
--        1# -> case andI# i mask ==# 0# of 
--            0# -> AA.unsafeModify' init (index i level) (unsafeSnocArr arr (nextMask mask) i (next level))
--            _  -> init1AA (unsafeSnocArr arr (nextMask mask) i (next level) (_init (empty :: Vector a)))
--        _ -> arr

--    in case tailSize ==# KEY_MASK of
--        0# ->  (# size', level, init, tail' #)
--        _  -> let
--            mask      = maxSize -# 1#
--            prevLevel = level +# KEY_BITS
--            maxSize   = uncheckedIShiftL# 1# prevLevel
--            init'     = unsafeSnocArr (ba2aa tail') mask initSize level init
--            in case initSize ==# maxSize of
--                0# -> (# size', level, init', A.new NODE_WIDTH #)
--                _  -> (# size', prevLevel, init2AA init init', A.new NODE_WIDTH #)
--{-# INLINE unsafeSnoc #-}

--fromList :: Prim a => [a] -> Vector a
--fromList xs = case go (# 0#, 0#, AA.new NODE_WIDTH, A.new NODE_WIDTH #) xs of
--    (# size, level, init, tail #) -> Vector size level init tail
--    where go acc (x:xs) = go (unsafeSnoc acc x) xs
--          go acc []     = acc 
--{-# INLINE fromList #-}


foldr :: forall a b. Prim a => (a -> b -> b) -> b -> Vector a -> b 
foldr f z (Vector size level arr tail) = case initSize ==# 0# of
    0# -> notfull (initSize -# 1#) level arr tailRes
    _  -> tailRes
    where
        tailSize = andI# size KEY_MASK
        initSize = size -# tailSize
        tailRes = A.foldr tailSize f z tail 

        notfull :: Int# -> Int# -> ArrayArray# -> b -> b 
        notfull lasti level arr z = case level ># 0# of
            1# -> AA.foldr lasti' (full level') (notfull lasti level' (AA.index arr lasti') z) arr
            _  -> A.foldr NODE_WIDTH f z (aa2ba arr)
            where lasti' = index lasti level
                  level' = next level

        full :: Int# -> ArrayArray# -> b -> b
        full level arr z = case level ># 0# of
            1# -> AA.foldr NODE_WIDTH (full (next level)) z arr
            _  -> A.foldr NODE_WIDTH f z (aa2ba arr)
{-# INLINABLE foldr #-}



rfoldr :: forall a b. Prim a => (a -> b -> b) -> b -> Vector a -> b 
rfoldr f z (Vector size level arr tail) = case initSize ==# 0# of
    0# -> A.rfoldr tailSize f (notfull (initSize -# 1#) level arr z) tail
    _  -> A.rfoldr tailSize f z tail 
    where
        tailSize = andI# size KEY_MASK
        initSize = size -# tailSize

        notfull :: Int# -> Int# -> ArrayArray# -> b -> b 
        notfull lasti level arr z = case level ># 0# of
            1# -> AA.rfoldr lasti' (full level') (notfull lasti level' (AA.index arr lasti') z) arr
            _  -> A.rfoldr NODE_WIDTH f z (aa2ba arr)
            where lasti' = index lasti level
                  level' = next level

        full :: Int# -> ArrayArray# -> b -> b
        full level arr z = case level ># 0# of
            1# -> AA.rfoldr NODE_WIDTH (full (next level)) z arr
            _  -> A.rfoldr NODE_WIDTH f z (aa2ba arr)
{-# INLINE rfoldr #-}


foldl' :: forall a b. Prim a => (b -> a -> b) -> b -> Vector a -> b 
foldl' f z (Vector size level arr tail) = case initSize ==# 0# of
    0# -> notfull (initSize -# 1#) level arr tailRes
    _  -> tailRes
    where
        tailSize = andI# size KEY_MASK
        initSize = size -# tailSize
        tailRes = A.foldl' tailSize f z tail 

        notfull :: Int# -> Int# -> ArrayArray# -> b -> b 
        notfull lasti level arr z = case level ># 0# of
            1# -> AA.foldl' lasti' (full level') (notfull lasti level' (AA.index arr lasti') z) arr
            _  -> A.foldl' width f z (aa2ba arr)
            where lasti' = index lasti level
                  level' = next level
                  width = NODE_WIDTH

        full :: Int# -> b -> ArrayArray# -> b
        full level z arr = case level ># 0# of
            1# -> AA.foldl' width (full (next level)) z arr
            _  -> A.foldl' width f z (aa2ba arr)
            where width = NODE_WIDTH
{-# INLINABLE foldl' #-}


rfoldl' :: forall a b. Prim a => (b -> a -> b) -> b -> Vector a -> b 
rfoldl' f z (Vector size level arr tail) = case initSize ==# 0# of
    0# -> A.rfoldl' tailSize f (notfull (initSize -# 1#) level arr z) tail  
    _  -> A.rfoldl' tailSize f z tail 
    where
        tailSize = andI# size KEY_MASK
        initSize = size -# tailSize

        notfull :: Int# -> Int# -> ArrayArray# -> b -> b 
        notfull lasti level arr z = case level ># 0# of
            1# -> AA.rfoldl' lasti' (full level') (notfull lasti level' (AA.index arr lasti') z) arr
            _  -> A.rfoldl' width f z (aa2ba arr)
            where lasti' = index lasti level
                  level' = next level
                  width = NODE_WIDTH

        full :: Int# -> b -> ArrayArray# -> b
        full level z arr = case level ># 0# of
            1# -> AA.rfoldl' width (full (next level)) z arr
            _  -> A.rfoldl' width f z (aa2ba arr)
            where width = NODE_WIDTH
{-# INLINABLE rfoldl' #-}

map :: forall a b. (Prim a, Prim b) => (a -> b) -> Vector a -> Vector b 
map f s@(Vector 0# level init tail) = unsafeCoerce# s
map f (Vector size level init tail) = Vector size level init' tail' where

    tailSize = andI# size KEY_MASK
    initSize = size -# tailSize
    init' = notfull (initSize -# 1#) level init
    tail' = A.map tailSize f tail

    notfull :: Int# -> Int# -> ArrayArray# -> ArrayArray#
    notfull lasti level arr = case level ># 0# of
        1# -> AA.mapInitLast' lasti' (full level') (notfull lasti level') arr 
        _  -> ba2aa (A.map NODE_WIDTH f (aa2ba arr))
        where lasti' = index lasti level
              level' = next level

    full :: Int# -> ArrayArray# -> ArrayArray#
    full level arr = case level ># 0# of
        1# -> AA.map' width (full (next level)) arr
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
            1# -> AA.modify' width arr (index i level) (go i (next level))
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
            1# -> AA.modify' width arr (index i level) (go i (next level))
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
empty = Vector 0# 0# emptyAA emptyTail where
    !emptyAA = AA.new 0#
    !emptyTail = A.new NODE_WIDTH
{-# INLINABLE empty #-}

singleton :: forall a. Prim a => a -> Vector a
singleton a = Vector 1# 0# (_init (empty :: Vector a)) (init1BA a)
{-# INLINABLE singleton #-}

length :: Vector a -> Int
length (Vector size _ _ _) = I# size
{-# INLINABLE length #-}

toList :: Prim a => Vector a -> [a]
toList = Unboxed.foldr (:) []
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

aa2ba :: ArrayArray# -> ByteArray#
aa2ba = unsafeCoerce#
{-# INLINE aa2ba #-}

ba2aa :: ByteArray# -> ArrayArray#
ba2aa = unsafeCoerce#
{-# INLINE ba2aa #-}

undefElem :: a
undefElem = error "Vector: undefined element"
{-# NOINLINE undefElem #-}

init1BA :: Prim a => a -> ByteArray#
init1BA a = A.init1 NODE_WIDTH a
{-# INLINE init1BA #-}

init1AA :: ArrayArray# -> ArrayArray#
init1AA a = AA.init1 NODE_WIDTH a undefElem
{-# INLINE init1AA #-}

init2AA :: ArrayArray# -> ArrayArray# -> ArrayArray#
init2AA a1 a2 = AA.init2 NODE_WIDTH a1 a2 undefElem
{-# INLINE init2AA #-}


-- Bounds checked functions ---------------------------------------------------

--modifyAA :: ArrayArray# -> Int# -> (ArrayArray# -> ArrayArray#) -> ArrayArray#
--modifyAA arr i a = let size = NODE_WIDTH in case i <# sizeofArrayArray# arr of
--    1# -> AA.modify' size arr i a 
--    _  -> error $ "modifyAA: out of bound: " ++ show (I# i) ++ " " ++ show (I# (sizeofArrayArray# arr))
--  {-# INLINE modifyAA #-}

--updateA :: Array# a -> Int# -> a -> Array# a
--updateA arr i a = let size = NODE_WIDTH in case i <# sizeofArray# arr of
--    1# -> A.update size arr i a
--    _  -> error $ "updateA: out of bound: " ++ show (I# i) ++ " " ++ show (I# (sizeofArray# arr))
-- {-# INLINE updateA #-}

--indexA arr i = case i <# sizeofArray# arr of
--    1# -> A.index arr i
--    _  -> error "indexA: out of bounds"

--indexAA arr i = case i <# sizeofArrayArray# arr of
--    1# -> AA.index arr i
--    _  -> error "indexAA: out of bounds"


#undef NODE_WIDTH 
#undef KEY_BITS 
#undef KEY_MASK 

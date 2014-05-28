
{-# LANGUAGE MagicHash, BangPatterns, CPP, RankNTypes, UnboxedTuples, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-full-laziness #-}

module Vector (
      Vector(..)
    , (|>)
    , (!#)
    , append
    , rappend
    , unsafeIndex#
    , (!)
    , unsafeIndex
    , snoc
    , Vector.foldr
    , Vector.foldl'
    , rfoldr
    , rfoldl'
    , Vector.map
    , modify
    , modify#
    , unsafeModify#
    , unsafeModify
    , singleton
    , empty
    , Vector.length
    , toList
    , fromList 
    , pop
 
    ) where

import qualified Data.Foldable as F
import qualified Data.Traversable as T 
import Data.Monoid
import GHC.Prim
import GHC.Types
import Debug.Trace
import Data.List

import qualified Array as A
import qualified ArrayArray as AA

#define NODE_WIDTH 16#
#define KEY_BITS 4#
#define KEY_MASK 15#


main = do
    let !a = Prelude.foldl snoc empty [0..10]
    let !b = Prelude.foldl snoc empty [0..20]
    print $ 0



data Vector a = Vector {
    _size, _level :: Int#,
    _init :: ArrayArray#,
    _tail :: Array# a}

instance Functor Vector where
    fmap = Vector.map
    {-# INLINABLE fmap #-}

instance Show a => Show (Vector a) where
    show seq = "fromList " ++ show (toList seq)

instance F.Foldable Vector where
    foldr = Vector.foldr
    {-# INLINE foldr #-}
    foldl = Vector.foldl'
    {-# INLINE foldl #-}
    foldl' = Vector.foldl'
    {-# INLINE foldl' #-}

instance T.Traversable Vector where
    traverse f (Vector s l arr tail) = error "TODO: not implemented"


safeAppend :: Vector a -> Vector a -> Vector a
safeAppend a b = Vector.foldl' snoc a b 
{-# INLINE safeAppend #-}

safeRappend :: Vector a -> Vector a -> Vector a
safeRappend a b = Vector.rfoldl' snoc a b 
{-# INLINE safeRappend #-}

append :: Vector a -> Vector a -> Vector a
append a b = Vector.foldl' go (copyEdge a) b where
    go (Vector s l i t) x = case unsafeSnoc (# s, l, i, t #) x of
        (# s, l, i, t #) -> Vector s l i t 
{-# INLINE append #-}

rappend :: Vector a -> Vector a -> Vector a
rappend a b = Vector.rfoldl' go (copyEdge a) b where
    go (Vector s l i t) x = case unsafeSnoc (# s, l, i, t #) x of
        (# s, l, i, t #) -> Vector s l i t 
{-# INLINE rappend #-}


copyEdge :: Vector a -> Vector a
copyEdge (Vector size level init tail) = let
    tailSize = andI# size KEY_MASK
    initSize = size -# tailSize
    width    = NODE_WIDTH
    tail'    = cloneArray# tail 0# width

    go i level arr = case level ># 0# of
        1# -> AA.modify' width arr (index i level) (go i (next level))
        _  -> a2aa (cloneArray# (aa2a arr) 0# width)

    in case initSize of
        0# -> Vector size level (a2aa (cloneArray# (aa2a init) 0# width)) tail'
        _  -> Vector size level (go (initSize -# 1#) level init) tail'
{-# INLINE copyEdge #-}


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


snocArr :: ArrayArray# -> Int# -> Int# -> Int# -> ArrayArray# -> ArrayArray#
snocArr arr mask i level init = case level ># 0# of
    1# -> case andI# i mask ==# 0# of 
        0# -> AA.modify' width init (index i level) (snocArr arr (nextMask mask) i (next level))
        _  -> init1AA (snocArr arr (nextMask mask) i (next level) (_init empty))
        where width = NODE_WIDTH
    _ -> arr

unsafeSnocArr :: ArrayArray# -> Int# -> Int# -> Int# -> ArrayArray# -> ArrayArray#
unsafeSnocArr arr mask i level init = case level ># 0# of
    1# -> case andI# i mask ==# 0# of 
        0# -> AA.unsafeModify' init (index i level) (unsafeSnocArr arr (nextMask mask) i (next level))
        _  -> init1AA (unsafeSnocArr arr (nextMask mask) i (next level) (AA.new NODE_WIDTH))
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

popArray :: Int# -> Int# -> Int# -> ArrayArray# -> (# Array# a, ArrayArray# #)
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
pop (Vector 0#   level init tail) = error "Vector.pop: empty vector"
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


unsafeSnoc :: (# Int#, Int#, ArrayArray#, Array# a #) -> a -> (# Int#, Int#, ArrayArray#, Array# a #)
unsafeSnoc (# size, level, init, tail #) v = let
    tailSize  = andI# size KEY_MASK
    initSize  = size -# tailSize
    size'     = size +# 1#
    tail'     = A.unsafeUpdate tail tailSize v

    in case tailSize ==# KEY_MASK of
        0# ->  (# size', level, init, tail' #)
        _  -> let
            mask      = maxSize -# 1#
            prevLevel = level +# KEY_BITS
            maxSize   = uncheckedIShiftL# 1# prevLevel
            init'     = unsafeSnocArr (a2aa tail') mask initSize level init
            in case initSize ==# maxSize of
                0# -> (# size', level, init', A.new NODE_WIDTH undefElem #)
                _  -> (# size', prevLevel, init2AA init init', A.new NODE_WIDTH undefElem #)
{-# INLINE unsafeSnoc #-}


-- NOTE : this implementation below isn't correct, because the new arrays can get CSE-d out. 
--fromList :: [a] -> Vector a
--fromList xs = case go (# 0#, 0#, AA.new NODE_WIDTH, A.new NODE_WIDTH undefElem #) xs of
--    (# size, level, init, tail #) -> Vector size level init tail
--    where go acc (x:xs) = go (unsafeSnoc acc x) xs
--          go acc []     = acc 
--{-# NOINLINE fromList #-}



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

        notfull :: Int# -> Int# -> ArrayArray# -> b -> b 
        notfull lasti level arr z = case level ># 0# of
            1# -> AA.foldr lasti' (full level') (notfull lasti level' (AA.index arr lasti') z) arr
            _  -> A.foldr NODE_WIDTH f z (aa2a arr)
            where lasti' = index lasti level
                  level' = next level

        full :: Int# -> ArrayArray# -> b -> b
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

        notfull :: Int# -> Int# -> ArrayArray# -> b -> b 
        notfull lasti level arr z = case level ># 0# of
            1# -> notfull lasti level' (AA.index arr lasti') (AA.rfoldr lasti' (full level') z arr)
            _  -> A.rfoldr NODE_WIDTH f z (aa2a arr)
            where lasti' = index lasti level
                  level' = next level

        full :: Int# -> ArrayArray# -> b -> b
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

        notfull :: Int# -> Int# -> ArrayArray# -> b -> b 
        notfull lasti level arr z = case level ># 0# of
            1# -> notfull lasti level' (AA.index arr lasti') (AA.foldl' lasti' (full level') z arr)
            _  -> A.foldl' width f z (aa2a arr)
            where lasti' = index lasti level
                  level' = next level
                  width = NODE_WIDTH

        full :: Int# -> b -> ArrayArray# -> b
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

        notfull :: Int# -> Int# -> ArrayArray# -> b -> b 
        notfull lasti level arr z = case level ># 0# of
            1# -> AA.rfoldl' lasti' (full level') (notfull lasti level' (AA.index arr lasti') z) arr
            _  -> A.rfoldl' width f z (aa2a arr)
            where lasti' = index lasti level
                  level' = next level
                  width = NODE_WIDTH

        full :: Int# -> b -> ArrayArray# -> b
        full level z arr = case level ># 0# of
            1# -> AA.rfoldl' width (full (next level)) z arr
            _  -> A.rfoldl' width f z (aa2a arr)
            where width = NODE_WIDTH
{-# INLINE rfoldl' #-}

map :: forall a b. (a -> b) -> Vector a -> Vector b 
map f s@(Vector 0# level init tail) = unsafeCoerce# s
map f (Vector size level init tail) = Vector size level init' tail' where

    tailSize = andI# size KEY_MASK
    initSize = size -# tailSize
    init' = notfull (initSize -# 1#) level init
    tail' = A.map tailSize f tail

    notfull :: Int# -> Int# -> ArrayArray# -> ArrayArray#
    notfull lasti level arr = case level ># 0# of
        1# -> AA.mapInitLast' lasti' (full level') (notfull lasti level') arr 
        _  -> a2aa (A.map NODE_WIDTH f (aa2a arr))
        where lasti' = index lasti level
              level' = next level

    full :: Int# -> ArrayArray# -> ArrayArray#
    full level arr = case level ># 0# of
        1# -> AA.map' width (full (next level)) arr
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
            1# -> AA.modify' width arr (index i level) (go i (next level))
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
            1# -> AA.modify' width arr (index i level) (go i (next level))
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
    !emptyAA = AA.new 0#
    !emptyTail = A.new NODE_WIDTH undefElem

singleton :: a -> Vector a
singleton a = Vector 1# 0# (_init empty) (init1A a)
{-# INLINE singleton #-}

length :: Vector a -> Int
length (Vector size _ _ _) = I# size
{-# INLINE length #-}

toList :: Vector a -> [a]
toList = Vector.foldr (:) []
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

aa2a :: ArrayArray# -> Array# a
aa2a = unsafeCoerce#
{-# INLINE aa2a #-}

a2aa :: Array# a -> ArrayArray#
a2aa = unsafeCoerce#
{-# INLINE a2aa #-}

undefElem :: a
undefElem = error "Vector: undefined element"
{-# NOINLINE undefElem #-}

init1A :: a -> Array# a
init1A a = A.init1 NODE_WIDTH a undefElem
{-# INLINE init1A #-}

init1AA :: ArrayArray# -> ArrayArray#
init1AA a = AA.init1 NODE_WIDTH a undefElem
{-# INLINE init1AA #-}

init2AA :: ArrayArray# -> ArrayArray# -> ArrayArray#
init2AA a1 a2 = AA.init2 NODE_WIDTH a1 a2 undefElem
{-# INLINE init2AA #-}


-- Bounds checked functions ---------------------------------------------------

--AA.modify NODE_WIDTH :: ArrayArray# -> Int# -> (ArrayArray# -> ArrayArray#) -> ArrayArray#
--AA.modify NODE_WIDTH arr i a = let size = NODE_WIDTH in case i <# sizeofArrayArray# arr of
--    1# -> AA.modify' size arr i a 
--    _  -> error $ "AA.modify NODE_WIDTH: out of bound: " ++ show (I# i) ++ " " ++ show (I# (sizeofArrayArray# arr))
--  {-# INLINE AA.modify NODE_WIDTH #-}

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

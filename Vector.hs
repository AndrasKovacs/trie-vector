
{-# LANGUAGE MagicHash, BangPatterns, CPP, RankNTypes, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-full-laziness #-}

module Vector (
      Vector(..)
    , (|>)
    , (!#)
    , unsafeIndex#
    , (!)
    , unsafeIndex
    , snoc
    , Vector.foldr
    , Vector.foldl
    , foldl'
    , rfoldr
    , rfoldl
    , rfoldl'
    , Vector.map
    , modify
    , modify#
    , unsafeModify#
    , unsafeModify
    , singleton
    , empty
    , Vector.length
    , toList ) where

import qualified Data.Foldable as F
import qualified Data.Traversable as T 
import GHC.Prim
import GHC.Types

import qualified Array as A
import qualified ArrayArray as AA

#define NODE_WIDTH 16#
#define KEY_BITS 4#
#define KEY_MASK 15#


main = do
    print $ rfoldr (:) [] $ Prelude.foldl snoc empty [0..10] 


data Vector a = Vector {
    _size, _level :: Int#,
    _arr :: ArrayArray#,
    _tail :: Array# a}

instance Functor Vector where
    fmap = Vector.map
    {-# INLINABLE fmap #-}

instance Show a => Show (Vector a) where
    show seq = "fromList " ++ show (toList seq)

instance F.Foldable Vector where
    foldr = Vector.foldr
    {-# INLINE foldr #-}

instance T.Traversable Vector where
    traverse f (Vector s l arr tail) = error "TODO: not implemented" 


(|>) :: Vector a -> a -> Vector a
(|>) = snoc
infixl 5 |>
{-# INLINE (|>) #-}


(!) :: forall a. Vector a -> Int -> a
(!) v (I# i) = v !# i
{-# INLINE (!) #-}

(!#) :: forall a. Vector a -> Int# -> a
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
            _  -> A.index (aa2a arr) (index i level)
infixl 5 !
{-# INLINE (!#) #-}

unsafeIndex :: Vector a -> Int -> a
unsafeIndex v (I# i) = unsafeIndex# v i
{-# INLINE unsafeIndex #-}

unsafeIndex# :: forall a. Vector a -> Int# -> a
unsafeIndex# (Vector size level arr tail) i = let
    tailSize = andI# size KEY_MASK
    initSize = size -# tailSize
    in case i <# initSize of
        1# -> go i level arr
        _  -> A.index tail (i -# initSize)
    where go i level arr = case level ># 0# of
            1# -> go i (next level) (AA.index arr (index i level))
            _  -> A.index (aa2a arr) (index i level)
{-# INLINE unsafeIndex# #-}

snoc :: forall a. Vector a -> a -> Vector a
snoc (Vector size level init tail) v = let
    tailSize  = andI# size KEY_MASK
    initSize  = size -# tailSize
    size'     = size +# 1#
    tail'     = updateA tail tailSize v

    snocArr :: ArrayArray# -> Int# -> Int# -> Int# -> ArrayArray# -> ArrayArray#
    snocArr arr mask i level init = case level ># 0# of
        1# -> case andI# i mask ==# 0# of 
            0# -> modifyAA init (index i level) (snocArr arr (nextMask mask) i (next level))
            _  -> init1AA (snocArr arr (nextMask mask) i (next level) (_arr empty))
        _ -> arr

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
rfoldr f z (Vector size level arr tail) = case initSize ==# 0# of
    0# -> A.rfoldr tailSize f (notfull (initSize -# 1#) level arr z) tail
    _  -> A.rfoldr tailSize f z tail 
    where
        tailSize = andI# size KEY_MASK
        initSize = size -# tailSize

        notfull :: Int# -> Int# -> ArrayArray# -> b -> b 
        notfull lasti level arr z = case level ># 0# of
            1# -> AA.rfoldr lasti' (full level') (notfull lasti level' (AA.index arr lasti') z) arr
            _  -> A.rfoldr NODE_WIDTH f z (aa2a arr)
            where lasti' = index lasti level
                  level' = next level

        full :: Int# -> ArrayArray# -> b -> b
        full level arr z = case level ># 0# of
            1# -> AA.rfoldr NODE_WIDTH (full (next level)) z arr
            _  -> A.rfoldr NODE_WIDTH f z (aa2a arr)
{-# INLINE rfoldr #-}


foldl' :: forall a b. (b -> a -> b) -> b -> Vector a -> b 
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
rfoldl' f z (Vector size level arr tail) = case initSize ==# 0# of
    0# -> A.rfoldl' tailSize f (notfull (initSize -# 1#) level arr z) tail  
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


foldl :: forall a b. (b -> a -> b) -> b -> Vector a -> b 
foldl f z (Vector size level arr tail) = case initSize ==# 0# of
    0# -> notfull (initSize -# 1#) level arr tailRes
    _  -> tailRes
    where
        tailSize = andI# size KEY_MASK
        initSize = size -# tailSize
        tailRes = A.foldl tailSize f z tail 

        notfull :: Int# -> Int# -> ArrayArray# -> b -> b 
        notfull lasti level arr z = case level ># 0# of
            1# -> AA.foldl lasti' (full level') (notfull lasti level' (AA.index arr lasti') z) arr
            _  -> A.foldl NODE_WIDTH f z (aa2a arr)
            where lasti' = index lasti level
                  level' = next level

        full :: Int# -> b -> ArrayArray# -> b
        full level z arr = case level ># 0# of
            1# -> AA.foldl NODE_WIDTH (full (next level)) z arr
            _  -> A.foldl NODE_WIDTH f z (aa2a arr)
{-# INLINE foldl #-}


rfoldl :: forall a b. (b -> a -> b) -> b -> Vector a -> b 
rfoldl f z (Vector size level arr tail) = case initSize ==# 0# of
    0# -> A.rfoldl tailSize f (notfull (initSize -# 1#) level arr z) tail  
    _  -> A.rfoldl tailSize f z tail 
    where
        tailSize = andI# size KEY_MASK
        initSize = size -# tailSize

        notfull :: Int# -> Int# -> ArrayArray# -> b -> b 
        notfull lasti level arr z = case level ># 0# of
            1# -> AA.rfoldl lasti' (full level') (notfull lasti level' (AA.index arr lasti') z) arr
            _  -> A.rfoldl width f z (aa2a arr)
            where lasti' = index lasti level
                  level' = next level
                  width = NODE_WIDTH

        full :: Int# -> b -> ArrayArray# -> b
        full level z arr = case level ># 0# of
            1# -> AA.rfoldl width (full (next level)) z arr
            _  -> A.rfoldl width f z (aa2a arr)
            where width = NODE_WIDTH
{-# INLINE rfoldl #-}

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
singleton a = Vector 1# 0# (_arr empty) (init1A a)
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

checkBound :: Int# -> Int# -> Int#
checkBound size i = andI# (i >=# 0#) (i <# size)
{-# INLINE checkBound #-}

aa2a :: ArrayArray# -> Array# a
aa2a = unsafeCoerce#
{-# INLINE aa2a #-}

a2aa :: Array# a -> ArrayArray#
a2aa = unsafeCoerce#
{-# INLINE a2aa #-}

undefElem :: a
undefElem = error "Vector: undefined element"
{-# NOINLINE undefElem #-}

modifyAA :: ArrayArray# -> Int# -> (ArrayArray# -> ArrayArray#) -> ArrayArray#
modifyAA arr i a = let size = NODE_WIDTH in AA.modify' size arr i a 
{-# INLINE modifyAA #-}

updateA :: Array# a -> Int# -> a -> Array# a
updateA arr i a = let size = NODE_WIDTH in A.update size arr i a
{-# INLINE updateA #-}

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

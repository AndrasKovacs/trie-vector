

{-# LANGUAGE MagicHash, BangPatterns, UnboxedTuples, CPP, RankNTypes, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-full-laziness #-}

module ArrayArray16 where

import qualified Data.Foldable as F
import GHC.Prim
import GHC.Types
import Debug.Trace
import Data.Primitive

import qualified Array as A
import qualified ArrayArray as AA


#define NODE_WIDTH 16#
#define KEY_BITS 4#
#define KEY_MASK 15#


main = do
    let r = [0..10]
        s = Prelude.foldl snoc empty r

    print $ s `unsafeIndex` 4

    print $ Prelude.foldl snoc empty r


data Seq a = Seq {
    _size, _level :: Int#,
    _arr :: ArrayArray#,
    _tail :: Array# a}

(|>) :: Seq a -> a -> Seq a
(|>) = snoc
infixl 5 |>
{-# INLINE (|>) #-}

(!) :: forall a. Seq a -> Int -> a
(!) (Seq size level arr tail) (I# i) = case i >=# 0# of 
    1# -> let
        tailSize = andI# size KEY_MASK
        initSize = size -# tailSize
        in case i <# initSize of
            1# -> go i level arr
            _  -> case i <# size of
                1# -> A.index tail (i -# initSize)
                _  -> error "Seq.!: out of bounds"
    _  -> error "Seq.!: out of bounds"
    where go i level arr = case level ># 0# of
            1# -> go i (next level) (AA.index arr (index i level))
            _  -> A.index (aa2a arr) (index i level)
infixl 5 !
{-# INLINE (!) #-}

unsafeIndex :: forall a. Seq a -> Int -> a
unsafeIndex (Seq size level arr tail) (I# i) = let
    tailSize = andI# size KEY_MASK
    initSize = size -# tailSize
    in case i <# initSize of
        1# -> go i level arr
        _  -> A.index tail (i -# initSize)
    where go i level arr = case level ># 0# of
            1# -> go i (next level) (AA.index arr (index i level))
            _  -> A.index (aa2a arr) (index i level)
{-# INLINE unsafeIndex #-}

snoc :: forall a. Seq a -> a -> Seq a
snoc (Seq size level init tail) v = let
    tailSize  = andI# size KEY_MASK
    initSize  = size -# tailSize
    size'     = size +# 1#
    tail'     = updateA tail tailSize v

    insertArr :: ArrayArray# -> Int# -> Int# -> Int# -> ArrayArray# -> ArrayArray#
    insertArr arr mask i level init = case level ># 0# of
        1# -> case andI# i mask ==# 0# of 
            0# -> modifyAA init (index i level) (insertArr arr (nextMask mask) i (next level))
            _  -> init1AA (insertArr arr (nextMask mask) i (next level) (_arr empty))
        _ -> arr

    in case tailSize ==# KEY_MASK of
        0# -> Seq size' level init tail'
        _  -> let
            mask      = maxSize -# 1#
            prevLevel = level +# KEY_BITS
            maxSize   = uncheckedIShiftL# 1# prevLevel
            init'     = insertArr (a2aa tail') mask initSize level init
            in case initSize ==# maxSize of
                0# -> Seq size' level init' (_tail empty)
                _  -> Seq size' prevLevel (init2AA init init') (_tail empty)
{-# INLINE snoc #-}


foldr :: forall a b. (a -> b -> b) -> b -> Seq a -> b 
foldr f z (Seq size level arr tail) = case initSize ==# 0# of
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

foldl' :: forall a b. (b -> a -> b) -> b -> Seq a -> b 
foldl' f z (Seq size level arr tail) = case initSize ==# 0# of
    0# -> notfull (initSize -# 1#) level arr tailRes
    _  -> tailRes
    where
        tailSize = andI# size KEY_MASK
        initSize = size -# tailSize
        tailRes = A.foldl' tailSize f z tail 

        notfull :: Int# -> Int# -> ArrayArray# -> b -> b 
        notfull lasti level arr z = case level ># 0# of
            1# -> AA.foldl lasti' (full level') (notfull lasti level' (AA.index arr lasti') z) arr
            _  -> A.foldl' width f z (aa2a arr)
            where lasti' = index lasti level
                  level' = next level
                  width = NODE_WIDTH

        full :: Int# -> b -> ArrayArray# -> b
        full level z arr = case level ># 0# of
            1# -> AA.foldl width (full (next level)) z arr
            _  -> A.foldl' width f z (aa2a arr)
            where width = NODE_WIDTH
{-# INLINE foldl' #-}


foldl :: forall a b. (b -> a -> b) -> b -> Seq a -> b 
foldl f z (Seq size level arr tail) = case initSize ==# 0# of
    0# -> notfull (initSize -# 1#) level arr tailRes
    _  -> tailRes
    where
        tailSize = andI# size KEY_MASK
        initSize = size -# tailSize
        tailRes = A.foldl' tailSize f z tail 

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

map :: forall a b. (a -> b) -> Seq a -> Seq b 
map f s@(Seq 0# level init tail) = unsafeCoerce# s
map f (Seq size level init tail) = Seq size level init' tail' where

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

modify :: forall a. Seq a -> Int -> (a -> a) -> Seq a 
modify (Seq size level init tail) (I# i) f = case i >=# 0# of 
    1# -> let
        tailSize = andI# size KEY_MASK
        initSize = size -# tailSize
        in case i <# initSize of
            1# -> Seq size level (go i level init) tail
            _  -> case i <# size of
                1# -> Seq size level init (A.modify NODE_WIDTH tail (i -# initSize) f)
                _  -> error "Seq.!: out of bounds"
    _  -> error "Seq.!: out of bounds"
    where go i level arr = case level ># 0# of
            1# -> AA.modify' width arr (index i level) (go i (next level))
            _  -> a2aa (A.modify NODE_WIDTH (aa2a arr) (index i level) f)
            where width = NODE_WIDTH
{-# INLINE modify #-}

empty :: Seq a
empty = Seq 0# 0# emptyAA emptyTail where
    !emptyAA = AA.new 0#
    !emptyTail = A.new NODE_WIDTH undefElem

singleton :: a -> Seq a
singleton a = Seq 1# 0# (_arr empty) (init1A a)
{-# INLINE singleton #-}

length :: Seq a -> Int
length (Seq size _ _ _) = I# size
{-# INLINE length #-}

toList :: Seq a -> [a]
toList = ArrayArray16.foldr (:) []
{-# INLINE toList #-}


instance Functor Seq where
    fmap = ArrayArray16.map
    {-# INLINE fmap #-}

instance Show a => Show (Seq a) where
    show seq = "fromList " ++ show (toList seq)

















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
undefElem = error "Seq: undefined element"

-- Remove bounds check later --


modifyAA :: ArrayArray# -> Int# -> (ArrayArray# -> ArrayArray#) -> ArrayArray#
modifyAA arr i a = let size = NODE_WIDTH in AA.modify' size arr i a 
{-# INLINE modifyAA #-}

updateA :: Array# a -> Int# -> a -> Array# a
updateA arr i a = let size = NODE_WIDTH in A.update size arr i a
{-# INLINE updateA #-}

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

-------------------------------

indexA arr i = case i <# sizeofArray# arr of
    1# -> A.index arr i
    _  -> error "indexA: out of bounds"

indexAA arr i = case i <# sizeofArrayArray# arr of
    1# -> AA.index arr i
    _  -> error "indexAA: out of bounds"

init1A :: a -> Array# a
init1A a = A.init1 NODE_WIDTH a undefElem
{-# INLINE init1A #-}

init1AA :: ArrayArray# -> ArrayArray#
init1AA a = AA.init1 NODE_WIDTH a undefElem
{-# INLINE init1AA #-}

init2AA :: ArrayArray# -> ArrayArray# -> ArrayArray#
init2AA a1 a2 = AA.init2 NODE_WIDTH a1 a2 undefElem
{-# INLINE init2AA #-}

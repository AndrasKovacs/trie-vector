

{-# LANGUAGE MagicHash, BangPatterns, UnboxedTuples, CPP, RankNTypes, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-full-laziness #-}

module ArrayArrayUnboxed where

import qualified Data.Foldable as F

import GHC.Prim
import GHC.Types
import Debug.Trace
import Data.Primitive

import qualified ArrayArray as AA
import qualified ByteArray as BA

#define NODE_WIDTH 16#
#define KEY_BITS 4#
#define KEY_MASK 15#

-- NOTE: performance: is it better to use ArrayArray's new for initializing?
-- might have better data locality for initial element. 

main = do
    print $ ArrayArrayUnboxed.map succ $ Prelude.foldl snoc empty [0..100::Int] 


data Seq a = Seq {
    _size, _level :: Int#,
    _arr :: ArrayArray# }

(|>) :: Prim a => Seq a -> a -> Seq a
(|>) = snoc
infixl 5 |>
{-# INLINABLE (|>) #-}

(!) :: Prim a => Seq a -> Int -> a
(!) (Seq size level arr) (I# i) = case checkBound size i of
    1# -> go i level arr
    _  -> error "Seq.!: out of bounds"
    where go i level arr = case level ># 0# of
            1# -> go i (next level) (AA.index arr (index i level))
            _  -> BA.index (aa2ba arr) (index i level)
infixl 5 !
{-# INLINABLE (!) #-}

unsafeIndex :: Prim a => Seq a -> Int -> a
unsafeIndex (Seq size level arr) (I# i) = go i level arr where
    go i level arr = case level ># 0# of
        1# -> go i (next level) (AA.index arr (index i level))
        _  -> BA.index (aa2ba arr) (index i level)
{-# INLINABLE unsafeIndex #-}

snoc :: forall a. Prim a => Seq a -> a -> Seq a
snoc !(Seq size level arr) v = case size ==# maxSize of 
    0# -> Seq (size +# 1#) level (go v mask size level arr)
    _  -> Seq (size +# 1#) prevLevel (init2AA arr (go v mask size level (_arr empty)))
    where
        prevLevel = level +# KEY_BITS
        maxSize   = uncheckedIShiftL# 1# prevLevel
        mask      = maxSize -# 1# 

        go :: a -> Int# -> Int# -> Int# -> ArrayArray# -> ArrayArray#
        go v mask size level arr = case level ># 0# of
            1# -> case andI# size mask ==# 0# of 
                0# -> modifyAA arr i (go v (nextMask mask) size (next level))
                _  -> init1AA (go v (nextMask mask) size (next level) (_arr empty))
            _ ->  case i ==# 0# of
                0# -> ba2aa (updateBA (aa2ba arr) i v)
                _  -> ba2aa (init1BA v)
            where i = index size level
{-# INLINABLE snoc #-}


foldr :: forall a b. Prim a => (a -> b -> b) -> b -> Seq a -> b 
foldr f z (Seq 0#   _     _  ) = z 
foldr f z (Seq size level arr) = notfull (size -# 1#) level arr z where

    notfull :: Int# -> Int# -> ArrayArray# -> b -> b 
    notfull lasti level arr z = case level ># 0# of
        1# -> AA.foldr lasti' (full level') (notfull lasti level' (AA.index arr lasti') z) arr
        _  -> BA.foldr (lasti' +# 1#) f z (aa2ba arr)
        where lasti' = index lasti level
              level' = next level

    full :: Int# -> ArrayArray# -> b -> b
    full level arr z = case level ># 0# of
        1# -> AA.foldr NODE_WIDTH (full (next level)) z arr
        _  -> BA.foldr NODE_WIDTH f z (aa2ba arr)
{-# INLINABLE foldr #-}


-- strict in the accumulator
foldl' :: forall a b. Prim a => (b -> a -> b) -> b -> Seq a -> b 
foldl' f z (Seq 0#   _     _  ) = z 
foldl' f z (Seq size level arr) = notfull (size -# 1#) level arr z where

    notfull :: Int# -> Int# -> ArrayArray# -> b -> b 
    notfull lasti level arr !z = case level ># 0# of
        1# -> AA.foldl' lasti' (full level') (notfull lasti level' (AA.index arr lasti') z) arr
        _  -> BA.foldl' (lasti' +# 1#) f z (aa2ba arr)
        where lasti' = index lasti level
              level' = next level

    full :: Int# -> b -> ArrayArray# -> b
    full level !z arr = case level ># 0# of
        1# -> AA.foldl' width (full (next level)) z arr
        _  -> BA.foldl' width f z (aa2ba arr)
        where width = NODE_WIDTH
{-# INLINABLE foldl' #-}


foldl :: forall a b. Prim a => (b -> a -> b) -> b -> Seq a -> b 
foldl f z (Seq 0#   _     _  ) = z 
foldl f z (Seq size level arr) = notfull (size -# 1#) level arr z where

    notfull :: Int# -> Int# -> ArrayArray# -> b -> b 
    notfull lasti level arr z = case level ># 0# of
        1# -> AA.foldl lasti' (full level') (notfull lasti level' (AA.index arr lasti') z) arr
        _  -> BA.foldl (lasti' +# 1#) f z (aa2ba arr)
        where lasti' = index lasti level
              level' = next level

    full :: Int# -> b -> ArrayArray# -> b
    full level z arr = case level ># 0# of
        1# -> AA.foldl NODE_WIDTH (full (next level)) z arr
        _  -> BA.foldl NODE_WIDTH f z (aa2ba arr)
{-# INLINABLE foldl #-}


map :: forall a b. (Prim a, Prim b) => (a -> b) -> Seq a -> Seq b 
map f s@(Seq 0# level arr) = unsafeCoerce# s
map f (Seq size level arr) = Seq size level (notfull (size -# 1#) level arr) where

    notfull :: Int# -> Int# -> ArrayArray# -> ArrayArray#
    notfull lasti level arr = case level ># 0# of
        1# -> AA.mapInitLast' lasti' (full level') (notfull lasti level') arr 
        _  -> ba2aa (BA.map (lasti' +# 1#) f (aa2ba arr))
        where lasti' = index lasti level
              level' = next level

    full :: Int# -> ArrayArray# -> ArrayArray#
    full level arr = case level ># 0# of
        1# -> AA.map' width (full (next level)) arr
        _  -> ba2aa (BA.map width f (aa2ba arr))
        where width = NODE_WIDTH
{-# INLINABLE map #-}

modify :: Prim a => Seq a -> Int -> (a -> a) -> Seq a
modify (Seq size level arr) (I# i) f = case checkBound size i of
    1# -> Seq size level (go i level arr) 
    _  -> error "Seq.modify: out of bounds"
    where
        go :: Int# -> Int# -> ArrayArray# -> ArrayArray#
        go i level arr = case level ># 0# of
            1# -> AA.modify' width arr (index i level) (go i (next level))
            _  -> ba2aa (BA.modify' width (aa2ba arr) (index i level) f)
            where width = NODE_WIDTH
{-# INLINABLE modify #-}

empty :: Seq a
empty = Seq 0# 0# emptyAA where
    emptyAA = AA.new 0#

singleton :: Prim a => a -> Seq a
singleton a = Seq 1# 0# (ba2aa (init1BA a))
{-# INLINABLE singleton #-}

length :: Seq a -> Int
length (Seq size _ _) = I# size
{-# INLINABLE length #-}

toList :: Prim a => Seq a -> [a]
toList = ArrayArrayUnboxed.foldr (:) []
{-# INLINABLE toList #-}

instance (Prim a, Show a) => Show (Seq a) where
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

aa2ba :: ArrayArray# -> ByteArray#
aa2ba = unsafeCoerce#
{-# INLINE aa2ba #-}

ba2aa :: ByteArray# -> ArrayArray#
ba2aa = unsafeCoerce#
{-# INLINE ba2aa #-}

undefElem :: a
undefElem = error "Seq: undefined element"

---- Remove bounds check later --

--modifyAA :: ArrayArray# -> Int# -> (ArrayArray# -> ArrayArray#) -> ArrayArray#
--modifyAA arr i a = let size = NODE_WIDTH in AA.modify' size arr i a 
-- {-# INLINE modifyAA #-}

--updateBA :: Prim a => ByteArray# -> Int# -> a -> ByteArray#
--updateBA arr i a = let size = NODE_WIDTH in BA.update size arr i a
-- {-# INLINE updateBA #-}

modifyAA :: ArrayArray# -> Int# -> (ArrayArray# -> ArrayArray#) -> ArrayArray#
modifyAA arr i a = let size = NODE_WIDTH in case i <# sizeofArrayArray# arr of
    1# -> AA.modify' size arr i a 
    _  -> error $ "modifyAA: out of bound: " ++ show (I# i) ++ " " ++ show (I# (sizeofArrayArray# arr))
{-# INLINE modifyAA #-}

updateBA :: Prim a => ByteArray# -> Int# -> a -> ByteArray#
updateBA arr i a = let size = NODE_WIDTH in case i <# sizeofByteArray# arr of
    1# -> BA.update size arr i a
    _  -> error $ "updateBA: out of bound: " ++ show (I# i) ++ " " ++ show (I# (sizeofByteArray# arr))
{-# INLINE updateBA #-}

---------------------------------

init1BA :: Prim a => a -> ByteArray#
init1BA a = BA.init1 NODE_WIDTH a
{-# INLINE init1BA #-}

init1AA :: ArrayArray# -> ArrayArray#
init1AA a = AA.init1 NODE_WIDTH a undefElem
{-# INLINE init1AA #-}

init2AA :: ArrayArray# -> ArrayArray# -> ArrayArray#
init2AA a1 a2 = AA.init2 NODE_WIDTH a1 a2 undefElem
{-# INLINE init2AA #-}

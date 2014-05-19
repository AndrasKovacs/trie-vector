

{-# LANGUAGE MagicHash, BangPatterns, UnboxedTuples, CPP, RankNTypes, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-full-laziness #-}

module ArrayArray32 where


import qualified Data.Foldable as F
import GHC.Prim
import GHC.Types
import Debug.Trace

import qualified Array as A
import qualified ArrayArray as AA

#define NODE_WIDTH 32#
#define KEY_BITS 5#
#define KEY_MASK 31#


-- NOTE: performance: is it better to use ArrayArray's new for initializing?
-- might have better data locality for initial element. 


--main = do
--    --print $ toList (singleton [0..10])
--    let !s = Prelude.foldl snoc empty [0..100]
--    print $ ArrayArray32.foldl' (+) 0 s 
--    print $ s 
--    --print $ ArrayArray32.foldr (:) [] $ fmap (*10) $ Prelude.foldl snoc empty [0..10]

data Seq a = Seq {
    _size, _level :: Int#,
    _arr :: ArrayArray# }

(|>) :: Seq a -> a -> Seq a
(|>) = snoc
infixl 5 |>
{-# INLINE (|>) #-}

(!) :: forall a. Seq a -> Int -> a
(!) (Seq size level arr) (I# i) = case andI# (i <# size) (i >=# 0#) of
    1# -> go i level arr
    _  -> error "Seq.!: out of bounds"
    where go i level arr = case level ># 0# of
            1# -> go i (next level) (AA.index arr (index i level))
            _  -> A.index (aa2a arr) (index i level)
infixl 5 !
{-# INLINE (!) #-}

snoc :: forall a. Seq a -> a -> Seq a
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
                0# -> a2aa (updateA (aa2a arr) i v)
                _  -> a2aa (init1A v)
            where i = index size level
{-# INLINE snoc #-}


foldr :: forall a b. (a -> b -> b) -> b -> Seq a -> b 
foldr f z (Seq 0#   _     _  ) = z 
foldr f z (Seq size level arr) = notfull (size -# 1#) level arr z where

    -- descend the right edge of the trie
    notfull :: Int# -> Int# -> ArrayArray# -> b -> b 
    notfull lasti level arr z = case level ># 0# of
        1# -> AA.foldr lasti' (full level') (notfull lasti level' (AA.index arr lasti') z) arr
        _  -> A.foldr (lasti' +# 1#) f z (aa2a arr)
        where lasti' = index lasti level
              level' = next level

    -- descend the interior of the trie, where all nodes are full
    full :: Int# -> ArrayArray# -> b -> b
    full level arr z = case level ># 0# of
        1# -> AA.foldr NODE_WIDTH (full (next level)) z arr
        _  -> A.foldr NODE_WIDTH f z (aa2a arr)
{-# INLINE foldr #-}


-- strict in the accumulator
foldl' :: forall a b. (b -> a -> b) -> b -> Seq a -> b 
foldl' f z (Seq 0#   _     _  ) = z 
foldl' f z (Seq size level arr) = notfull (size -# 1#) level arr z where

    notfull :: Int# -> Int# -> ArrayArray# -> b -> b 
    notfull lasti level arr !z = case level ># 0# of
        1# -> AA.foldl' lasti' (full level') (notfull lasti level' (AA.index arr lasti') z) arr
        _  -> A.foldl' (lasti' +# 1#) f z (aa2a arr)
        where lasti' = index lasti level
              level' = next level

    full :: Int# -> b -> ArrayArray# -> b
    full level !z arr = case level ># 0# of
        1# -> AA.foldl' width (full (next level)) z arr
        _  -> A.foldl' width f z (aa2a arr)
        where width = NODE_WIDTH
{-# INLINE foldl' #-}


foldl :: forall a b. (b -> a -> b) -> b -> Seq a -> b 
foldl f z (Seq 0#   _     _  ) = z 
foldl f z (Seq size level arr) = notfull (size -# 1#) level arr z where

    notfull :: Int# -> Int# -> ArrayArray# -> b -> b 
    notfull lasti level arr z = case level ># 0# of
        1# -> AA.foldl lasti' (full level') (notfull lasti level' (AA.index arr lasti') z) arr
        _  -> A.foldl (lasti' +# 1#) f z (aa2a arr)
        where lasti' = index lasti level
              level' = next level

    full :: Int# -> b -> ArrayArray# -> b
    full level z arr = case level ># 0# of
        1# -> AA.foldl NODE_WIDTH (full (next level)) z arr
        _  -> A.foldl NODE_WIDTH f z (aa2a arr)
{-# INLINE foldl #-}


map :: forall a b. (a -> b) -> Seq a -> Seq b 
map f s@(Seq 0# level arr) = unsafeCoerce# s
map f (Seq size level arr) = Seq size level (notfull (size -# 1#) level arr) where

    notfull :: Int# -> Int# -> ArrayArray# -> ArrayArray#
    notfull lasti level arr = case level ># 0# of
        1# -> AA.mapInitLast' lasti' (full level') (notfull lasti level') arr 
        _  -> a2aa (A.map (lasti' +# 1#) f (aa2a arr))
        where lasti' = index lasti level
              level' = next level

    full :: Int# -> ArrayArray# -> ArrayArray#
    full level arr = case level ># 0# of
        1# -> AA.map' width (full (next level)) arr
        _  -> a2aa (A.map width f (aa2a arr))
        where width = NODE_WIDTH
{-# INLINE map #-}


empty :: Seq a
empty = Seq 0# 0# emptyAA where
    emptyAA = AA.new 0#

singleton :: a -> Seq a
singleton a = Seq 1# 0# (a2aa (init1A a))
{-# INLINE singleton #-}

length :: Seq a -> Int
length (Seq size _ _) = I# size
{-# INLINE length #-}

toList :: Seq a -> [a]
toList = ArrayArray32.foldr (:) []
{-# INLINE toList #-}


instance Functor Seq where
    fmap = ArrayArray32.map
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
modifyAA arr i a = let size = NODE_WIDTH in case i <# sizeofArrayArray# arr of
    1# -> AA.modify' size arr i a 
    _  -> error $ "modifyAA: out of bound: " ++ show (I# i) ++ " " ++ show (I# (sizeofArrayArray# arr))
{-# INLINE modifyAA #-}

updateA :: Array# a -> Int# -> a -> Array# a
updateA arr i a = let size = NODE_WIDTH in case i <# sizeofArray# arr of
    1# -> A.update size arr i a
    _  -> error $ "updateA: out of bound: " ++ show (I# i) ++ " " ++ show (I# (sizeofArray# arr))
{-# INLINE updateA #-}

-------------------------------



init1A :: a -> Array# a
init1A a = A.init1 NODE_WIDTH a undefElem
{-# INLINE init1A #-}

init1AA :: ArrayArray# -> ArrayArray#
init1AA a = AA.init1 NODE_WIDTH a undefElem
{-# INLINE init1AA #-}

init2AA :: ArrayArray# -> ArrayArray# -> ArrayArray#
init2AA a1 a2 = AA.init2 NODE_WIDTH a1 a2 undefElem
{-# INLINE init2AA #-}

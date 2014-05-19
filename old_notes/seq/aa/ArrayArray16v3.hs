

{-# LANGUAGE MagicHash, BangPatterns, UnboxedTuples, CPP, RankNTypes, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-full-laziness #-}

import GHC.Prim
import GHC.Types
import qualified Array as A
import qualified ArrayArray as AA
import Debug.Trace

#define NODE_WIDTH 16#
#define KEY_BITS 4#
#define KEY_MASK 15#



-- NOTE: performance: is it better to use ArrayArray's new for initializing?
-- might have better data locality for initial element. 

-- Does strictness have any effect on unlifted args?


main = do
    --print $ toList (singleton [0..10])
    let !s = Prelude.foldl snoc empty [0..100]
    print $ Main.foldr (:) [] s 





data Seq a = Seq {
    _size, _level :: Int#,
    _arr :: ArrayArray# }

(|>) :: Show a => Seq a -> a -> Seq a
(|>) = snoc
infixl 5 |>
{-# INLINE (|>) #-}

snoc :: forall a. Show a => Seq a -> a -> Seq a
snoc !(Seq size level arr) v = case size ==# maxSize of 
    0# -> Seq (size +# 1#) level (go v initMask size level arr)
    _  -> Seq (size +# 1#) prevLevel (init2AA arr (go v initMask size level (_arr empty)))
    where
        prevLevel    = level +# KEY_BITS
        maxSize      = uncheckedIShiftL# 1# prevLevel
        initMask     = maxSize -# 1# 

        go :: a -> Int# -> Int# -> Int# -> ArrayArray# -> ArrayArray#
        go v mask size level arr = case level ># 0# of
            1# -> case andI# size mask ==# 0# of 
                0# -> modifyAA arr i (go v (nextMask mask) size (next level))
                _  -> init1AA (go v (nextMask mask) size (next level) (_arr empty))
            _ ->  case i ==# 0# of
                0# -> a2aa (updateA (aa2a arr) i v)
                _  -> a2aa (init1A v)
            where i = index size level
{-# INLINABLE snoc #-}


foldr :: forall a b. Show a => (a -> b -> b) -> b -> Seq a -> b 
foldr f z !(Seq 0#   _     _  ) = z 
foldr f z  (Seq size level arr) = notfull (size -# 1#) level arr z where

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


empty :: Seq a
empty = Seq 0# 0# emptyAA where
    emptyAA = AA.new 0#

singleton :: a -> Seq a
singleton a = Seq 1# 0# (a2aa (init1A a))
{-# INLINE singleton #-}


length :: Seq a -> Int
length !(Seq size _ _) = I# size
{-# INLINE length #-}















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

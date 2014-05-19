

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


main = do
    --print $ toList (singleton [0..10])
    let !s = Prelude.foldl snoc empty [0..16]

    print $ Main.foldr (:) [] s
    print 0




data Seq a = Seq {
    _size, _level :: Int#,
    _arr :: !ArrayArray# }

(|>) :: Show a => Seq a -> a -> Seq a
(|>) = snoc
infixl 5 |>
{-# INLINE (|>) #-}

snoc :: forall a. Show a => Seq a -> a -> Seq a
snoc !(Seq size level arr) v = case rootOverflow of 
    0# -> Seq (size +# 1#) level (go v size level arr)
    _  -> Seq (size +# 1#) (level +# KEY_BITS) (init2AA arr (go v size level (_arr empty)))
    where
        rootOverflow :: Int#
        rootOverflow = size ==# uncheckedIShiftL# 1# (level +# KEY_BITS)

        -- optimize init mask 
        go :: a -> Int# -> Int# -> ArrayArray# -> ArrayArray#
        go v size level arr = case level ># 0# of
            1# -> case andI# size (uncheckedIShiftL# 1# (level +# KEY_BITS) -# 1#) ==# 0# of
                0# -> modifyAA arr i (go v size (next level))
                _  -> init1AA (go v size (next level) (_arr empty))
            _ ->  case i ==# 0# of
                0# -> a2aa (updateA (aa2a arr) i v)
                _  -> a2aa (init1A v)
            where i = index size level
{-# INLINABLE snoc #-}


foldr :: forall a b. (a -> b -> b) -> b -> Seq a -> b 
foldr f z !(Seq 0#   _     _  ) = z 
foldr f z  (Seq size level arr) = notfull (size -# 1#) level arr z where

    notfull :: Int# -> Int# -> ArrayArray# -> b -> b 
    notfull lasti level arr z = case level ># 0# of
        1# -> AA.foldr lasti' (full level) (notfull lasti (next level) (AA.index arr lasti') z) arr
        _  -> A.foldr (lasti' +# 1#) f z (aa2a arr)
        where lasti' = index lasti level 

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

next :: Int# -> Int#
next level = level -# KEY_BITS
{-# INLINE next #-}

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


-- Remove bounds check after

modifyAA :: ArrayArray# -> Int# -> (ArrayArray# -> ArrayArray#) -> ArrayArray#
modifyAA arr i a = let size = NODE_WIDTH in case i <# sizeofArrayArray# arr of
    1# -> AA.modify' size arr i a 
    _  -> error "modifyAA: our of bound"
{-# INLINE modifyAA #-}

updateA :: Array# a -> Int# -> a -> Array# a
updateA arr i a = let size = NODE_WIDTH in case i <# sizeofArray# arr of
    1# -> A.update size arr i a
    _  -> error ("updateA: out of bound: " ++ show (I# i) ++ " " ++ show (I# (sizeofArray# arr)))
{-# INLINE updateA #-}

-------------------------------


init1A :: a -> Array# a
init1A a = A.init1 NODE_WIDTH a undefElem
{-# INLINE init1A #-}

init2A :: a -> a -> Array# a
init2A a1 a2 = A.init2 NODE_WIDTH a1 a2 undefElem
{-# INLINE init2A #-}

init1AA :: ArrayArray# -> ArrayArray#
init1AA a = AA.init1 NODE_WIDTH a undefElem
{-# INLINE init1AA #-}

init2AA :: ArrayArray# -> ArrayArray# -> ArrayArray#
init2AA a1 a2 = AA.init2 NODE_WIDTH a1 a2 undefElem
{-# INLINE init2AA #-}


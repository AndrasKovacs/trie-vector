




{-# LANGUAGE MagicHash, BangPatterns, UnboxedTuples, CPP, RankNTypes, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-full-laziness #-}

import GHC.Prim
import GHC.Types
import qualified Array as A
import Debug.Trace

#define NODE_WIDTH 16#
#define KEY_BITS 4#
#define KEY_MASK 15#


main = do
    let a = empty :: Seq Int
        !x@(Seq s l d) = foldl snoc empty [0..5 :: Integer]

    print $ 0

data Seq a = Seq {
    size, level :: Int#,
    arr :: Any }


(|>) :: Seq a -> a -> Seq a
(|>) = snoc
infixl 5 |>
{-# INLINE (|>) #-}

snoc :: forall a. Seq a -> a -> Seq a
snoc !(Seq size level arr) v = traceShow "snoc" $ case rootOverflow of 
    0# -> Seq (size +# 1#) level arr' where
        !arr' = traceShow "olla" $ go v size level arr
    _  -> Seq (size +# 1#) (level +# KEY_BITS) (arr2any arr') where
        !arr' = init2 arr (go v size level undefElem)

    where
        rootOverflow :: Int#
        rootOverflow = size ==# uncheckedIShiftL# 1# (level +# KEY_BITS)

        go :: a -> Int# -> Int# -> Any -> Any
        go v size level arr = traceShow "foo" $ case level ># 0# of
            1# -> case i ==# 0# of
                0# -> arr2any (A.modify (any2arr arr) i (go v size (next level)))
                _  -> arr2any (init1 (go v size (next level) undefElem))
            _ -> case i ==# 0# of
                0# -> arr2any (A.update (any2arr arr) i v)
                _  -> arr2any (init1 v)
            where i = index size level
{-# INLINABLE snoc #-}

toList :: forall a. Seq a -> [a]
toList (Seq size level arr) = go size level arr where
    go :: Int# -> Int# -> Any -> [a]
    go size level arr = traceShow "ab" $ case level ># 0# of
        1# -> concatMap (go size (next level)) $ take i $ A.toList (any2arr arr)
        _  -> take i $ A.toList (any2arr arr :: Array# a)
        where i = I# (index size level)
{-# INLINABLE toList #-}

empty :: Seq a
empty = Seq 0# 0# undefElem

singleton :: a -> Seq a
singleton a = Seq 1# 0# (arr2any (init1 a))
{-# INLINE singleton #-}


next :: Int# -> Int#
next level = level -# KEY_BITS
{-# INLINE next #-}

any2arr :: Any -> Array# a
any2arr = unsafeCoerce# 
{-# INLINE any2arr #-}

arr2any :: Array# a -> Any
arr2any = unsafeCoerce# 
{-# INLINE arr2any #-}

undefElem :: a
undefElem = error "Seq: undefined element"

init1 :: a -> Array# a
init1 a = A.init1 NODE_WIDTH a undefElem
{-# INLINE init1 #-}

init2 :: a -> a -> Array# a
init2 a1 a2 = A.init2 NODE_WIDTH a1 a2 undefElem
{-# INLINE init2 #-}

index :: Int# -> Int# -> Int#
index i level = andI# (uncheckedIShiftRL# i level) KEY_MASK
{-# INLINE index #-}
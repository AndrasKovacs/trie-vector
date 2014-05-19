

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
    let !s = Prelude.foldl snoc empty [0..256]

    --print $ Main.foldr (:) [] s
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

        go :: a -> Int# -> Int# -> ArrayArray# -> ArrayArray#
        go v size level arr = case level ># 0# of
            1# -> case i ==# 0# of
                0# -> modifyAA arr i (go v size (next level))
                _  -> init1AA (go v size (next level) (_arr empty))
            _ -> case i ==# 0# of
                0# -> a2aa (updateA (aa2a arr) i v)
                _  -> a2aa (init1A v)
            where i = index size level
{-# INLINABLE snoc #-}





--foldr :: forall a b. (a -> b -> b) -> b -> Seq a -> b 
--foldr f z !(Seq 0#   _     _  ) = z 
--foldr f z  (Seq size level arr) = AA.foldr (index (size -# 1#) level) (go level 

--{-# INLINABLE foldr #-}


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
{-# INLINABLE foldr #-}


--foldl :: forall a b. (b -> a -> b) -> b -> Seq a -> b 
--foldl f z !(Seq 0#   _     _  ) = z 
--foldl f z  (Seq size level arr) = notfull (size -# 1#) level arr z where

--    notfull :: Int# -> Int# -> ArrayArray# -> b -> b 
--    notfull lasti level arr z = case level ># 0# of
--        1# -> AA.foldl lasti' (full level) (notfull lasti (next level) (AA.index arr lasti') z) arr
--        _  -> A.foldl (lasti' +# 1#) f z (aa2a arr)
--        where lasti' = index lasti level 

--    full :: Int# -> b -> ArrayArray# -> b
--    full level arr z = case level ># 0# of
--        1# -> AA.foldl NODE_WIDTH (full (next level)) z arr
--        _  -> A.foldl NODE_WIDTH f z (aa2a arr)
--{-# INLINABLE foldl #-}


--foldr :: forall a b. (a -> b -> b) -> b -> Seq a -> b 
--foldr f z !(Seq 0#   _     _  ) = z 
--foldr f z  (Seq size level arr) = unfull (size -# 1#) level arr z where

--    unfull :: Int# -> Int# -> ArrayArray# -> b -> b 
--    unfull lasti level arr acc = case level ># 0# of
--        1# -> loop 0# where
--            loop i = case i <# lasti of
--                1# -> full (next level) (AA.index arr i) (loop (i +# 1#))
--                _  -> unfull (index lasti level) (next level) (AA.index arr lasti) acc
--        _ -> loop 0# where
--            loop i = case i <=# lasti of
--                1# -> f (A.index (aa2a arr) i) (loop (i +# 1#))
--                _  -> acc

--    full :: Int# -> ArrayArray# -> b -> b
--    full level arr acc = case level ># 0# of
--        1# -> loop 0# where
--            loop i = case i <# NODE_WIDTH of
--                1# -> full (next level) (AA.index arr i) (loop (i +# 1#))
--                _  -> acc
--        _ -> loop 0# where
--            loop i = case i <# NODE_WIDTH of
--                1# -> f (A.index (aa2a arr) i) (loop (i +# 1#))
--                _  -> acc 
--{-# INLINABLE foldr #-}


--toList :: forall a. Seq a -> [a]
--toList !(Seq 0# level arr) = []
--toList (Seq size level arr) = unfull (size -# 1#) level arr where

--    unfull :: Int# -> Int# -> ArrayArray# -> [a]
--    unfull lasti level arr = case level ># 0# of
--        1# -> fulls 0# lasti level arr ++ unfull (index lasti level) (next level) arr
--        0# -> fulls 0# lasti level arr ++ [A.index (aa2a arr) lasti] 



--    go :: Int# -> Int# -> ArrayArray# -> [a]
--    go lasti level arr = case level ># 0# of
--        1# -> full 0# ++ 
--            loop !i = case i <# levelIndex of
--                1# -> go lasti (next level) (AA.indexAA arr i) ++ loop (i +# 1#)
--                _  -> []
--        _  -> loop 0# where
--            loop !i = case i <# levelIndex of
--                1# -> A.index (aa2a arr) i : loop (i +# 1#)
--                _  -> []
--        where levelIndex = index lasti level
--{-# INLINABLE toList #-}

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


modifyAA :: ArrayArray# -> Int# -> (ArrayArray# -> ArrayArray#) -> ArrayArray#
modifyAA = let size = NODE_WIDTH in AA.modify' size
{-# INLINE modifyAA #-}

modifyA :: Array# a -> Int# -> (a -> a) -> Array# a
modifyA = let size = NODE_WIDTH in A.modify' size
{-# INLINE modifyA #-}

updateA :: Array# a -> Int# -> a -> Array# a
updateA = let size = NODE_WIDTH in A.update size
{-# INLINE updateA #-}

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


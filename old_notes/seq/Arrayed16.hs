
{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples, RecordWildCards, CPP #-}
{-# OPTIONS_GHC -fno-full-laziness #-}

module Arrayed16 where

#define WIDTH 16#
#define MASK  15#
#define BITS  4#

import GHC.Prim
import GHC.Types
import Unsafe
import Debug.Trace


--(!) :: Seq a -> Int -> a
--(!) !s@Seq{..} (I# i) = go (readSeq (index i level) s) (level -# BITS) where
--    go n lvl = case lvl <# 0# of
--        0# -> case n of
--            n@Node{} -> go (readNode (index i lvl) n) (lvl -# BITS)
--            _ -> error "Seq.(!): out of bounds"
--        _# -> case n of
--            Leaf a -> a
--            _ -> error "Seq.(!): out of bounds"
--infixl 5 !
--{-# INLINE (!) #-}


--(|>) :: Seq a -> a -> Seq a
--(|>) = snoc
--{-# INLINE (|>) #-}
--infixl 5 |>

--empty :: Seq a
--empty = Seq 0# 0#
--    Empty Empty Empty Empty 
--    Empty Empty Empty Empty 
--    Empty Empty Empty Empty 
--    Empty Empty Empty Empty


--singleton :: a -> Seq a
--singleton a = empty {sn0 = Leaf a, size = 1#}
--{-# INLINE singleton #-}

--snoc :: Seq a -> a -> Seq a
--snoc s@Seq{..} a = case size ==# (uncheckedIShiftL# 1# (level +# BITS)) of
--    0# -> modifySeqWithSize (size +# 1#) (index size level) (go (level -# BITS)) s
--    _  -> empty {size = size +# 1#, level = level +# BITS, sn0 = rootToNode s, sn1 = go level Empty}
--    where go lvl n = case lvl <# 0# of
--            0# -> case n of
--                n@Node{} -> modifyNode (index size lvl) (go (lvl -# BITS)) n
--                Empty    -> emptyNode {n0 = go (lvl -# BITS) Empty}
--                _        -> error "Seq.snoc: invalid data"
--            _  -> case n of
--                Empty -> Leaf a
--                _     -> error "Seq.snoc: invalid data"
--{-# INLINE snoc #-}

--adjust :: (a -> a) -> Int -> Seq a -> Seq a
--adjust f (I# i) s@Seq{..} = case checkBound i size of
--    1# -> unsafeModify i f s
--    _  -> s
--{-# INLINE adjust #-}

--write :: Seq a -> Int -> a -> Seq a
--write s@Seq{..} (I# i) a = unsafeModify i (const a) s 
--{-# INLINE write #-}

--toList :: Seq a -> [a]
--toList s@Seq{..} = 
--    go sn0  ++  go sn1  ++  go sn2  ++  go sn3  ++ 
--    go sn4  ++  go sn5  ++  go sn6  ++  go sn7  ++ 
--    go sn8  ++  go sn9  ++  go sn10 ++  go sn11 ++  
--    go sn12 ++  go sn13 ++  go sn14 ++  go sn15
--    where go Node{..} = 
--            go n0  ++  go n1  ++  go n2  ++  go n3  ++ 
--            go n4  ++  go n5  ++  go n6  ++  go n7  ++ 
--            go n8  ++  go n9  ++  go n10 ++  go n11 ++  
--            go n12 ++  go n13 ++  go n14 ++  go n15
--          go (Leaf a) = [a]
--          go Empty    = []

--instance Show a => Show (Seq a) where
--    show seq = "fromList " ++ (show $ toList seq)




data Array a = Array (Array# a) 

data Seq a = Seq {size, level :: Int#, arr :: !(Array# (Node a))}

data Node a
    = Node !(Array# (Node a))
    | Leaf a
    | Empty 


--modifyArr :: Array# a -> Int# -> (a -> a) -> Array# a
--modifyArr


--snoc :: Seq a -> a -> Seq a
--snoc s@Seq{..} a = case size ==# (uncheckedIShiftL# 1# (level +# BITS)) of
--    0# -> Seq (size +# 1#) level (modifyArr arr (index i level) (go (level -# BITS)))
--    _  -> empty {size = size +# 1#, level = level +# BITS, sn0 = Node arr, sn1 = go level Empty}
--    where go lvl n = case lvl <# 0# of
--            0# -> case n of
--                n@Node{} -> modifyNode (index size lvl) (go (lvl -# BITS)) n
--                Empty    -> emptyNode {n0 = go (lvl -# BITS) Empty}
--                _        -> error "Seq.snoc: invalid data"
--            _  -> case n of
--                Empty -> Leaf a
--                _     -> error "Seq.snoc: invalid data"
--{-# INLINE snoc #-}snoc !s@Seq{..} a 




index :: Int# -> Int# -> Int#
index i level = andI# (uncheckedIShiftRL# i level) MASK
{-# INLINE index #-}

checkBound :: Int# -> Int# -> Int#
checkBound i size = andI# (i >=# 0#) (i <# size)
{-# INLINE checkBound #-}


#undef WIDTH 
#undef MASK  
#undef BITS 

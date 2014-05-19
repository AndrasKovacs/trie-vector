{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples, RecordWildCards, CPP #-}



--  TODO : index is unsafe!!

module Flat16 where

#define WIDTH 16#
#define MASK  15#
#define BITS  4#

import GHC.Prim
import GHC.Types
import Debug.Trace


(!) :: Seq a -> Int -> a
(!) !s@Seq{..} (I# i) = go (readSeq (index i level) s) (level -# BITS) where
    go n lvl = case lvl <# 0# of
        0# -> case n of
            n@Node{} -> go (readNode (index i lvl) n) (lvl -# BITS)
            _ -> error "Seq.(!): out of bounds"
        _# -> case n of
            Leaf a -> a
            _ -> error "Seq.(!): out of bounds"
infixl 5 !
{-# INLINE (!) #-}


(|>) :: Seq a -> a -> Seq a
(|>) = snoc
{-# INLINE (|>) #-}
infixl 5 |>

empty :: Seq a
empty = Seq 0# 0#
    Empty Empty Empty Empty 
    Empty Empty Empty Empty 
    Empty Empty Empty Empty 
    Empty Empty Empty Empty


singleton :: a -> Seq a
singleton a = empty {sn0 = Leaf a, size = 1#}
{-# INLINE singleton #-}

snoc :: Seq a -> a -> Seq a
snoc s@Seq{..} a = case size ==# (uncheckedIShiftL# 1# (level +# BITS)) of
    0# -> modifySeqWithSize (size +# 1#) (index size level) (go (level -# BITS)) s
    _  -> empty {size = size +# 1#, level = level +# BITS, sn0 = rootToNode s, sn1 = go level Empty}
    where go lvl n = case lvl <# 0# of
            0# -> case n of
                n@Node{} -> modifyNode (index size lvl) (go (lvl -# BITS)) n
                Empty    -> emptyNode {n0 = go (lvl -# BITS) Empty}
                _        -> error "Seq.snoc: invalid data"
            _  -> case n of
                Empty -> Leaf a
                _     -> error "Seq.snoc: invalid data"
{-# INLINE snoc #-}

adjust :: (a -> a) -> Int -> Seq a -> Seq a
adjust f (I# i) s@Seq{..} = case checkBound i size of
    1# -> unsafeModify i f s
    _  -> s
{-# INLINE adjust #-}

write :: Seq a -> Int -> a -> Seq a
write s@Seq{..} (I# i) a = unsafeModify i (const a) s 
{-# INLINE write #-}

toList :: Seq a -> [a]
toList s@Seq{..} = 
    go sn0  ++  go sn1  ++  go sn2  ++  go sn3  ++ 
    go sn4  ++  go sn5  ++  go sn6  ++  go sn7  ++ 
    go sn8  ++  go sn9  ++  go sn10 ++  go sn11 ++  
    go sn12 ++  go sn13 ++  go sn14 ++  go sn15
    where go Node{..} = 
            go n0  ++  go n1  ++  go n2  ++  go n3  ++ 
            go n4  ++  go n5  ++  go n6  ++  go n7  ++ 
            go n8  ++  go n9  ++  go n10 ++  go n11 ++  
            go n12 ++  go n13 ++  go n14 ++  go n15
          go (Leaf a) = [a]
          go Empty    = []

instance Show a => Show (Seq a) where
    show seq = "fromList " ++ (show $ toList seq)














emptyNode :: Node a
emptyNode = Node
    Empty Empty Empty Empty 
    Empty Empty Empty Empty 
    Empty Empty Empty Empty 
    Empty Empty Empty Empty


data Node a = Node {
    n0,  n1,  n2,  n3, 
    n4,  n5,  n6,  n7, 
    n8,  n9,  n10, n11, 
    n12, n13, n14, n15 :: !(Node a)}
    | Leaf a
    | Empty

data Seq a = Seq {
    size, level :: Int#,
    sn0,  sn1,  sn2,  sn3, 
    sn4,  sn5,  sn6,  sn7, 
    sn8,  sn9,  sn10, sn11, 
    sn12, sn13, sn14, sn15 :: !(Node a)}


rootToNode :: Seq a -> Node a
rootToNode !s@Seq{..} = Node 
    sn0  sn1  sn2  sn3 
    sn4  sn5  sn6  sn7 
    sn8  sn9  sn10 sn11 
    sn12 sn13 sn14 sn15
{-# INLINE  rootToNode #-}

unsafeModify :: Int# -> (a -> a) -> Seq a -> Seq a
unsafeModify i f s@Seq{..} = modifySeq (index i level) (go (level -# BITS)) s where
    go lvl n = case lvl <# 0# of
        0# -> case n of
            n@Node{} -> modifyNode (index i lvl) (go (lvl -# BITS)) n
            _        -> error "Seq.unsafeModify: out of bounds"
        _  -> case n of
            Leaf a   -> Leaf (f a)
            _        -> error "Seq.unsafeModify: out of bounds"
{-# INLINE unsafeModify #-}

index :: Int# -> Int# -> Int#
index i level = andI# (uncheckedIShiftRL# i level) MASK
{-# INLINE index #-}

checkBound :: Int# -> Int# -> Int#
checkBound i size = andI# (i >=# 0#) (i <# size)
{-# INLINE checkBound #-}

readNode :: Int# -> Node a -> Node a
readNode i !(Node{..}) = case i of
    0#  -> n0 ; 1#  -> n1 ; 2#  -> n2 ; 3#  -> n3 ;
    4#  -> n4 ; 5#  -> n5 ; 6#  -> n6 ; 7#  -> n7 ;
    8#  -> n8 ; 9#  -> n9 ; 10# -> n10; 11# -> n11;
    12# -> n12; 13# -> n13; 14# -> n14; 15# -> n15;
    _   -> undefined
readNode _ _ = undefined
{-# INLINE readNode #-}

modifyNode :: Int# -> (Node a -> Node a) -> Node a -> Node a
modifyNode i f !n@(Node{..}) = case i of
    0#  -> n {n0  = f n0 }; 1#  -> n {n1  = f n1  }; 2#  -> n {n2  = f n2 }; 3#  -> n {n3  = f n3 };
    4#  -> n {n4  = f n4 }; 5#  -> n {n5  = f n5  }; 6#  -> n {n6  = f n6 }; 7#  -> n {n7  = f n7 };
    8#  -> n {n8  = f n8 }; 9#  -> n {n9  = f n9  }; 10# -> n {n10 = f n10}; 11# -> n {n11 = f n11};
    12# -> n {n12 = f n12}; 13# -> n {n13 = f n13 }; 14# -> n {n14 = f n14}; 15# -> n {n15 = f n15};
    _   -> undefined
modifyNode _ _ _ = undefined
{-# INLINE modifyNode #-}

readSeq :: Int# -> Seq a -> Node a
readSeq i !(Seq{..}) = case i of
    0#  -> sn0 ; 1#  -> sn1 ; 2#  -> sn2 ; 3#  -> sn3 ;
    4#  -> sn4 ; 5#  -> sn5 ; 6#  -> sn6 ; 7#  -> sn7 ;
    8#  -> sn8 ; 9#  -> sn9 ; 10# -> sn10; 11# -> sn11;
    12# -> sn12; 13# -> sn13; 14# -> sn14; 15# -> sn15;
    _   -> undefined
{-# INLINE readSeq #-}

modifySeq :: Int# -> (Node a -> Node a) -> Seq a -> Seq a
modifySeq i f !s@(Seq{..}) = case i of
    0#  -> s {sn0  = f sn0 }; 1#  -> s {sn1  = f sn1  }; 2#  -> s {sn2  = f sn2 }; 3#  -> s {sn3  = f sn3 };
    4#  -> s {sn4  = f sn4 }; 5#  -> s {sn5  = f sn5  }; 6#  -> s {sn6  = f sn6 }; 7#  -> s {sn7  = f sn7 };
    8#  -> s {sn8  = f sn8 }; 9#  -> s {sn9  = f sn9  }; 10# -> s {sn10 = f sn10}; 11# -> s {sn11 = f sn11};
    12# -> s {sn12 = f sn12}; 13# -> s {sn13 = f sn13 }; 14# -> s {sn14 = f sn14}; 15# -> s {sn15 = f sn15};
    _   -> undefined
{-# INLINE modifySeq #-}

modifySeqWithSize :: Int# -> Int# -> (Node a -> Node a) -> Seq a -> Seq a
modifySeqWithSize size' i f !s@(Seq{..}) = case i of
    0#  -> s {sn0  = f sn0 , size = size' }; 1#  -> s {sn1  = f sn1 , size = size'}; 
    2#  -> s {sn2  = f sn2 , size = size' }; 3#  -> s {sn3  = f sn3 , size = size'};
    4#  -> s {sn4  = f sn4 , size = size' }; 5#  -> s {sn5  = f sn5 , size = size'}; 
    6#  -> s {sn6  = f sn6 , size = size' }; 7#  -> s {sn7  = f sn7 , size = size'};
    8#  -> s {sn8  = f sn8 , size = size' }; 9#  -> s {sn9  = f sn9 , size = size'}; 
    10# -> s {sn10 = f sn10, size = size' }; 11# -> s {sn11 = f sn11, size = size'};
    12# -> s {sn12 = f sn12, size = size' }; 13# -> s {sn13 = f sn13, size = size'}; 
    14# -> s {sn14 = f sn14, size = size' }; 15# -> s {sn15 = f sn15, size = size'};
    _   -> undefined
{-# INLINE modifySeqWithSize #-}

#undef WIDTH 
#undef MASK  
#undef BITS 

{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples, RecordWildCards, CPP #-}



-- TODO : omit bounds checks because it can be done inside the traversal anyway, 
--        and we also expect the operations to usually not fail. 

module Flat32 where

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
    go sn0  ++ go sn1  ++ go sn2  ++ go sn3  ++
    go sn4  ++ go sn5  ++ go sn6  ++ go sn7  ++
    go sn8  ++ go sn9  ++ go sn10 ++ go sn11 ++ 
    go sn12 ++ go sn13 ++ go sn14 ++ go sn15 ++
    go sn16 ++ go sn17 ++ go sn18 ++ go sn19 ++
    go sn20 ++ go sn21 ++ go sn22 ++ go sn23 ++
    go sn24 ++ go sn25 ++ go sn26 ++ go sn27 ++
    go sn28 ++ go sn29 ++ go sn30 ++ go sn31
    where go Node{..} = 
            go n0  ++ go n1  ++ go n2  ++ go n3  ++
            go n4  ++ go n5  ++ go n6  ++ go n7  ++
            go n8  ++ go n9  ++ go n10 ++ go n11 ++ 
            go n12 ++ go n13 ++ go n14 ++ go n15 ++
            go n16 ++ go n17 ++ go n18 ++ go n19 ++
            go n20 ++ go n21 ++ go n22 ++ go n23 ++
            go n24 ++ go n25 ++ go n26 ++ go n27 ++
            go n28 ++ go n29 ++ go n30 ++ go n31
          go (Leaf a) = [a]
          go Empty    = []

instance Show a => Show (Seq a) where
    show seq = "fromList " ++ (show $ toList seq)



data Node a = Node {
    n0,  n1,  n2,  n3, 
    n4,  n5,  n6,  n7, 
    n8,  n9,  n10, n11, 
    n12, n13, n14, n15,
    n16, n17, n18, n19,
    n20, n21, n22, n23,
    n24, n25, n26, n27,
    n28, n29, n30, n31 :: !(Node a)}
    | Leaf a
    | Empty

data Seq a = Seq {
    size, level :: Int#,
    sn0,  sn1,  sn2,  sn3, 
    sn4,  sn5,  sn6,  sn7, 
    sn8,  sn9,  sn10, sn11, 
    sn12, sn13, sn14, sn15,
    sn16, sn17, sn18, sn19,
    sn20, sn21, sn22, sn23,
    sn24, sn25, sn26, sn27,
    sn28, sn29, sn30, sn31 :: !(Node a)}



emptyNode :: Node a
emptyNode = Node
    Empty Empty Empty Empty 
    Empty Empty Empty Empty 
    Empty Empty Empty Empty 
    Empty Empty Empty Empty
    Empty Empty Empty Empty 
    Empty Empty Empty Empty 
    Empty Empty Empty Empty 
    Empty Empty Empty Empty


rootToNode :: Seq a -> Node a
rootToNode !s@Seq{..} = Node 
    sn0  sn1  sn2  sn3 
    sn4  sn5  sn6  sn7 
    sn8  sn9  sn10 sn11 
    sn12 sn13 sn14 sn15
    sn16 sn17 sn18 sn19
    sn20 sn21 sn22 sn23
    sn24 sn25 sn26 sn27
    sn28 sn29 sn30 sn31
{-# INLINE rootToNode #-}

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
    16# -> n16; 17# -> n17; 18# -> n18; 19# -> n19;
    20# -> n20; 21# -> n21; 22# -> n22; 23# -> n23;
    24# -> n24; 25# -> n25; 26# -> n26; 27# -> n27;
    28# -> n28; 29# -> n29; 30# -> n30; 31# -> n31;
    _   -> undefined
readNode _ _ = undefined
{-# INLINE readNode #-}

modifyNode :: Int# -> (Node a -> Node a) -> Node a -> Node a
modifyNode i f !n@(Node{..}) = case i of
    0#  -> n {n0  = f n0 }; 1#  -> n {n1  = f n1 } ; 2#  -> n {n2  = f n2 }; 3#  -> n {n3  = f n3 } ;
    4#  -> n {n4  = f n4 }; 5#  -> n {n5  = f n5 } ; 6#  -> n {n6  = f n6 }; 7#  -> n {n7  = f n7 } ;
    8#  -> n {n8  = f n8 }; 9#  -> n {n9  = f n9 } ; 10# -> n {n10 = f n10}; 11# -> n {n11 = f n11} ;
    12# -> n {n12 = f n12}; 13# -> n {n13 = f n13} ; 14# -> n {n14 = f n14}; 15# -> n {n15 = f n15} ;
    16# -> n {n16 = f n16}; 17# -> n {n17 = f n17} ; 18# -> n {n18 = f n18}; 19# -> n {n19 = f n19} ;
    20# -> n {n20 = f n20}; 21# -> n {n21 = f n21} ; 22# -> n {n22 = f n22}; 23# -> n {n23 = f n23} ;
    24# -> n {n24 = f n24}; 25# -> n {n25 = f n25} ; 26# -> n {n26 = f n26}; 27# -> n {n27 = f n27} ;
    28# -> n {n28 = f n28}; 29# -> n {n29 = f n29} ; 30# -> n {n30 = f n30}; 31# -> n {n31 = f n31} ;
    _   -> undefined
modifyNode _ _ _ = undefined
{-# INLINE modifyNode #-}

readSeq :: Int# -> Seq a -> Node a
readSeq i !Seq{..} = case i of
    0#  -> sn0 ; 1#  -> sn1 ; 2#  -> sn2 ; 3#  -> sn3 ;
    4#  -> sn4 ; 5#  -> sn5 ; 6#  -> sn6 ; 7#  -> sn7 ;
    8#  -> sn8 ; 9#  -> sn9 ; 10# -> sn10; 11# -> sn11;
    12# -> sn12; 13# -> sn13; 14# -> sn14; 15# -> sn15;
    16# -> sn16; 17# -> sn17; 18# -> sn18; 19# -> sn19;
    20# -> sn20; 21# -> sn21; 22# -> sn22; 23# -> sn23;
    24# -> sn24; 25# -> sn25; 26# -> sn26; 27# -> sn27;
    28# -> sn28; 29# -> sn29; 30# -> sn30; 31# -> sn31;
    _   -> undefined
{-# INLINE readSeq #-}

modifySeq :: Int# -> (Node a -> Node a) -> Seq a -> Seq a
modifySeq i f !s@Seq{..} = case i of
    0#  -> s {sn0  = f sn0 }; 1#  -> s {sn1  = f sn1 } ; 2#  -> s {sn2  = f sn2 }; 3#  -> s {sn3  = f sn3 } ;
    4#  -> s {sn4  = f sn4 }; 5#  -> s {sn5  = f sn5 } ; 6#  -> s {sn6  = f sn6 }; 7#  -> s {sn7  = f sn7 } ;
    8#  -> s {sn8  = f sn8 }; 9#  -> s {sn9  = f sn9 } ; 10# -> s {sn10 = f sn10}; 11# -> s {sn11 = f sn11} ;
    12# -> s {sn12 = f sn12}; 13# -> s {sn13 = f sn13} ; 14# -> s {sn14 = f sn14}; 15# -> s {sn15 = f sn15} ;
    16# -> s {sn16 = f sn16}; 17# -> s {sn17 = f sn17} ; 18# -> s {sn18 = f sn18}; 19# -> s {sn19 = f sn19} ;
    20# -> s {sn20 = f sn20}; 21# -> s {sn21 = f sn21} ; 22# -> s {sn22 = f sn22}; 23# -> s {sn23 = f sn23} ;
    24# -> s {sn24 = f sn24}; 25# -> s {sn25 = f sn25} ; 26# -> s {sn26 = f sn26}; 27# -> s {sn27 = f sn27} ;
    28# -> s {sn28 = f sn28}; 29# -> s {sn29 = f sn29} ; 30# -> s {sn30 = f sn30}; 31# -> s {sn31 = f sn31} ;
    _   -> undefined
{-# INLINE modifySeq #-}

modifySeqWithSize :: Int# -> Int# -> (Node a -> Node a) -> Seq a -> Seq a
modifySeqWithSize size' i f !s@Seq{..} = case i of
    0#  -> s {sn0  = f sn0 , size = size'}; 1#  -> s {sn1  = f sn1 , size = size'} ; 2#  -> s {sn2  = f sn2 , size = size'}; 3#  -> s {sn3  = f sn3 , size = size'};
    4#  -> s {sn4  = f sn4 , size = size'}; 5#  -> s {sn5  = f sn5 , size = size'} ; 6#  -> s {sn6  = f sn6 , size = size'}; 7#  -> s {sn7  = f sn7 , size = size'};
    8#  -> s {sn8  = f sn8 , size = size'}; 9#  -> s {sn9  = f sn9 , size = size'} ; 10# -> s {sn10 = f sn10, size = size'}; 11# -> s {sn11 = f sn11, size = size'};
    12# -> s {sn12 = f sn12, size = size'}; 13# -> s {sn13 = f sn13, size = size'} ; 14# -> s {sn14 = f sn14, size = size'}; 15# -> s {sn15 = f sn15, size = size'};
    16# -> s {sn16 = f sn16, size = size'}; 17# -> s {sn17 = f sn17, size = size'} ; 18# -> s {sn18 = f sn18, size = size'}; 19# -> s {sn19 = f sn19, size = size'};
    20# -> s {sn20 = f sn20, size = size'}; 21# -> s {sn21 = f sn21, size = size'} ; 22# -> s {sn22 = f sn22, size = size'}; 23# -> s {sn23 = f sn23, size = size'};
    24# -> s {sn24 = f sn24, size = size'}; 25# -> s {sn25 = f sn25, size = size'} ; 26# -> s {sn26 = f sn26, size = size'}; 27# -> s {sn27 = f sn27, size = size'};
    28# -> s {sn28 = f sn28, size = size'}; 29# -> s {sn29 = f sn29, size = size'} ; 30# -> s {sn30 = f sn30, size = size'}; 31# -> s {sn31 = f sn31, size = size'};
    _   -> undefined
{-# INLINE modifySeqWithSize #-}


#undef WIDTH
#undef MASK 
#undef BITS 
{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples, RecordWildCards, CPP #-}


module Flat16Bottom where

#define WIDTH 16#
#define MASK  15#
#define BITS  4#
#define MAXIX 15#

import GHC.Prim
import GHC.Types
import Debug.Trace


unsafeIndex# :: Seq a -> Int# -> a
unsafeIndex# !s@Seq{..} i = go (indexSeq s (index i level)) (level -# BITS) 
    where
        go n level = case level >=# 0# of
            1# -> case n of
                n@Partial{} -> go (indexPartial n (index i level)) (level -# BITS)
                n@Bottom{}  -> indexBottom n (index i level) 
                _           -> error "Seq.unsafeIndex: invalid data"
            _ -> case n of
                Leaf a -> a
                _      -> error "Seq.unsafeIndex: invalid data"
{-# INLINE unsafeIndex# #-}

unsafeIndex :: Seq a -> Int -> a
unsafeIndex !s@Seq{..} (I# i) = unsafeIndex# s i 
{-# INLINE unsafeIndex #-}

(!) :: Seq a -> Int -> a
(!) !s@Seq{..} (I# i) = case checkBound i size of
    1# -> unsafeIndex# s i 
    _  -> error "Seq.(!): out of bounds"
infixl 5 !
{-# INLINE (!) #-}

(|>) :: Seq a -> a -> Seq a
(|>) = snoc
{-# INLINE (|>) #-}
infixl 5 |>


singleton :: a -> Seq a
singleton a = empty {s0 = Leaf a, size = 1#}
{-# INLINE singleton #-}

snoc :: Seq a -> a -> Seq a
snoc s@Seq{..} a = case size ==# (uncheckedIShiftL# 1# (level +# BITS)) of
    0# -> modifySeqWithSize s (index size level)  (go (level -# BITS))  (size +# 1#)
    _  -> empty {size = size +# 1#, level = level +# BITS, s0 = rootToNode s, s1 = go level Empty}
    where 
        go lvl n = case lvl ># 0# of
            1# -> case n of
                n@Partial{} -> modifyPartial n (index size lvl) (go (lvl -# BITS))
                Empty       -> emptyPartial {p0 = go (lvl -# BITS) Empty}
                _           -> error "Seq.snoc: invalid data"
            _  -> case lvl ==# 0# of 
                1# -> case n of
                    n@Partial{} -> case index size lvl ==# MAXIX of
                        0# -> modifyPartial n (index size lvl) (const (Leaf a))
                        _  -> partialToBottom n a
                    Empty -> emptyPartial {p0 = Leaf a}
                    _     -> error "Seq.snoc: invalid data"
                _ -> case n of 
                    Empty -> Leaf a
                    _     -> error "Seq.snoc: invalid data"
{-# INLINE snoc #-}

modify :: Seq a -> Int -> (a -> a) -> Seq a
modify s (I# i) f = unsafeModify# s i f
{-# INLINE modify #-}

adjust :: (a -> a) -> Int -> Seq a -> Seq a
adjust f (I# i) s@Seq{..} = case checkBound i size of
    1# -> unsafeModify# s i f
    _  -> s
{-# INLINE adjust #-}

write :: Seq a -> Int -> a -> Seq a
write s@Seq{..} !i a = modify s i (const a)
{-# INLINE write #-}

toList :: Seq a -> [a]
toList s@Seq{..} = 
    go s0  ++  go s1  ++  go s2  ++  go s3  ++ 
    go s4  ++  go s5  ++  go s6  ++  go s7  ++ 
    go s8  ++  go s9  ++  go s10 ++  go s11 ++  
    go s12 ++  go s13 ++  go s14 ++  go s15
    where go Partial{..} = 
            go p0  ++  go p1  ++  go p2  ++  go p3  ++ 
            go p4  ++  go p5  ++  go p6  ++  go p7  ++ 
            go p8  ++  go p9  ++  go p10 ++  go p11 ++  
            go p12 ++  go p13 ++  go p14 ++  go p15
          go (Bottom{..}) = 
            b0  :  b1  :  b2  :  b3  : 
            b4  :  b5  :  b6  :  b7  : 
            b8  :  b9  :  b10 :  b11 :  
            b12 :  b13 :  b14 :  b15 : []
          go (Leaf a) = [a]
          go Empty    = []

instance Show a => Show (Seq a) where
    show seq = "fromList " ++ (show $ toList seq)


empty :: Seq a
empty = Seq 0# 0#
    Empty Empty Empty Empty 
    Empty Empty Empty Empty 
    Empty Empty Empty Empty 
    Empty Empty Empty Empty

emptyPartial :: Node a
emptyPartial = Partial
    Empty Empty Empty Empty 
    Empty Empty Empty Empty 
    Empty Empty Empty Empty 
    Empty Empty Empty Empty

data Node a
    = Bottom {
        b0,  b1,  b2,  b3, 
        b4,  b5,  b6,  b7, 
        b8,  b9,  b10, b11, 
        b12, b13, b14, b15 :: a}
    | Partial {
        p0,  p1,  p2,  p3, 
        p4,  p5,  p6,  p7, 
        p8,  p9,  p10, p11, 
        p12, p13, p14, p15 :: !(Node a)}
    | Leaf a
    | Empty 

data Seq a = Seq {
    size, level :: Int#,
    s0,  s1,  s2,  s3, 
    s4,  s5,  s6,  s7, 
    s8,  s9,  s10, s11, 
    s12, s13, s14, s15 :: !(Node a)}

rootToNode :: Seq a -> Node a
rootToNode (Seq _ _ 
        (Leaf a0 ) (Leaf a1 ) (Leaf a2 ) (Leaf a3 )
        (Leaf a4 ) (Leaf a5 ) (Leaf a6 ) (Leaf a7 )
        (Leaf a8 ) (Leaf a9 ) (Leaf a10) (Leaf a11) 
        (Leaf a12) (Leaf a13) (Leaf a14) (Leaf a15)) =
    Bottom 
        a0  a1  a2  a3 
        a4  a5  a6  a7 
        a8  a9  a10 a11 
        a12 a13 a14 a15
rootToNode !Seq{..} = Partial 
    s0  s1  s2  s3 
    s4  s5  s6  s7 
    s8  s9  s10 s11 
    s12 s13 s14 s15
{-# INLINE rootToNode #-}

partialToBottom :: Node a -> a -> Node a
partialToBottom (Partial
        (Leaf p0 ) (Leaf p1 ) (Leaf p2 ) (Leaf p3 )
        (Leaf p4 ) (Leaf p5 ) (Leaf p6 ) (Leaf p7 )
        (Leaf p8 ) (Leaf p9 ) (Leaf p10) (Leaf p11) 
        (Leaf p12) (Leaf p13) (Leaf p14) _        ) a =
    Bottom 
        p0  p1  p2  p3 
        p4  p5  p6  p7 
        p8  p9  p10 p11 
        p12 p13 p14 a
partialToBottom _ _ = undefined 
{-# INLINE partialToBottom #-}

unsafeModify# :: Seq a -> Int# -> (a -> a) -> Seq a 
unsafeModify# !s@Seq{..} !i f = modifySeq s (index i level) (go (level -# BITS)) where
    go lvl n = case lvl <# 0# of
        0# -> case n of
            n@Partial{} -> modifyPartial n (index i lvl) (go (lvl -# BITS))
            n@Bottom{}  -> modifyBottom  n (index i lvl) f
            _           -> error "Seq.unsafeModify: invalid data"
        _  -> case n of
            Leaf a   -> Leaf (f a)
            _        -> error "Seq.unsafeModify: invalid data"
{-# INLINE unsafeModify# #-}

index :: Int# -> Int# -> Int#
index i level = andI# (uncheckedIShiftRL# i level) MASK
{-# INLINE index #-}

checkBound :: Int# -> Int# -> Int#
checkBound i size = andI# (i >=# 0#) (i <# size)
{-# INLINE checkBound #-}


-- ****************************************************************************


indexBottom :: Node a -> Int# -> a
indexBottom !Bottom{..} !i = case i of
    0#  -> b0 ; 1#  -> b1 ; 2#  -> b2 ; 3#  -> b3 ;
    4#  -> b4 ; 5#  -> b5 ; 6#  -> b6 ; 7#  -> b7 ;
    8#  -> b8 ; 9#  -> b9 ; 10# -> b10; 11# -> b11;
    12# -> b12; 13# -> b13; 14# -> b14; 15# -> b15;
    _   -> undefined
indexBottom !_ !_ = undefined
{-# INLINE indexBottom #-}

modifyBottom :: Node a -> Int# -> (a -> a) -> Node a
modifyBottom !n@(Bottom{..}) !i f = case i of
    0#  -> n {b0  = f b0 }; 1#  -> n {b1  = f b1  }; 2#  -> n {b2  = f b2 }; 3#  -> n {b3  = f b3 };
    4#  -> n {b4  = f b4 }; 5#  -> n {b5  = f b5  }; 6#  -> n {b6  = f b6 }; 7#  -> n {b7  = f b7 };
    8#  -> n {b8  = f b8 }; 9#  -> n {b9  = f b9  }; 10# -> n {b10 = f b10}; 11# -> n {b11 = f b11};
    12# -> n {b12 = f b12}; 13# -> n {b13 = f b13 }; 14# -> n {b14 = f b14}; 15# -> n {b15 = f b15};
    _   -> undefined
modifyBottom !_ !_ _ = undefined
{-# INLINE modifyBottom #-}


-- ****************************************************************************


modifyPartial :: Node a -> Int# -> (Node a -> Node a) -> Node a
modifyPartial !n@(Partial{..}) !i f = case i of
    0#  -> n {p0  = f p0 }; 1#  -> n {p1  = f p1  }; 2#  -> n {p2  = f p2 }; 3#  -> n {p3  = f p3 };
    4#  -> n {p4  = f p4 }; 5#  -> n {p5  = f p5  }; 6#  -> n {p6  = f p6 }; 7#  -> n {p7  = f p7 };
    8#  -> n {p8  = f p8 }; 9#  -> n {p9  = f p9  }; 10# -> n {p10 = f p10}; 11# -> n {p11 = f p11};
    12# -> n {p12 = f p12}; 13# -> n {p13 = f p13 }; 14# -> n {p14 = f p14}; 15# -> n {p15 = f p15};
    _   -> undefined
modifyPartial !_ !_ _ = undefined
{-# INLINE modifyPartial #-}

indexPartial :: Node a -> Int# -> Node a
indexPartial !Partial{..} !i = case i of
    0#  -> p0 ; 1#  -> p1 ; 2#  -> p2 ; 3#  -> p3 ;
    4#  -> p4 ; 5#  -> p5 ; 6#  -> p6 ; 7#  -> p7 ;
    8#  -> p8 ; 9#  -> p9 ; 10# -> p10; 11# -> p11;
    12# -> p12; 13# -> p13; 14# -> p14; 15# -> p15;
    _   -> undefined
indexPartial !_ !_ = undefined
{-# INLINE indexPartial #-}


-- ****************************************************************************

indexSeq :: Seq a -> Int# -> Node a
indexSeq !(Seq{..}) !i = case i of
    0#  -> s0 ; 1#  -> s1 ; 2#  -> s2 ; 3#  -> s3 ;
    4#  -> s4 ; 5#  -> s5 ; 6#  -> s6 ; 7#  -> s7 ;
    8#  -> s8 ; 9#  -> s9 ; 10# -> s10; 11# -> s11;
    12# -> s12; 13# -> s13; 14# -> s14; 15# -> s15;
    _   -> undefined
{-# INLINE indexSeq #-}

modifySeq :: Seq a -> Int# -> (Node a -> Node a) -> Seq a
modifySeq !s@(Seq{..}) !i f = case i of
    0#  -> s {s0  = f s0 }; 1#  -> s {s1  = f s1  }; 2#  -> s {s2  = f s2 }; 3#  -> s {s3  = f s3 };
    4#  -> s {s4  = f s4 }; 5#  -> s {s5  = f s5  }; 6#  -> s {s6  = f s6 }; 7#  -> s {s7  = f s7 };
    8#  -> s {s8  = f s8 }; 9#  -> s {s9  = f s9  }; 10# -> s {s10 = f s10}; 11# -> s {s11 = f s11};
    12# -> s {s12 = f s12}; 13# -> s {s13 = f s13 }; 14# -> s {s14 = f s14}; 15# -> s {s15 = f s15};
    _   -> undefined
{-# INLINE modifySeq #-}

modifySeqWithSize :: Seq a -> Int# -> (Node a -> Node a) -> Int# -> Seq a
modifySeqWithSize !s@(Seq{..}) !i f !size' = case i of
    0#  -> s {s0  = f s0 , size = size' }; 1#  -> s {s1  = f s1 , size = size'}; 
    2#  -> s {s2  = f s2 , size = size' }; 3#  -> s {s3  = f s3 , size = size'};
    4#  -> s {s4  = f s4 , size = size' }; 5#  -> s {s5  = f s5 , size = size'}; 
    6#  -> s {s6  = f s6 , size = size' }; 7#  -> s {s7  = f s7 , size = size'};
    8#  -> s {s8  = f s8 , size = size' }; 9#  -> s {s9  = f s9 , size = size'}; 
    10# -> s {s10 = f s10, size = size' }; 11# -> s {s11 = f s11, size = size'};
    12# -> s {s12 = f s12, size = size' }; 13# -> s {s13 = f s13, size = size'}; 
    14# -> s {s14 = f s14, size = size' }; 15# -> s {s15 = f s15, size = size'};
    _   -> undefined
{-# INLINE modifySeqWithSize #-}


-- ****************************************************************************


#undef WIDTH 
#undef MASK  
#undef BITS 


{-# LANGUAGE
  MagicHash, BangPatterns, UnboxedTuples,
  RoleAnnotations, CPP, RankNTypes, ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-full-laziness -fno-warn-name-shadowing #-}

module Data.TrieVector.Unboxed (
      Vector(..)
    , (|>)
    , (!#)
    , unsafeIndex#
    , (!)
    , unsafeIndex
    , snoc
    , pop
    , Data.TrieVector.Unboxed.foldr
    , Data.TrieVector.Unboxed.foldl'
    , rfoldr
    , rfoldl'
    , Data.TrieVector.Unboxed.map
    , modify
    , unsafeModify
    , modify#
    , unsafeModify#
    , singleton
    , empty
    , Data.TrieVector.Unboxed.length
    , toList
    , fromList
    ) where


import GHC.Prim
import GHC.Types
import Data.Primitive.Types

import Data.TrieVector.ByteArray (ByteArray)
import qualified Data.TrieVector.ByteArray as A

import Data.TrieVector.ArrayArray (AArray)
import qualified Data.TrieVector.ArrayArray as AA

import Data.List
import Data.Word
import Data.Int

#define NODE_WIDTH 16#
#define KEY_BITS 4#
#define KEY_MASK 15#


instance Prim a => Monoid (Vector a) where
  mempty = (empty :: Vector a)
  {-# INLINABLE mempty #-}
  mappend = Data.TrieVector.Unboxed.foldl' snoc
  {-# INLINABLE mappend #-}

  
type role Vector nominal

data Vector a = Vector {
    _size, _level :: Int#,
    _init :: AArray,
    _tail :: ByteArray}

instance (Show a, Prim a) => Show (Vector a) where
    show seq = "fromList " ++ show (toList seq)    

(|>) :: Prim a => Vector a -> a -> Vector a
(|>) = snoc
infixl 5 |>
{-# INLINABLE (|>) #-}

(!) :: forall a. Prim a => Vector a -> Int -> a
(!) v (I# i) = v !# i
{-# INLINABLE (!) #-}

(!#) :: forall a. Prim a => Vector a -> Int# -> a
(!#) (Vector size level init tail) i = case i >=# 0# of 
    1# -> let
        tailSize = andI# size KEY_MASK
        initSize = size -# tailSize
        indexAA 0#    init = A.index (aa2ba init) (index i 0#)
        indexAA level init = indexAA (next level) (AA.index init (index i level))        
        in case i <# initSize of        
            1# ->
              case level of
                0# -> A.index (aa2ba init) (index i level)
                _  -> let
                  l2 = next level
                  i2 = AA.index init (index i level) in
                  case l2 of
                    0# -> A.index (aa2ba i2) (index i l2)
                    _  -> let
                      l3 = next l2
                      i3 = AA.index i2 (index i l2) in
                      case l3 of
                        0# -> A.index (aa2ba i3) (index i l3)
                        _  -> let
                          l4 = next l3
                          i4 = AA.index i3 (index i l3) in
                          case l4 of
                            0# -> A.index (aa2ba i4) (index i l4)
                            _  -> let
                              l5 = next l4
                              i5 = AA.index i4 (index i l4) in
                              case l5 of
                                0# -> A.index (aa2ba i5) (index i l5)
                                _  -> let
                                  l6 = next l5
                                  i6 = AA.index i5 (index i l5) in
                                  case l6 of
                                    0# -> A.index (aa2ba i6) (index i l6)
                                    _  -> let
                                      l7 = next l6
                                      i7 = AA.index i6 (index i l6) in
                                      indexAA l7 i7                                   
            _  -> case i <# size of
                1# -> A.index tail (i -# initSize)
                _  -> boundsError
    _  -> boundsError
{-# INLINABLE (!#) #-}

unsafeIndex :: Prim a => Vector a -> Int -> a
unsafeIndex v (I# i) = unsafeIndex# v i
{-# INLINABLE unsafeIndex #-}

unsafeIndex# :: forall a. Prim a => Vector a -> Int# -> a
unsafeIndex# (Vector size level init tail) i = let
  
    indexAA 0#    init = A.index (aa2ba init) (index i 0#)
    indexAA level init = indexAA (next level) (AA.index init (index i level))
    
    tailSize = andI# size KEY_MASK
    initSize = size -# tailSize
    in case i <# initSize of
        1# ->
          case level of
            0# -> A.index (aa2ba init) (index i level)
            _  -> let
              l2 = next level
              i2 = AA.index init (index i level) in
              case l2 of
                0# -> A.index (aa2ba i2) (index i l2)
                _  -> let
                  l3 = next l2
                  i3 = AA.index i2 (index i l2) in
                  case l3 of
                    0# -> A.index (aa2ba i3) (index i l3)
                    _  -> let
                      l4 = next l3
                      i4 = AA.index i3 (index i l3) in
                      case l4 of
                        0# -> A.index (aa2ba i4) (index i l4)
                        _  -> let
                          l5 = next l4
                          i5 = AA.index i4 (index i l4) in
                          case l5 of
                            0# -> A.index (aa2ba i5) (index i l5)
                            _  -> let
                              l6 = next l5
                              i6 = AA.index i5 (index i l5) in
                              case l6 of
                                0# -> A.index (aa2ba i6) (index i l6)
                                _  -> let
                                  l7 = next l6
                                  i7 = AA.index i6 (index i l6) in
                                  indexAA l7 i7            
        _  -> A.index tail (i -# initSize)
{-# INLINABLE unsafeIndex# #-}


snocAA :: AArray -> Int# -> Int# -> Int# -> Vector a -> AArray -> AArray
snocAA arr _    _ 0#    _     _    = arr
snocAA arr mask i level empty init = case andI# i mask of
  0# -> init1AA (snocAA arr (nextMask mask) i (next level) empty (ba2aa (_tail empty)))
  _  -> AA.modify NODE_WIDTH init
    (index i level) (snocAA arr (nextMask mask) i (next level) empty)

snoc :: forall a. Prim a => Vector a -> a -> Vector a
snoc (Vector size level init tail) v = let
    
    tailSize  = andI# size KEY_MASK
    initSize  = size -# tailSize
    size'     = size +# 1#
    tail'     =
      A.update NODE_WIDTH tail tailSize v

    in case tailSize of
        KEY_MASK -> let
          mask      = maxSize -# 1#
          prevLevel = level +# KEY_BITS
          maxSize   = uncheckedIShiftL# 1# prevLevel
          emptyVec  = empty :: Vector a
          init'     = snocAA (ba2aa tail') mask initSize level emptyVec init
          in case initSize ==# maxSize of
              0# -> Vector size' level init' (_tail emptyVec)
              _  -> Vector size' prevLevel (init2AA init init') (_tail emptyVec)
        _ -> Vector size' level init tail'
{-# INLINABLE snoc #-}


pop :: forall a. Prim a => Vector a -> (Vector a, a)
pop (Vector 0#   _     _    _   ) = popError
pop (Vector size level init tail) = let

    popArray :: Prim a => Int# -> Int# -> Int# -> Vector a -> AArray -> (# ByteArray, AArray #)
    popArray mask i level empty init = case level of
        0# -> (# aa2ba init, _init empty #)  
        _  -> case popArray (nextMask mask) i (next level) empty (AA.index init ix) of
            (# popped, newElem #) -> case andI# i mask of
                0# -> (# popped, _init empty #)          
                _  -> (# popped, AA.update width init ix newElem #)
            where ix = index i level
                  width = NODE_WIDTH
              
    tailSize  = andI# size KEY_MASK
    initSize  = size -# tailSize
    size'     = size -# 1#
    width     = NODE_WIDTH

    in case tailSize of
        0# -> let
          prevLevel = level +# KEY_BITS
          mask      = (uncheckedIShiftL# 1# prevLevel) -# 1#
          (# popped, init' #) = popArray mask size' level (empty :: Vector a) init
          in case index size' level of
              0# -> (Vector size' (next level) (AA.index init' 0#) popped, A.index popped (width -# 1#))          
              _  -> (Vector size' level init' popped, A.index popped (width -# 1#))
        _ -> (Vector size' level init tail, A.index tail (tailSize -# 1#))
{-# INLINABLE pop #-}


fromList :: Prim a => [a] -> Vector a
fromList = Data.List.foldl' snoc empty 
{-# INLINABLE fromList #-}


-- | For some reason this is not faster than naive snoccing!

-- snocArr :: forall a. Prim a => Vector a -> ByteArray# -> Vector a
-- snocArr (Vector size level init tail) arr = let
--   width     = NODE_WIDTH
--   size'     = size +# width
--   prevLevel = level +# KEY_BITS          
--   maxSize   = uncheckedIShiftL# 1# prevLevel          
--   mask      = maxSize -# 1#
--   init'     = snocAA (ba2aa arr) mask size level (empty :: Vector a) init
--   in case size ==# maxSize of
--       1# -> Vector size' prevLevel (init2AA init init') tail
--       _  -> Vector size' level init' tail
-- {-# INLINABLE snocArr #-}

-- fromList :: forall a. Prim a => [a] -> Vector a
-- fromList = go (empty :: Vector a) where
--   width = NODE_WIDTH
--   go acc@(Vector size level init tail) xs = case A.fromList' width xs of
--     (# arr, xs, consumed #) -> case consumed of
--       NODE_WIDTH -> go (snocArr acc arr) xs
--       _          -> Vector (size +# consumed) level init arr
-- {-# INLINABLE fromList #-}   


foldr :: forall a b. Prim a => (a -> b -> b) -> b -> Vector a -> b 
foldr f z (Vector size level init tail) = case initSize of
    0# -> tailRes  
    _  -> notfull (initSize -# 1#) level init tailRes

    where
        tailSize = andI# size KEY_MASK
        initSize = size -# tailSize
        tailRes  = A.foldr tailSize f z tail 

        notfull :: Int# -> Int# -> AArray -> b -> b
        notfull lasti level arr z = case level of
            0# -> A.foldr NODE_WIDTH f z (aa2ba arr)          
            _ -> AA.foldr lasti' (full level') (notfull lasti level' (AA.index arr lasti') z) arr
            where lasti' = index lasti level
                  level' = next level

        full :: Int# -> AArray -> b -> b
        full 0#    arr z = A.foldr NODE_WIDTH f z (aa2ba arr)
        full level arr z = AA.foldr NODE_WIDTH (full (next level)) z arr
{-# INLINABLE foldr #-}


rfoldr :: forall a b. Prim a => (a -> b -> b) -> b -> Vector a -> b 
rfoldr f z (Vector size level init tail) = case initSize of
    0# -> A.rfoldr tailSize f z tail   
    _  -> A.rfoldr tailSize f (notfull (initSize -# 1#) level init z) tail

    where
        tailSize = andI# size KEY_MASK
        initSize = size -# tailSize

        notfull :: Int# -> Int# -> AArray -> b -> b 
        notfull lasti level arr z = case level of
            0# -> A.rfoldr NODE_WIDTH f z (aa2ba arr)          
            _  -> notfull lasti level' (AA.index arr lasti') (AA.rfoldr lasti' (full level') z arr)
            where lasti' = index lasti level
                  level' = next level

        full :: Int# -> AArray -> b -> b
        full 0#    arr z = A.rfoldr NODE_WIDTH f z (aa2ba arr)
        full level arr z = AA.rfoldr NODE_WIDTH (full (next level)) z arr
{-# INLINABLE rfoldr #-}

foldl' :: forall a b. Prim a => (b -> a -> b) -> b -> Vector a -> b 
foldl' f z (Vector size level init tail) = case initSize of
    0# -> A.foldl' tailSize f z tail   
    _  -> A.foldl' tailSize f (notfull (initSize -# 1#) level init z) tail  

    where
        tailSize = andI# size KEY_MASK
        initSize = size -# tailSize

        notfull :: Int# -> Int# -> AArray -> b -> b 
        notfull lasti level arr z = case level of
            0# -> A.foldl' width f z (aa2ba arr)          
            _  -> notfull lasti level' (AA.index arr lasti') (AA.foldl' lasti' (full level') z arr)
            where lasti' = index lasti level
                  level' = next level
                  width = NODE_WIDTH

        full :: Int# -> b -> AArray -> b        
        full level z arr = case level of
            0# -> A.foldl' width f z (aa2ba arr)          
            _  -> AA.foldl' width (full (next level)) z arr

            where width = NODE_WIDTH
{-# INLINABLE foldl' #-}


rfoldl' :: forall a b. Prim a => (b -> a -> b) -> b -> Vector a -> b 
rfoldl' f z (Vector size level init tail) = case initSize of
    0# -> A.rfoldl' tailSize f z tail   
    _  -> notfull (initSize -# 1#) level init (A.rfoldl' tailSize f z tail) 

    where
        tailSize = andI# size KEY_MASK
        initSize = size -# tailSize

        notfull :: Int# -> Int# -> AArray -> b -> b 
        notfull lasti level arr z = case level of
            0# -> A.rfoldl' width f z (aa2ba arr)          
            _  -> AA.rfoldl' lasti' (full level') (notfull lasti level' (AA.index arr lasti') z) arr
            where lasti' = index lasti level
                  level' = next level
                  width  = NODE_WIDTH

        full :: Int# -> b -> AArray -> b
        full level z arr = case level of
            0# -> A.rfoldl' width f z (aa2ba arr)          
            _  -> AA.rfoldl' width (full (next level)) z arr
            where width = NODE_WIDTH
{-# INLINABLE rfoldl' #-}


map :: forall a b. (Prim a, Prim b) => (a -> b) -> Vector a -> Vector b 
map _ s@(Vector 0# _     _    _   ) = unsafeCoerce# s
map f (Vector size level init tail) = Vector size level init' tail' where

    tailSize = andI# size KEY_MASK
    initSize = size -# tailSize
    init' = notfull (initSize -# 1#) level init
    tail' = A.map tailSize f tail

    notfull :: Int# -> Int# -> AArray -> AArray
    notfull lasti level arr = case level of
        0# -> ba2aa (A.map NODE_WIDTH f (aa2ba arr))      
        _  -> AA.mapInitLast lasti' (full level') (notfull lasti level') arr 
        where lasti' = index lasti level
              level' = next level

    full :: Int# -> AArray -> AArray
    full level arr = case level of
        0# -> ba2aa (A.map width f (aa2ba arr))      
        _  -> AA.map width (full (next level)) arr
        where width = NODE_WIDTH
{-# INLINABLE map #-}

modify# :: forall a. Prim a => Vector a -> Int# -> (a -> a) -> Vector a 
modify# (Vector size level init tail) i f = case i >=# 0# of 
    1# -> let
        tailSize = andI# size KEY_MASK
        initSize = size -# tailSize
        width    = NODE_WIDTH
        in case i <# initSize of
            1# -> Vector size level (modifyAA i level f init) tail
            _  -> case i <# size of
                1# -> Vector size level init (A.modify' width tail (i -# initSize) f)
                _  -> boundsError
    _  -> boundsError
    where
      modifyAA i level f arr = case level of
        0# -> ba2aa (A.modify' width (aa2ba arr) (index i 0#) f)
        _  -> AA.modify width arr (index i level) (modifyAA i (next level) f)
        where width = NODE_WIDTH
{-# INLINABLE modify# #-}

unsafeModify# :: forall a. Prim a => Vector a -> Int# -> (a -> a) -> Vector a 
unsafeModify# (Vector size level init tail) i f = let
  
    modifyAA i level f arr = case level of
      0# -> ba2aa (A.modify' width (aa2ba arr) (index i 0#) f)
      _  -> AA.modify width arr (index i level) (modifyAA i (next level) f)
        where width = NODE_WIDTH
              
    tailSize = andI# size KEY_MASK
    initSize = size -# tailSize
    width    = NODE_WIDTH
    in case i <# initSize of
        1# -> Vector size level (modifyAA i level f init) tail
        _  -> Vector size level init (A.modify' width tail (i -# initSize) f)
{-# INLINABLE unsafeModify# #-}

modify :: forall a. Prim a => Vector a -> Int -> (a -> a) -> Vector a 
modify v (I# i) f = modify# v i f
{-# INLINABLE modify #-}

unsafeModify :: forall a. Prim a => Vector a -> Int -> (a -> a) -> Vector a
unsafeModify v (I# i) f = unsafeModify# v i f
{-# INLINABLE unsafeModify #-}


empty :: Prim a => Vector a
empty = Vector 0# 0# (_getArr emptyAA) (A.new NODE_WIDTH)

{-# INLINABLE empty #-}
{-# SPECIALIZE empty :: Vector Int #-}
{-# SPECIALIZE empty :: Vector Char #-}
{-# SPECIALIZE empty :: Vector Int8 #-}
{-# SPECIALIZE empty :: Vector Int16 #-}
{-# SPECIALIZE empty :: Vector Int32 #-}
{-# SPECIALIZE empty :: Vector Int64 #-}
{-# SPECIALIZE empty :: Vector Word8 #-}
{-# SPECIALIZE empty :: Vector Word16 #-}
{-# SPECIALIZE empty :: Vector Word32 #-}
{-# SPECIALIZE empty :: Vector Word64 #-}
{-# SPECIALIZE empty :: Vector Float #-}
{-# SPECIALIZE empty :: Vector Double #-}


-- | This is needed so we can have a non-polymorphic static value
-- for the empty AArray
data AAWrap = AAWrap {_getArr :: AArray}
emptyAA = AAWrap (AA.new 0# undefElem)
{-# NOINLINE emptyAA #-}

singleton :: forall a. Prim a => a -> Vector a
singleton a = Vector 1# 0# (_init (empty :: Vector a)) (init1BA a)
{-# INLINABLE singleton #-}

length :: Vector a -> Int
length (Vector size _ _ _) = I# size
{-# INLINABLE length #-}

toList :: Prim a => Vector a -> [a]
toList = Data.TrieVector.Unboxed.foldr (:) []
{-# INLINABLE toList #-}


-- Internals -----------------------------------------------------------------


next :: Int# -> Int#
next level = level -# KEY_BITS
{-# INLINE next #-}

nextMask :: Int# -> Int#
nextMask mask = uncheckedIShiftRL# mask KEY_BITS
{-# INLINE nextMask #-}

index :: Int# -> Int# -> Int#
index i level = andI# (uncheckedIShiftRL# i level) KEY_MASK
{-# INLINE index #-}

aa2ba :: AArray -> ByteArray#
aa2ba = unsafeCoerce#
{-# INLINE aa2ba #-}

ba2aa :: ByteArray# -> AArray
ba2aa = unsafeCoerce#
{-# INLINE ba2aa #-}

undefElem :: a
undefElem = error "Vector: undefined element"
{-# NOINLINE undefElem #-}

boundsError :: a
boundsError = error "TrieVector: index out of bounds"
{-# NOINLINE boundsError #-}

popError :: a
popError = error "TrieVector: can't pop from empty vector"
{-# NOINLINE popError #-}

init1BA :: Prim a => a -> ByteArray#
init1BA a = A.init1 NODE_WIDTH a
{-# INLINE init1BA #-}

init1AA :: AArray -> AArray
init1AA a = AA.init1 NODE_WIDTH a (_getArr emptyAA)
{-# INLINE init1AA #-}

init2AA :: AArray -> AArray -> AArray
init2AA a1 a2 = AA.init2 NODE_WIDTH a1 a2 (_getArr emptyAA)
{-# INLINE init2AA #-}


#undef NODE_WIDTH 
#undef KEY_BITS 
#undef KEY_MASK 

{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples, RankNTypes #-}


module IntSeq where

import Data.List (foldl')
import Data.Bits
import Data.Primitive
import Control.Monad.Primitive
import GHC.Prim
import Control.Monad.ST
import Control.Applicative
import GHC.Types


thawArray :: PrimMonad m => Array a -> m (MutableArray (PrimState m) a)
thawArray (Array arr) = primitive (\s# ->
    case thawArray# arr 0# (sizeofArray# arr) s# of
        (# s'#, arr' #) -> (# s'#, MutableArray arr' #))
{-# INLINE thawArray #-}

sizeofArray :: Array a -> Int
sizeofArray (Array arr) = I# (sizeofArray# arr)

arrayToList :: Array a -> [a]
arrayToList arr = go 0 where
    size = sizeofArray arr 
    go !i | i == size = []
    go !i = indexArray arr i : go (i + 1)

updateArray :: Array a -> Int -> a -> Array a
updateArray arr i a = runST $ do
    arr' <- thawArray arr
    writeArray arr' i a
    unsafeFreezeArray arr'
{-# INLINE updateArray #-}


data Vector a = Vector {-# UNPACK #-} !Int {-# UNPACK #-} !Int !(Array (Node a ))

data Node a 
    = Leaf a
    | Node {-# UNPACK #-} !(Array (Node a ))


bits :: Int
bits = 4

width :: Int
width = shiftL 1 bits

mask :: Int
mask = width - 1



toList :: Vector a -> [a]
toList (Vector size shift node) = go node where
    go (Node arr) = go' 0 where
        size = sizeofArray arr
        go' !i | i == size = []
        go' !i = go (indexArray arr i) ++ go' (i + 1)
    go (Leaf a) = [a]

fromList :: [a] -> Vector a
fromList = foldl' snoc IntSeq.empty


null :: Vector a -> Bool
null vec = size vec == 0 

empty :: Vector a
empty = Vector 0 0 undefined

snoc :: Vector a -> a -> Vector a
snoc a (Vector size shift node) = go node shift where
    go (Node arr) !level = 



size :: Vector a -> Int
size (Vector size _ _) = size

update :: Int -> (a -> a) -> Vector a -> Vector a
update i f (Vector size shift _) | i < 0 || i >= size = error "IntSeq.update: out of bounds"
update i f (Vector size shift node) = Vector size shift (go node shift) where
    go (Node arr) !level = Node (updateArray arr index newNode) where
        index   = shiftR i level .&. mask
        newNode = go (indexArray arr index) (level - bits)
    go (Leaf a) _ = Leaf (f a)

unsafeIndex :: Int -> Vector a -> a
unsafeIndex i (Vector size shift node) = go node shift where
    go (Node arr) !level = go (indexArray arr (shiftR i level .&. mask)) (level - bits)
    go (Leaf a)   _      = a
{-# INLINE unsafeIndex #-}

index :: Int -> Vector a -> a
index i (Vector size _ _) | i < 0 || i >= size = error "IntSeq.index: out of bounds"
index i vec = unsafeIndex i vec

lookup :: Int -> Vector a -> Maybe a
lookup i vec | i < 0 || i >= size vec = Nothing
lookup i vec = Just $ unsafeIndex i vec
{-# INLINE lookup #-}

















--{-# LANGUAGE BangPatterns, MagicHash #-}

--import Data.Word
--import GHC.Types
--import GHC.Prim
--import Data.Primitive
--import Unsafe.Coerce

--import Control.Lens
--import Numeric.Lens

--data Test = Test {-# UNPACK #-} !Int deriving (Show)

--main = do
--    arr <- newByteArray 100
--    fillByteArray arr 0 100 0
--    arr <- unsafeFreezeByteArray arr

--    let !(ByteArray barr) = arr
--        addr = unsafeCoerce# barr :: Addr#
--        i = 999 :: Int
--        foo = plusAddr# (unsafeCoerce# i :: Addr#) 5#

--    print $ (I# (indexIntOffAddr# foo 0#))     -- changes
--    print $ (I# (indexIntOffAddr# foo 1#))       -- changes


--import Data.Bits


--data Vector a = Vector !Int !Int !(Node a)


--data Node a
--    = Nil
--    | Leaf a
--    | Node !(Node a) !(Node a) !(Node a) !(Node a)
--           !(Node a) !(Node a) !(Node a) !(Node a)
--           !(Node a) !(Node a) !(Node a) !(Node a)
--           !(Node a) !(Node a) !(Node a) !(Node a)


--insert :: a -> Vector a -> Vector a
--insert a (Vector size shift node) = go node where
    


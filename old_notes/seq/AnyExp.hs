{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples #-}

import qualified Array as A
import Control.Monad.Primitive
import Data.Primitive
import Unsafe
import GHC.Prim
import Control.Monad
import Debug.Trace
import GHC.Types


-- linked list of singleton arrays

cons :: a -> Any -> Any
cons x xs = runST $ do
    cell <- newArray 2 (undefined :: Any)
    writeArray cell 0 (unsafeCoerce# x)
    writeArray cell 1 xs
    !(Array cell) <- unsafeFreezeArray cell
    return (unsafeCoerce# cell)
infixr 5 `cons`

i = 10 :: Int

l3i =  (unsafeCoerce# i) `cons` (unsafeCoerce# i) `cons` (unsafeCoerce# i)

toList :: Int -> Any -> [Int]
toList 0 xs = []
toList n a = traceShow n $ let
    xs = unsafeCoerce# a :: ArrayArray#
    (# head #) = indexArray# (unsafeCoerce# xs :: Array# Int) 0#
    tail    = indexArray# (unsafeCoerce# xs :: Array# Any) 1#
    in head : toList (n - 1) (unsafeCoerce# tail) 


toList'' :: Int -> Any -> [Int]
toList'' 0 _ = []
toList'' n a = traceShow "mulc" $ let
    xs = unsafeCoerce# a :: ArrayArray#
    (# head #) = indexArray# (unsafeCoerce# xs :: Array# Int) 0#
    tail = indexArrayArrayArray# (unsafeCoerce# xs :: ArrayArray#) 1#
    in head : toList'' (n - 1) (unsafeCoerce# tail)


freezeMA :: MutableArrayArray# s -> ST s AA
freezeMA maa = ST $ \s -> 
    case unsafeFreezeArrayArray# maa s of
        (# s, aa #) -> (# s, AA aa #) 

fromList :: [Int] -> ST s Any
fromList [] = return undefined 
fromList (x:xs) = traceShow (x:xs) $ do
    xs <- fromList xs
    cell <- newArray 2 (undefined :: Any) 
    writeArray cell 0 (unsafeCoerce# x)
    writeArray cell 1 xs
    Array a <- unsafeFreezeArray cell
    return (unsafeCoerce# a)


fromList' :: [Int] -> ST s Any 
fromList' [] = ST $ \s -> 
    case newArrayArray# 0# s of 
        (# s, maa #) -> case unsafeFreezeArrayArray# maa s of
            (# s, aa #) -> (# s, unsafeCoerce# aa #)
fromList' (x:xs) = do
    traceShow "majjal" $ return ()
    xs <- fromList' xs
    c@(MutableArray cell) <- newArray 2 (undefined :: Any)
    writeArray c 0 (unsafeCoerce# x)
    --ST $ \s -> case 
    --    writeArray# (unsafeCoerce# cell :: MutableArray# s Int) 0# x s of
    --        s -> (# s, () #)
    ST $ \s -> case
        writeArrayArrayArray# (unsafeCoerce# cell) 1# (unsafeCoerce# xs :: ArrayArray#) s of
            s -> (# s, () #)
    AA aa <- freezeMA (unsafeCoerce# cell)
    return (unsafeCoerce# aa)
    --ST $ \s -> case
    --    unsafeFreezeArrayArray# cell s of
    --        (# s, aa #) -> (# s, unsafeCoerce# aa #)

foo = runST $ fromList' [1, 2, 3]


barf :: Any -> Any -> (Any, Any)
barf a b = (b, a)

main = do
    print $ toList'' 3 foo 



--{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples #-}

--import qualified Array as A
--import Control.Monad.Primitive
--import Data.Primitive
--import Unsafe
--import GHC.Prim
--import Control.Monad
--import Debug.Trace
--import GHC.Types


---- linked list of singleton arrays

----cons :: a -> Any -> Any
----cons x xs = runST $ do
----    cell <- newArray 2 (undefined :: Any)
----    writeArray cell 0 (unsafeCoerce# x)
----    writeArray cell 1 xs
----    !(Array cell) <- unsafeFreezeArray cell
----    return (unsafeCoerce# cell)
----infixr 5 `cons`

----i = 10 :: Int

----l3i =  (unsafeCoerce# i) `cons` (unsafeCoerce# i) `cons` (unsafeCoerce# i)

----toList :: Int -> Any -> [Int]
----toList 0 xs = []
----toList n xs = let
----    !head = traceShow "head" $ A.index (unsafeCoerce# xs :: Array# Int) 0#
----    tail = (indexArrayArrayArray# (unsafeCoerce# xs :: ArrayArray#) 1#)
----    in head : toList (n - 1) (unsafeCoerce# tail) 


----fromList :: [Int] -> ST s Any
----fromList [] = return undefined 
----fromList (x:xs) = do
----    xs <- fromList xs
----    cell <- newArray 2 (undefined :: Any)
----    writeArray cell 0 (unsafeCoerce# x)
----    writeArray cell 1 xs
----    !(Array cell) <- unsafeFreezeArray cell
----    return (unsafeCoerce# cell)




--{- Stuff we can't do:
--    - trying to evaluate naked frozen array pointers (seq, !, etc.) 

--Can we wrap the arrays in a constructor, force them, then unwrap?
--Can we use ArrayArray?
---}

---- ArrayArray of lifted data. 


data AA = AA ArrayArray#
data MAA s = MAA (MutableArrayArray# s)

--i = 10000 :: Int 


newMAA :: Int -> ST s (MAA s)
newMAA (I# i) = ST $ \s -> 
    case newArrayArray# i s of 
        (# s, maa #) -> (# s, MAA maa #)


--fromList :: [Int] -> ST s AA
--fromList [] = ST $ \s -> 
--    case newArrayArray# 0# s of 
--        (# s, maa #) -> case unsafeFreezeArrayArray# maa s of
--            (# s, aa #) -> (# s, AA aa #)
--fromList (x:xs) = do
--    traceShow "majjal" $ return ()
--    (AA xs) <- fromList xs
--    (MAA cell) <- newMAA 2
--    ST $ \s -> case 
--        writeArray# (unsafeCoerce# cell :: MutableArray# s Int) 0# x s of
--            s -> (# s, () #)
--    ST $ \s -> case
--        writeArrayArrayArray# cell 1# xs s of
--            s -> (# s, () #)
--    ST $ \s -> case
--        unsafeFreezeArrayArray# cell s of
--            (# s, aa #) -> (# s, AA aa #)


--fromList' :: [Int] -> ST s Any 
--fromList' [] = ST $ \s -> 
--    case newArrayArray# 0# s of 
--        (# s, maa #) -> case unsafeFreezeArrayArray# maa s of
--            (# s, aa #) -> (# s, unsafeCoerce# aa #)
--fromList' (x:xs) = do
--    traceShow "majjal" $ return ()
--    xs <- fromList' xs
--    (MAA cell) <- newMAA 2
--    ST $ \s -> case 
--        writeArray# (unsafeCoerce# cell :: MutableArray# s Int) 0# x s of
--            s -> (# s, () #)
--    ST $ \s -> case
--        writeArrayArrayArray# cell 1# (unsafeCoerce# xs :: ArrayArray#) s of
--            s -> (# s, () #)
--    ST $ \s -> case
--        unsafeFreezeArrayArray# cell s of
--            (# s, aa #) -> (# s, unsafeCoerce# aa #)


--toList' :: Int -> Any -> [Int]
--toList' 0 _ = []
--toList' n a = let
--    !(AA xs) = unsafeCoerce# a :: AA
--    (# head #) = indexArray# (unsafeCoerce# xs :: Array# Int) 0#
--    tail = indexArrayArrayArray# xs 1#
--    in head : toList' (n - 1) (unsafeCoerce# (AA tail))


--toList'' :: Int -> Any -> [Int]
--toList'' 0 _ = []
--toList'' n a = let
--    xs = unsafeCoerce# a :: ArrayArray#
--    (# head #) = indexArray# (unsafeCoerce# xs :: Array# Int) 0#
--    tail = indexArray# (unsafeCoerce# xs :: Array# Any) 1#
--    in head : toList'' (n - 1) (unsafeCoerce# tail)


--toList :: Int -> AA -> [Int]
--toList 0 xs      = []
--toList n (AA xs) = let
--    (# head #) = indexArray# (unsafeCoerce# xs :: Array# Int) 0#
--    tail = indexArrayArrayArray# xs 1#
--    in head : toList (n - 1) (AA tail)


--foo = runST $ fromList' [0..10]

--main = do
--    --let !(AA aa) = foo 
--    print $ toList'' 10 foo





{-# LANGUAGE MagicHash, BangPatterns, UnboxedTuples #-}

import GHC.Prim
import GHC.Types
import GHC.ST
import Data.Primitive


main = do
    let x = (30, 40) :: (Int, Int)
        arr = runST $ do
            marr <- newArray 1 (undefined :: Any)
            barr <- newByteArray 100
            writeByteArray barr 1 (10000 :: Int)
            ByteArray barr <- unsafeFreezeByteArray barr
            writeArray marr 0 (unsafeCoerce# barr)
            unsafeFreezeArray marr

    print $ (indexByteArray (ByteArray (unsafeCoerce# (indexArray arr 0) :: ByteArray#)) 1 :: Int)

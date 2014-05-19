
{-# LANGUAGE MagicHash, UnboxedTuples, DeriveFunctor, DeriveFoldable, BangPatterns #-}

import qualified Data.Foldable as F
import Data.Primitive 
import Control.Lens
import Control.Applicative
import GHC.ST
import GHC.Prim
import GHC.Types


thawArray :: Array a -> ST s (MutableArray s a)
thawArray (Array arr) = ST $ \s ->
    case thawArray# arr 0# (sizeofArray# arr) s of
        (# s, marr #) -> (# s, MutableArray marr #)

sizeofArray :: Array a -> Int 
sizeofArray (Array arr) = I# (sizeofArray# arr)

newtype List a = List [a] deriving (Eq, Show, Ord, Functor, F.Foldable)

instance Traversable List where
    traverse f (List []) = pure (List [])
    traverse f (List (x:xs)) = (\x (List xs) -> List (x:xs)) <$> f x <*> traverse f (List xs)

instance Functor Array where
    fmap f arr = runST $ do
        arr' <- thawArray arr
        let !size = sizeofArray arr
            go i | i < size = do
                v <- readArray arr' i
                let !v' = f v
                writeArray arr' i (unsafeCoerce# v')
            go i = return ()
        go 0
        unsafeFreezeArray (unsafeCoerce# arr')

foldr :: (a -> b -> b) -> b -> Array a -> b
foldr f z arr = go 0 where
    go i | i < sizeofArray arr = f (indexArray arr i) (go (i + 1))
    go i = z

instance Show a => Show (Array a) where
    show = show . F.foldr (:) []

instance F.Foldable Array where
    foldr = Main.foldr





main = do
    let a = runST $ unsafeFreezeArray =<< newArray 10 (0 :: Int)
    print $ a

{-# LANGUAGE MagicHash, BangPatterns, CPP, RankNTypes, ScopedTypeVariables #-}

module Deque where

import GHC.Prim
import GHC.Types
import qualified Vector as V
import Debug.Trace

#define NODE_WIDTH 16#
#define KEY_BITS 4#
#define KEY_MASK 15#

data Deque a = Deque {
    prefix, suffix :: {-# UNPACK #-} !(V.Vector a)}

(!) :: Deque a -> Int -> a
(!) (Deque prefix suffix) (I# i) =
    traceShow ("!", I# (V._size prefix), I# (V._size prefix -# i)) $
    case i >=# 0# of
        1# -> case i <# V._size prefix of
            0# -> let i' = i -# V._size prefix in case i' <# V._size suffix of 
                1# -> V.unsafeIndex# suffix i'
                _  -> error "Deque.!: out of bounds"
            _  -> V.unsafeIndex# prefix (V._size prefix -# i -# 1#)
        _ -> error "Deque.!: out of bounds"
{-# INLINE (!) #-}

unsafeIndex :: Deque a -> Int -> a
unsafeIndex (Deque prefix suffix) (I# i) =
    case i <# V._size prefix of
        0# -> V.unsafeIndex# suffix (i -# V._size prefix)
        _  -> V.unsafeIndex# prefix (V._size prefix -# i -# 1#)
{-# INLINE unsafeIndex #-}

(|>) :: Deque a -> a -> Deque a
(|>) = snoc
infixl 5 |>
{-# INLINE (|>) #-}

(<|) :: a -> Deque a -> Deque a
(<|) = cons
infixr 6 <|
{-# INLINE (<|) #-}

snoc :: Deque a -> a -> Deque a 
snoc (Deque prefix suffix) a = Deque prefix (V.snoc suffix a)
{-# INLINE snoc #-}

cons :: a -> Deque a -> Deque a
cons a (Deque prefix suffix) = Deque (V.snoc prefix a) suffix
{-# INLINE cons #-}

map :: (a -> b) -> Deque a -> Deque b
map f (Deque prefix suffix) = Deque (V.map f prefix) (V.map f suffix)
{-# INLINE map #-}

modify :: Deque a -> Int -> (a -> a) -> Deque a
modify (Deque prefix suffix) (I# i) f =
    case i >=# 0# of
        1# -> case i <# V._size prefix of
            0# -> let i' = i -# V._size prefix in case i' <# V._size suffix of 
                1# -> Deque prefix (V.unsafeModify# suffix i' f)
                _  -> error "Deque.!: out of bounds"
            _  -> Deque (V.unsafeModify# prefix (V._size prefix -# i -# 1#) f) suffix
        _ -> error "Deque.!: out of bounds"
{-# INLINE modify #-}

unsafeModify :: Deque a -> Int -> (a -> a) -> Deque a
unsafeModify (Deque prefix suffix) (I# i) f =
    case i <# V._size prefix of
        0# -> Deque prefix (V.unsafeModify# suffix (i -# V._size prefix) f)
        _  -> Deque (V.unsafeModify# prefix (V._size prefix -# i -# 1#) f) suffix
{-# INLINE unsafeModify #-}

empty :: Deque a 
empty = Deque V.empty V.empty

singleton :: a -> Deque a
singleton a = Deque V.empty (V.singleton a)
{-# INLINE singleton #-}

length :: Deque a -> Int
length (Deque prefix suffix) = I# (V._size prefix +# V._size suffix)
{-# INLINE length #-}


-- TODO : reversed fold and traversal for Vector
instance Show a => Show (Deque a) where
    show (Deque pref suff) = "fromList " ++ show (reverse (V.toList pref) ++ V.toList suff)

fromList :: [a] -> Deque a
fromList xs = Deque V.empty (foldl (V.snoc) V.empty xs)


main = do
    print $ modify (5 <| empty |> 3 |> 5 |> 100) 4 (+ 400)
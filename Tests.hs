{-# LANGUAGE ScopedTypeVariables #-}

import qualified Vector as V 
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Test.QuickCheck.Function
import Control.Lens


main = defaultMain $ 
       localOption (QuickCheckMaxSize 1000) $
       testGroup "tests" [
          QC.testProperty "toList/fromList" $ 
            \(xs :: [Int]) -> xs == V.toList (V.fromList xs)
        , QC.testProperty "snoc" $
            \(xs :: [Int]) (x :: Int) -> (xs ++ [x]) == V.toList (V.fromList xs V.|> x)
        , QC.testProperty "append" $
            \(xs :: [Int]) (ys :: [Int]) -> (xs ++ ys) == V.toList (V.safeAppend (V.fromList xs) (V.fromList ys))
        , QC.testProperty "!" $
            \(xs :: [Int]) -> not (null xs) ==> let vec = V.fromList xs in forAll (choose (0, length xs - 1)) $ \i -> (xs !! i) == (vec V.! i)
        , QC.testProperty "pop" $
            \(xs :: [Int]) -> not (null xs) ==> ((init xs, last xs) == over _1 V.toList (V.pop (V.fromList xs)))
        , QC.testProperty "map" $
            \(xs :: [Int]) (f :: Int -> Int) -> map f xs == V.toList (V.map f (V.fromList xs))
        , QC.testProperty "foldr" $
            \(xs :: [Int]) (f :: Int -> Int -> Int) (z :: Int) -> foldr f z xs == (V.foldr f z (V.fromList xs))
        , QC.testProperty "rfoldr" $
            \(xs :: [Int]) (f :: Int -> Int -> Int) (z :: Int) -> foldr f z (reverse xs) == (V.rfoldr f z (V.fromList xs))
        , QC.testProperty "foldl'" $
            \(xs :: [Int]) (f :: Int -> Int -> Int) (z :: Int) -> foldl f z xs == (V.foldl' f z (V.fromList xs))
        , QC.testProperty "rfoldl'" $
            \(xs :: [Int]) (f :: Int -> Int -> Int) (z :: Int) -> foldl f z (reverse xs) == (V.rfoldl' f z (V.fromList xs))
        , QC.testProperty "modify" $
            \(xs :: [Int]) (f :: Int -> Int) -> not (null xs) ==> forAll (choose (0, length xs - 1)) $ \i -> (xs& ix i %~ f) == (V.toList (V.modify (V.fromList xs) i f))
        ]









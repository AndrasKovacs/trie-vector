{-# LANGUAGE ScopedTypeVariables, BangPatterns, MagicHash #-}

{-# OPTIONS -fno-full-laziness #-}

import qualified Data.TrieVector as V
import qualified Data.TrieVector.Unboxed as U

import Data.TrieVector.Array (Array)
import qualified Data.TrieVector.Array as A

import Data.TrieVector.ArrayArray (AArray)
import qualified Data.TrieVector.ArrayArray as AA

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck
import Test.QuickCheck.Function
import Control.Lens
import Data.List
import qualified Data.Foldable as F

import GHC.Types
import GHC.Prim

apply2 :: Fun a (Fun b c) -> a -> b -> c
apply2 f a b = apply (apply f a) b

fromListA :: [a] -> A.Array a
fromListA xs = let !(I# l) = length xs in A.fromList l undefined xs

indexAA' :: AArray -> Int# -> AArray
indexAA' arr i = unsafeCoerce# (indexAddrOffAddr# (unsafeCoerce# arr) (i +# 2#))
{-# INLINE indexAA' #-}

indexA' :: Array a -> Int# -> a
indexA' arr i = unsafeCoerce# (indexAddrOffAddr# (unsafeCoerce# arr) (i +# 2#))
{-# INLINE indexA' #-}

main :: IO ()
main = defaultMain $ localOption (QuickCheckMaxSize 1000) $
       testGroup "tests" [

         testGroup "Array" [
             QC.testProperty "toList/fromList" $
             \(xs :: [Int]) -> A.toList (fromListA xs) === xs

           , QC.testProperty "update" $
             \(NonEmpty (xs :: [Int])) (n :: Int) ->
             let arr = fromListA xs in
             forAll (choose (0, length xs - 1)) $ \i@(I# pi) ->
                A.toList (A.update (A.sizeof arr) arr pi n)
                === (xs& ix i .~ n)

           , QC.testProperty "modify" $
             \(NonEmpty (xs :: [Int])) (f :: Fun Int Int) ->
             let arr = fromListA xs in
             forAll (choose (0, length xs - 1)) $ \i@(I# pi) ->
                A.toList (A.modify (A.sizeof arr) arr pi (apply f))
                === (xs& ix i %~ apply f)

           , QC.testProperty "map" $
             \(xs :: [Int]) (f :: Fun Int Int) ->
             let arr = fromListA xs
             in A.toList (A.map (A.sizeof arr) (apply f) arr)
                === map (apply f) xs

           , QC.testProperty "index" $
             \(NonEmpty (xs :: [Int])) ->
              forAll (choose (0, length xs - 1)) $ \i@(I# pi) ->
              let arr = fromListA xs
              in A.index arr pi === (xs !! i)

           , QC.testProperty "new" $
             \(def :: Int) ->
             forAll (choose (0, 1000)) $ \n@(I# pn) ->
             replicate n def === A.toList (A.new pn def)

           , QC.testProperty "foldr" $
             \(xs :: [Int]) (f :: Fun Int (Fun Int Int)) (z :: Int) ->
             let arr = fromListA xs
             in foldr (apply2 f) z xs ===
                A.foldr (A.sizeof arr) (apply2 f) z arr

           , QC.testProperty "rfoldr" $
             \(xs :: [Int]) (f :: Fun Int (Fun Int Int)) (z :: Int) ->
             let arr = fromListA xs
             in foldr (apply2 f) z (reverse xs) ===
                A.rfoldr (A.sizeof arr) (apply2 f) z arr

           , QC.testProperty "foldl'" $
             \(xs :: [Int]) (f :: Fun Int (Fun Int Int)) (z :: Int) ->
             let arr = fromListA xs
             in foldl (apply2 f) z xs ===
                A.foldl' (A.sizeof arr) (apply2 f) z arr

           , QC.testProperty "rfoldl'" $
             \(xs :: [Int]) (f :: Fun Int (Fun Int Int)) (z :: Int) ->
             let arr = fromListA xs
             in foldl (apply2 f) z (reverse xs) ===
                A.rfoldl' (A.sizeof arr) (apply2 f) z arr

           , QC.testProperty "init1" $
             \((I# size) :: Int) (a :: Int) (def :: Int) ->
             ((I# size) >= 1) ==>
             (a : replicate ((I# size) - 1) def) === A.toList (A.init1 size a def)

           , QC.testProperty "init2" $
             \((I# size) :: Int) (a1 :: Int) (a2 :: Int) (def :: Int) ->
             ((I# size) >= 2) ==>
             (a1 : a2 : replicate ((I# size) - 2) def) === A.toList (A.init2 size a1 a2 def)
         ],

         testGroup "TrieVector" [

             QC.testProperty "length/fromList" $
               \(xs :: [Int]) -> length xs === V.length (V.fromList xs)

           , QC.testProperty "toList/fromList" $
               \(xs :: [Int]) -> xs === F.toList (V.fromList xs)

           , QC.testProperty "snoc" $
               \(xs :: [Int]) (x :: Int) ->
                 (xs ++ [x]) === F.toList (V.fromList xs V.|> x)

           , QC.testProperty "append" $
               \(xs :: [Int]) (ys :: [Int]) ->
                 (xs ++ ys) === F.toList (mappend (V.fromList xs) (V.fromList ys))

           , QC.testProperty "!" $
               \(xs :: [Int]) -> not (null xs) ==>
                  let vec = V.fromList xs in
                  forAll (choose (0, length xs - 1)) $ \i ->
                  (xs !! i) === (vec V.! i)

           , QC.testProperty "pop" $
               \(xs :: [Int]) -> not (null xs) ==>
                  ((init xs, last xs) === over _1 F.toList (V.pop (V.fromList xs)))

           , QC.testProperty "map" $
               \(xs :: [Int]) (f :: Fun Int Int) ->
                 map (apply f) xs === F.toList (V.map (apply f) (V.fromList xs))

           , QC.testProperty "foldr" $
               \(xs :: [Int]) (f :: Fun Int (Fun Int Int)) (z :: Int) ->
                 foldr (apply2 f) z xs === (V.foldr (apply2 f) z (V.fromList xs))

           , QC.testProperty "rfoldr" $
               \(xs :: [Int]) (f :: Fun Int (Fun Int Int)) (z :: Int) ->
                 foldr (apply2 f) z (reverse xs) ===
                 (V.rfoldr (apply2 f) z (V.fromList xs))

           , QC.testProperty "foldl'" $
               \(xs :: [Int]) (f :: Fun Int (Fun Int Int)) (z :: Int) ->
                 foldl (apply2 f) z xs ===
                 (V.foldl' (apply2 f) z (V.fromList xs))

           , QC.testProperty "rfoldl'" $
               \(xs :: [Int]) (f :: Fun Int (Fun Int Int)) (z :: Int) ->
                 foldl (apply2 f) z (reverse xs) ===
                 (V.rfoldl' (apply2 f) z (V.fromList xs))

           , QC.testProperty "modify" $
               \(xs :: [Int]) (f :: Fun Int Int) -> not (null xs) ==>
                 forAll (choose (0, length xs - 1)) $ \i ->
                 (xs& ix i %~ apply f) ===
                 (F.toList (V.modify (V.fromList xs) i (apply f)))

           , QC.testProperty "noCopyModify'#" $
               \(xs :: [Int]) (f :: Fun Int Int) -> not (null xs) ==>
                 forAll (choose (0, length xs - 1)) $ \i@(I# pi) ->
                 (xs& ix i %~ apply f) ===
                 (F.toList (V.noCopyModify'# (V.fromList xs) pi (apply f)))

           , localOption (QuickCheckMaxSize 200) $ QC.testProperty "append" $
               \(xs :: [Int]) (ys :: [Int]) ->
               (xs ++ ys) === F.toList (V.fromList xs `mappend` V.fromList ys)

           , QC.testProperty "reverse" $
             \(xs :: [Int]) -> reverse xs === F.toList (V.reverse (V.fromList xs))

           , QC.testProperty "inits" $
             \(xs :: [Int]) -> reverse (inits xs) === map F.toList (V.inits (V.fromList xs))

           , QC.testProperty "revTails" $
             \(xs :: [Int]) -> map reverse (tails xs)
                               === map F.toList (V.revTails (V.fromList xs))

           ],

         testGroup "TrieVector.Unboxed" [

             QC.testProperty "toList/fromList" $
               \(xs :: [Int]) -> xs === U.toList (U.fromList xs)

           , QC.testProperty "snoc" $
               \(xs :: [Int]) (x :: Int) ->
                 (xs ++ [x]) === U.toList (U.fromList xs U.|> x)

           , QC.testProperty "pop" $
               \(xs :: [Int]) -> not (null xs) ==>
                  ((init xs, last xs) === over _1 U.toList (U.pop (U.fromList xs)))

           , QC.testProperty "!" $
               \(xs :: [Int]) -> not (null xs) ==>
                  let vec = U.fromList xs in
                  forAll (choose (0, length xs - 1)) $ \i -> (xs !! i) === (vec U.! i)

           , QC.testProperty "map" $
               \(xs :: [Int]) (f :: Fun Int Int) ->
                 map (apply f) xs === U.toList (U.map (apply f) (U.fromList xs))

           , QC.testProperty "foldr" $
               \(xs :: [Int]) (f :: Fun Int (Fun Int Int)) (z :: Int) ->
                 foldr (apply2 f) z xs === (U.foldr (apply2 f) z (U.fromList xs))

           , QC.testProperty "rfoldr" $
               \(xs :: [Int]) (f :: Fun Int (Fun Int Int)) (z :: Int) ->
                 foldr (apply2 f) z (reverse xs) === (U.rfoldr (apply2 f) z (U.fromList xs))

           , QC.testProperty "foldl'" $
               \(xs :: [Int]) (f :: Fun Int (Fun Int Int)) (z :: Int) ->
                 foldl (apply2 f) z xs === (U.foldl' (apply2 f) z (U.fromList xs))

           , QC.testProperty "rfoldl'" $
               \(xs :: [Int]) (f :: Fun Int (Fun Int Int)) (z :: Int) ->
                 foldl (apply2 f) z (reverse xs) === (U.rfoldl' (apply2 f) z (U.fromList xs))

           , QC.testProperty "modify" $
               \(xs :: [Int]) (f :: Fun Int Int) -> not (null xs) ==>
                 forAll (choose (0, length xs - 1)) $ \i ->
                 (xs& ix i %~ apply f) === (U.toList (U.modify (U.fromList xs) i (apply f)))

           , QC.testProperty "noCopyModify'#" $
               \(xs :: [Int]) (f :: Fun Int Int) -> not (null xs) ==>
                 forAll (choose (0, length xs - 1)) $ \i@(I# pi) ->
                 (xs& ix i %~ apply f) ===
                 (U.toList (U.noCopyModify'# (U.fromList xs) pi (apply f)))
           ]
     ]


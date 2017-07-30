{-# LANGUAGE MagicHash, BangPatterns #-}

import GHC.Types
import GHC.Prim

import qualified Data.TrieVector as TV
import qualified Data.TrieVector.Unboxed as TUV

import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S

tvn :: Int -> ()
tvn n = seq (TV.fromList [1..n]) ()

tuvn :: Int -> ()
tuvn n = seq (TUV.fromList [1..n]) ()

vn :: Int -> ()
vn n = seq (V.fromList [1..n]) ()

uvn :: Int -> ()
uvn n = seq (UV.fromList [1..n]) ()

hn :: Int -> ()
hn n = seq (HM.fromList $ zip [0..n - 1] [0..n - 1]) ()

sn :: Int -> ()
sn n = seq (S.fromList [0..n]) ()

main :: IO ()
main = print $ TV.fromList [0..10]











{-
------------------------------------------------------------

-- OPTIMIZING MEMORY LAYOUT IN GHC --
--     A brief case study          --

------------------------------------------------------------

GOAL: Clojure-style radix trie vectors for Haskell
  - Int-keyed sequence, fast random read/modify,
    fast (but not O(1)) snoc
  - linear concatenation

------------------------------------------------------------

Leafy tree for bitstrings:

data Trie a = Empty | Leaf a | Node (Trie a) (Trie a)

Lookup :
  - iterate over each bit starting from the most significant one
  - if 0, take left subtree, else take right

------------------------------------------------------------

Scaling up:
  - make Node 16-ary
  - when doing lookup/modify, read 4-bit chunks of the key

------------------------------------------------------------

Naive Haskell implementation:

import Data.Vector

data Trie a =
  Empty | Leaf a | Node {-# unpack #-} !(Vector (Trie a))

------------------------------------------------------------

BAD.

Layout of (Vector a):
   
  info | Int# | Int# | Array# a

                         |
                         ˇ

           info | Int# | Int# | a₀ | a₁ | a₂ ... aₙ | Int#

Administrative overhead: 7 words, 1 indirection

------------------------------------------------------------

Johan Tibell, annoyed by Array#, implemented SmallArray# for GHC 7.8, intended
for use in "unordered-containers"

   | info | Int# | a₀ | a₁ | a₂ ... aₙ

There is only a sigle Int# overhead for size.

------------------------------------------------------------

However, what we would actually like is the following structure

  - We know the overall depth of the tree from the size
    - (take logarithm, preferably with native bit-fiddling operations)
  - We know whether the next constructor is Leaf or Node by tracking depth.
  - So, we can implement the structure with a single nested primitive array.

  import GHC.Prim -- This is our new Prelude

  type role Trie nominal

  data Trie a = Trie {
    size     :: !Int,
    contents :: SmallArray# Any}


   |
  info | Int# | trie₀ ... trieₙ
                  |
                info | Int# | trie₀ ... trieₙ
                                |


We have "a"-s in the bottom of the tree.

GHC RTS can handle this fine.

Coercing (SmallArray# a) to Any is a bit delicate (if you try to "seq" a primitive Array#, you get an RTS exception)

------------------------------------------------------------

What we gain:
  - Less then half of the number of indirections to elements
  - very low space overhead (<1.2 word / element)

Speed:
  - lookup: 20-30x faster than Data.Sequence
            5x     faster than Data.HashMap (note hash function)

  - modify: 2-3x faster than Data.Sequence
  -         1.5-2x faster than Data.Hashmap

------------------------------------------------------------

TODO:
  - make a proper library from this
    - make internals less horrifying
  - rewrite Data.HashMap using nested primitive arrays
  - write a bunch of libraries with minimal space overhead
    - try to uniformly support boxed, unboxed, and unlifted data

-}























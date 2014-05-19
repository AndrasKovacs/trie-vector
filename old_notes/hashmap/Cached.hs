{-# LANGUAGE MagicHash, BangPatterns, ScopedTypeVariables #-}

import GHC.Types
import GHC.Prim

import qualified Array as A
import qualified ArrayArray as AA
import qualified ByteArray as BA 

import Data.Hashable

ptrEq = reallyUnsafePtrEquality#

data Leaf k v = L !k v 

data HashMap k v
    = Empty
    | BitmapIndexed Int# !(HashMap k v) !(Array# (HashMap k v))
    | Leaf Int# !(Leaf k v)
    | Full Int# !(HashMap k v) !(Array# (HashMap k v))
    | Collision Int# !(Array# (Leaf k v))


insert :: forall k v. (Eq k, Hashable k) => k -> v -> HashMap k v -> HashMap k v
insert k0 v0 m0 = go h0 k0 v0 0# m0 where
    !(I# h0) = hash k0
    go :: Int# -> k -> v -> Int# -> HashMap k v -> HashMap k v 
    go h !k x _ Empty = Leaf h (L k x)
    go h k x s t@(Leaf hy l@(L ky y))
        | hy == h = if ky == k
                    then if x `ptrEq` y
                         then t
                         else Leaf h (L k x)
                    else Collision h (A.init2 16# l (L k x))
        | otherwise = runST (two s h k x hy ky y)




-- | Create a map from two key-value pairs which hashes don't collide.
two :: Shift -> Hash -> k -> v -> Hash -> k -> v -> ST s (HashMap k v)
two = go
  where
    go s h1 k1 v1 h2 k2 v2
        | bp1 == bp2 = do
            st <- go (s+bitsPerSubkey) h1 k1 v1 h2 k2 v2
            ary <- A.singletonM st
            return $! BitmapIndexed bp1 ary
        | otherwise  = do
            mary <- A.new 2 $ Leaf h1 (L k1 v1)
            A.write mary idx2 $ Leaf h2 (L k2 v2)
            ary <- A.unsafeFreeze mary
            return $! BitmapIndexed (bp1 .|. bp2) ary
      where
        bp1  = mask h1 s
        bp2  = mask h2 s
        idx2 | index h1 s < index h2 s = 1
             | otherwise               = 0
{-# INLINE two #-}

--insert :: (Eq k, Hashable k) => k -> v -> HashMap k v -> HashMap k v
--insert k0 v0 m0 = go h0 k0 v0 0 m0
--  where
--    h0 = hash k0
--    go !h !k x !_ Empty = Leaf h (L k x)
--    go h k x s t@(Leaf hy l@(L ky y))
--        | hy == h = if ky == k
--                    then if x `ptrEq` y
--                         then t
--                         else Leaf h (L k x)
--                    else collision h l (L k x)
--        | otherwise = runST (two s h k x hy ky y)
--    go h k x s t@(BitmapIndexed b ary)
--        | b .&. m == 0 =
--            let !ary' = A.insert ary i $! Leaf h (L k x)
--            in bitmapIndexedOrFull (b .|. m) ary'
--        | otherwise =
--            let !st  = A.index ary i
--                !st' = go h k x (s+bitsPerSubkey) st
--            in if st' `ptrEq` st
--               then t
--               else BitmapIndexed b (A.update ary i st')
--      where m = mask h s
--            i = sparseIndex b m
--    go h k x s t@(Full ary) =
--        let !st  = A.index ary i
--            !st' = go h k x (s+bitsPerSubkey) st
--        in if st' `ptrEq` st
--            then t
--            else Full (update16 ary i st')
--      where i = index h s
--    go h k x s t@(Collision hy v)
--        | h == hy   = Collision h (updateOrSnocWith const k x v)
--        | otherwise = go h k x s $ BitmapIndexed (mask hy s) (A.singleton t)
-- {-# INLINABLE insert #-}   
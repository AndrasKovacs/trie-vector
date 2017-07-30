
{-# language
  MagicHash, BangPatterns, CPP, RankNTypes,
  RoleAnnotations, UnboxedTuples, ScopedTypeVariables #-}

{-# options_ghc -fno-full-laziness -fno-warn-name-shadowing #-}

module Data.TrieVector (
      Vector(..)
    , (|>)
    , (!#)
    , (!)
    , snoc
    , Data.TrieVector.foldr
    , Data.TrieVector.foldl'
    , rfoldr
    , rfoldl'
    , Data.TrieVector.map
    , modify
    , unsafeModify
    , modify#
    , unsafeModify#
    , noCopyModify'#
    , singleton
    , empty
    , Data.TrieVector.length
    , fromList
    , snocFromList
    , pop
    , Data.TrieVector.reverse
    , Data.TrieVector.inits
    , revTails
    ) where

import qualified Data.Foldable as F

import GHC.Prim
import GHC.Types
import Data.List

import Data.TrieVector.Array (Array)
import qualified Data.TrieVector.Array as A

import Data.TrieVector.ArrayArray (AArray)
import qualified Data.TrieVector.ArrayArray as AA

import Debug.Trace

#define NODE_WIDTH 16#
#define KEY_BITS 4#
#define KEY_MASK 15#

type role Vector nominal

data Vector a = Vector {
    _size, _level :: Int#,
    _init :: AArray,
    _tail :: Array a}

instance Functor Vector where
    fmap = Data.TrieVector.map
    {-# inline fmap #-}

instance Show a => Show (Vector a) where
    show v = show (F.toList v)

instance F.Foldable Vector where
    foldr = Data.TrieVector.foldr
    {-# inline foldr #-}
    foldr' f = Data.TrieVector.rfoldl' (flip f)
    {-# inline foldr' #-}
    foldl f = Data.TrieVector.rfoldr (flip f)
    {-# inline foldl #-}
    foldl' = Data.TrieVector.foldl'
    {-# inline foldl' #-}
    length = Data.TrieVector.length
    {-# inline length #-}
    null v = Data.TrieVector.length v == 0
    {-# inline null #-}

instance Monoid (Vector a) where
  mempty  = empty
  mappend = Data.TrieVector.foldl' snoc
  {-# inline mappend #-}

instance Traversable Vector where
  traverse f = fmap fromList . traverse f . F.toList
  {-# inline traverse #-}

(|>) :: Vector a -> a -> Vector a
(|>) = snoc
infixl 5 |>
{-# inline (|>) #-}

infixl 5 !
(!) :: forall a. Vector a -> Int -> a
(!) v (I# i) = v !# i
{-# inline (!) #-}

(!#) :: forall a. Vector a -> Int# -> a  
(!#) (Vector size level init tail) i = case i >=# 0# of 
    1# -> let
        tailSize = andI# size KEY_MASK
        initSize = size -# tailSize
        indexAA 0#    init = A.index (aa2a init) (index i 0#)
        indexAA level init = indexAA (next level) (AA.index init (index i level))        
        in case i <# initSize of        
            1# ->
              case level of
                0# -> A.index (aa2a init) (index i level)
                _  -> let
                  l2 = next level
                  i2 = AA.index init (index i level) in
                  case l2 of
                    0# -> A.index (aa2a i2) (index i l2)
                    _  -> let
                      l3 = next l2
                      i3 = AA.index i2 (index i l2) in
                      case l3 of
                        0# -> A.index (aa2a i3) (index i l3)
                        _  -> let
                          l4 = next l3
                          i4 = AA.index i3 (index i l3) in
                          case l4 of
                            0# -> A.index (aa2a i4) (index i l4)
                            _  -> let
                              l5 = next l4
                              i5 = AA.index i4 (index i l4) in
                              case l5 of
                                0# -> A.index (aa2a i5) (index i l5)
                                _  -> let
                                  l6 = next l5
                                  i6 = AA.index i5 (index i l5) in
                                  case l6 of
                                    0# -> A.index (aa2a i6) (index i l6)
                                    _  -> let
                                      l7 = next l6
                                      i7 = AA.index i6 (index i l6) in
                                      indexAA l7 i7                                   
            _  -> case i <# size of
                1# -> A.index tail (i -# initSize)
                _  -> boundsError
    _  -> boundsError
{-# inline (!#) #-}

snocAA :: AArray -> Int# -> Int# -> Int# -> AArray -> AArray
snocAA arr _    _ 0#    _    = arr
snocAA arr mask i level init = case andI# i mask of
  0# -> init1AA (snocAA arr (nextMask mask) i (next level) (_init empty))
  _  -> AA.modify NODE_WIDTH init (index i level) (snocAA arr (nextMask mask) i (next level))

snoc :: forall a. Vector a -> a -> Vector a
snoc (Vector size level init tail) v = let
    tailSize  = andI# size KEY_MASK
    initSize  = size -# tailSize
    size'     = size +# 1#
    tail'     =
      A.update NODE_WIDTH tail tailSize v

    in case tailSize of
        KEY_MASK -> let
          prevLevel = level +# KEY_BITS          
          maxSize   = uncheckedIShiftL# 1# prevLevel          
          mask      = maxSize -# 1#
          init'     = snocAA (a2aa tail') mask initSize level init
          in case initSize ==# maxSize of
              1# -> Vector size' prevLevel (init2AA init init') (_tail empty)          
              _  -> Vector size' level init' (_tail empty)
        _ -> Vector size' level init tail'
{-# inline snoc #-} 

popArray :: Int# -> Int# -> Int# -> AArray -> (# Array a, AArray #)
popArray mask i level init = case level of
    0# -> (# aa2a init, _init empty #)  
    _  -> case popArray (nextMask mask) i (next level) (AA.index init ix) of
        (# popped, newElem #) -> case andI# i mask of
            0# -> (# popped, _init empty #)          
            _  -> (# popped, AA.update width init ix newElem #)
        where ix = index i level
              width = NODE_WIDTH
{-# inline popArray #-}

pop :: forall a. Vector a -> (Vector a, a)
pop (Vector 0#   _     _    _   ) = popError
pop (Vector size level init tail) = let
    tailSize  = andI# size KEY_MASK
    size'     = size -# 1#
    width     = NODE_WIDTH

    in case tailSize of
        0# -> let
          prevLevel = level +# KEY_BITS
          mask      = (uncheckedIShiftL# 1# prevLevel) -# 1#
          (# popped, init' #) = popArray mask size' level init
          in case index size' level of
              0# -> (Vector size' (next level) (AA.index init' 0#) popped, A.index popped (width -# 1#))          
              _  -> (Vector size' level init' popped, A.index popped (width -# 1#))
        _ -> let lasti = tailSize -# 1# in 
          (Vector size' level init (A.update width tail lasti undefElem), A.index tail lasti)
{-# inline pop #-}

snocFromList :: [a] -> Vector a
snocFromList = Data.List.foldl' snoc empty
{-# inline snocFromList #-}

snocArr :: forall a. Vector a -> Array a -> Vector a
snocArr (Vector size level init tail) arr = let
  width     = NODE_WIDTH
  size'     = size +# width
  prevLevel = level +# KEY_BITS          
  maxSize   = uncheckedIShiftL# 1# prevLevel          
  mask      = maxSize -# 1#
  init'     = snocAA (a2aa arr) mask size level init
  in case size ==# maxSize of
      1# -> Vector size' prevLevel (init2AA init init') tail
      _  -> Vector size' level init' tail
{-# inline snocArr #-}     

fromList :: [a] -> Vector a
fromList = go empty where
  width = NODE_WIDTH
  go acc@(Vector size level init _) xs = case A.fromList' width undefElem xs of
    (# arr, xs, consumed #) -> case consumed of
      NODE_WIDTH -> go (snocArr acc arr) xs
      _          -> Vector (size +# consumed) level init arr
{-# inline fromList #-}                        

foldr :: forall a b. (a -> b -> b) -> b -> Vector a -> b 
foldr f z (Vector size level arr tail) = case initSize of
    0# -> tailRes  
    _  -> notfull (initSize -# 1#) level arr tailRes

    where
        tailSize = andI# size KEY_MASK
        initSize = size -# tailSize
        tailRes = A.foldr tailSize f z tail 

        notfull :: Int# -> Int# -> AArray -> b -> b
        notfull _     0#    arr z = A.foldr NODE_WIDTH f z (aa2a arr)
        notfull lasti level arr z =
            AA.foldr lasti' (full level') (notfull lasti level' (AA.index arr lasti') z) arr
            where lasti' = index lasti level
                  level' = next level

        full :: Int# -> AArray -> b -> b
        full 0#    arr z = A.foldr NODE_WIDTH f z (aa2a arr)
        full level arr z = AA.foldr NODE_WIDTH (full (next level)) z arr
{-# inline foldr #-}


rfoldr :: forall a b. (a -> b -> b) -> b -> Vector a -> b 
rfoldr f z (Vector size level init tail) = case initSize of
    0# -> A.rfoldr tailSize f z tail   
    _  -> A.rfoldr tailSize f (notfull (initSize -# 1#) level init z) tail

    where
        tailSize = andI# size KEY_MASK
        initSize = size -# tailSize

        notfull :: Int# -> Int# -> AArray -> b -> b 
        notfull lasti level arr z = case level of
            0# -> A.rfoldr NODE_WIDTH f z (aa2a arr)
            _  -> notfull lasti level' (AA.index arr lasti') (AA.rfoldr lasti' (full level') z arr) 
            where lasti' = index lasti level
                  level' = next level

        full :: Int# -> AArray -> b -> b
        full 0#    arr z = A.rfoldr NODE_WIDTH f z (aa2a arr)
        full level arr z = AA.rfoldr NODE_WIDTH (full (next level)) z arr
{-# inline rfoldr #-}

foldl' :: forall a b. (b -> a -> b) -> b -> Vector a -> b 
foldl' f z (Vector size level init tail) = case initSize of
    0# -> A.foldl' tailSize f z tail   
    _  -> A.foldl' tailSize f (notfull (initSize -# 1#) level init z) tail  

    where
        tailSize = andI# size KEY_MASK
        initSize = size -# tailSize

        notfull :: Int# -> Int# -> AArray -> b -> b 
        notfull lasti level arr z = case level of
            0#  -> A.foldl' width f z (aa2a arr)          
            _   -> notfull lasti level' (AA.index arr lasti') (AA.foldl' lasti' (full level') z arr)
            where lasti' = index lasti level
                  level' = next level
                  width  = NODE_WIDTH

        full :: Int# -> b -> AArray -> b
        full level z arr = case level of
            0# -> A.foldl' width f z (aa2a arr)          
            _  -> AA.foldl' width (full (next level)) z arr
            where width = NODE_WIDTH
{-# inline foldl' #-}

rfoldl' :: forall a b. (b -> a -> b) -> b -> Vector a -> b 
rfoldl' f z (Vector size level init tail) = case initSize of
    0# -> A.rfoldl' tailSize f z tail   
    _  -> notfull (initSize -# 1#) level init (A.rfoldl' tailSize f z tail)  
    where
        tailSize = andI# size KEY_MASK
        initSize = size -# tailSize

        notfull :: Int# -> Int# -> AArray -> b -> b 
        notfull lasti level arr z = case level ># 0# of
            1# -> AA.rfoldl' lasti' (full level') (notfull lasti level' (AA.index arr lasti') z) arr
            _  -> A.rfoldl' width f z (aa2a arr)
            where lasti' = index lasti level
                  level' = next level
                  width = NODE_WIDTH

        full :: Int# -> b -> AArray -> b
        full level z arr = case level ># 0# of
            1# -> AA.rfoldl' width (full (next level)) z arr
            _  -> A.rfoldl' width f z (aa2a arr)
            where width = NODE_WIDTH
{-# inline rfoldl' #-}

map :: forall a b. (a -> b) -> Vector a -> Vector b 
map _ s@(Vector 0# _      _    _  ) = unsafeCoerce# s
map f (Vector size level init tail) = Vector size level init' tail' where

    tailSize = andI# size KEY_MASK
    initSize = size -# tailSize
    init' = notfull (initSize -# 1#) level init
    tail' = A.map tailSize f tail

    notfull :: Int# -> Int# -> AArray -> AArray
    notfull lasti level arr = case level of
        0# -> a2aa (A.map NODE_WIDTH f (aa2a arr))      
        _  -> AA.mapInitLast lasti' (full level') (notfull lasti level') arr 
        where lasti' = index lasti level
              level' = next level

    full :: Int# -> AArray -> AArray
    full 0#    arr = a2aa (A.map NODE_WIDTH f (aa2a arr))
    full level arr = AA.map NODE_WIDTH (full (next level)) arr
{-# inline map #-}

modify# :: forall a. Vector a -> Int# -> (a -> a) -> Vector a 
modify# (Vector size level init tail) i f = case i >=# 0# of 
    1# -> let
        modifyAA 0#    arr = a2aa (A.modify NODE_WIDTH (aa2a arr) (index i 0#) f)
        modifyAA level arr = AA.modify NODE_WIDTH arr (index i level) (modifyAA (next level))
      
        tailSize = andI# size KEY_MASK
        initSize = size -# tailSize
        in case i <# initSize of
            1# -> Vector size level (modifyAA level init) tail     
            _  -> case i <# size of
                1# -> Vector size level init (A.modify NODE_WIDTH tail (i -# initSize) f)
                _  -> boundsError        
    _  -> boundsError
{-# inline modify# #-}

unsafeModify# :: forall a. Vector a -> Int# -> (a -> a) -> Vector a 
unsafeModify# (Vector size level init tail) i f = let
    modifyAA 0#    arr = a2aa (A.modify NODE_WIDTH (aa2a arr) (index i 0#) f)
    modifyAA level arr = AA.modify NODE_WIDTH arr (index i level) (modifyAA (next level))
      
    tailSize = andI# size KEY_MASK
    initSize = size -# tailSize
    in case i <# initSize of
        1# -> Vector size level (modifyAA level init) tail
        _  -> Vector size level init (A.modify NODE_WIDTH tail (i -# initSize) f)
{-# inline unsafeModify# #-}

modify :: forall a. Vector a -> Int -> (a -> a) -> Vector a 
modify v (I# i) f = modify# v i f
{-# inline modify #-}

unsafeModify :: forall a. Vector a -> Int -> (a -> a) -> Vector a
unsafeModify v (I# i) f = unsafeModify# v i f
{-# inline unsafeModify #-}

noCopyModify'# :: forall a. Vector a -> Int# -> (a -> a) -> Vector a
noCopyModify'# v@(Vector size level init tail) i f = case i >=# 0# of
  1# -> let
    width = NODE_WIDTH
    modifyAA 0#    arr = a2aa (A.noCopyModify' width (aa2a arr) (index i 0#) f)
    modifyAA level arr = AA.noCopyModify width arr (index i level) (modifyAA (next level))

    tailSize = andI# size KEY_MASK
    initSize = size -# tailSize
    in case i <# initSize of
        1# -> let
          init' = modifyAA level init in
          case AA.ptrEq init init' of            
            1# -> v
            _  -> Vector size level init' tail
        _  -> case i <# size of
            1# -> let
              tail' = A.noCopyModify' width tail (i -# initSize) f in
              case AA.ptrEq (a2aa tail) (a2aa tail') of
                1# -> v
                _  -> Vector size level init tail'
            _  -> boundsError        
  _  -> boundsError
{-# inline noCopyModify'# #-}

reverse :: Vector a -> Vector a
reverse v = rfoldl' snoc empty v
{-# inline reverse #-}

-- | Note: lists inits in reversed order compared to Data.List.inits !
inits :: Vector a -> [Vector a]
inits = go where
  go v | F.null v = [v]
  go v = v : go (fst $ pop v)
{-# inline inits #-}

revTails :: Vector a -> [Vector a]
revTails = Data.TrieVector.inits . Data.TrieVector.reverse where
{-# inline revTails #-}  

empty :: forall a. Vector a
empty = Vector 0# 0# (AA.new 0# undefElem) (A.new NODE_WIDTH undefElem)
{-# noinline empty #-}

singleton :: a -> Vector a
singleton a = Vector 1# 0# (_init empty) (init1A a)
{-# inline singleton #-}

length :: Vector a -> Int
length (Vector size _ _ _) = I# size
{-# inline length #-}

-- Low-level fiddling
--------------------------------------------------------------------------------

next :: Int# -> Int#
next level = level -# KEY_BITS
{-# inline next #-}

nextMask :: Int# -> Int#
nextMask mask = uncheckedIShiftRL# mask KEY_BITS
{-# inline nextMask #-}

index :: Int# -> Int# -> Int#
index i level = andI# (uncheckedIShiftRL# i level) KEY_MASK
{-# inline index #-}

aa2a :: AArray -> Array a
aa2a = unsafeCoerce#
{-# inline aa2a #-}

a2aa :: Array a -> AArray
a2aa = unsafeCoerce#
{-# inline a2aa #-}

boundsError :: a
boundsError = error "TrieVector: index out of bounds"
{-# noinline boundsError #-}

undefElem :: a
undefElem = error "Vector: undefined element"
{-# noinline undefElem #-}

popError :: a
popError = error "TrieVector: can't pop from empty vector"
{-# noinline popError #-}

init1A :: a -> Array a
init1A a = A.init1 NODE_WIDTH a undefElem
{-# inline init1A #-}

init1AA :: AArray -> AArray
init1AA a = AA.init1 NODE_WIDTH a (_tail empty)
{-# inline init1AA #-}

init2AA :: AArray -> AArray -> AArray
init2AA a1 a2 = AA.init2 NODE_WIDTH a1 a2 (_tail empty)
{-# inline init2AA #-}

nFields :: Addr# -> Int# -> [Int]
nFields addr n = go 0# where
  go i = case i ==# n of
    1# -> []
    _  -> (I# (indexIntOffAddr# addr i)) : go (i +# 1#)
{-# inline nFields #-}
                    
#undef NODE_WIDTH 
#undef KEY_BITS 
#undef KEY_MASK
#undef NODE_SIZE


{-# LANGUAGE CPP, MagicHash, BangPatterns #-}

import Data.Word
import GHC.Types
import GHC.Prim
import Data.Bits

#if defined(__GLASGOW_HASKELL__)
# include "MachDeps.h"
#endif

-- Use macros to define strictness of functions.
-- STRICT_x_OF_y denotes an y-ary function strict in the x-th parameter.
-- We do not use BangPatterns, because they are not in any standard and we
-- want the compilers to be compiled by as many compilers as possible.
#define STRICT_1_OF_2(fn) fn arg _ | arg `seq` False = undefined
#define STRICT_2_OF_2(fn) fn _ arg | arg `seq` False = undefined
#define STRICT_1_OF_3(fn) fn arg _ _ | arg `seq` False = undefined
#define STRICT_2_OF_3(fn) fn _ arg _ | arg `seq` False = undefined


-- A "Nat" is a natural machine word (an unsigned Int)
type Nat = Word

natFromInt :: Int -> Nat
natFromInt i = fromIntegral i
{-# INLINE natFromInt #-}

intFromNat :: Nat -> Int
intFromNat w = fromIntegral w
{-# INLINE intFromNat #-}


lowestBitSet :: Nat -> Int

{-# INLINE lowestBitSet #-}




shiftRL (W# i) (I# n) = W# (uncheckedShiftRL# i n) 

indexOfTheOnlyBit :: Nat -> Int
{-# INLINE indexOfTheOnlyBit #-}
indexOfTheOnlyBit bitmask =
  I# (lsbArray `indexInt8OffAddr#` unboxInt (intFromNat ((bitmask * magic) `shiftRL` offset)))
  where unboxInt (I# i) = i
#if WORD_SIZE_IN_BITS==32
        magic = 0x077CB531
        offset = 27
        !lsbArray = "\0\1\28\2\29\14\24\3\30\22\20\15\25\17\4\8\31\27\13\23\21\19\16\7\26\12\18\6\11\5\10\9"#
#else
        magic = 0x07EDD5E59A4E28C2
        offset = 58
        !lsbArray = "\63\0\58\1\59\47\53\2\60\39\48\27\54\33\42\3\61\51\37\40\49\18\28\20\55\30\34\11\43\14\22\4\62\57\46\52\38\26\32\41\50\36\17\19\29\10\13\21\56\45\25\31\35\16\9\12\44\24\15\8\23\7\6\5"#
#endif

lowestBitMask :: Nat -> Nat
lowestBitMask x = x .&. negate x
{-# INLINE lowestBitMask #-}


lowestBitSet x = indexOfTheOnlyBit (lowestBitMask x)

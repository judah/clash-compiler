{-|
Copyright  :  (C) 2015-2016, University of Twente,
                  2017     , Google Inc.
                  2019     , Myrtle Software Ltd
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

= Initializing a BlockRAM with a data file #usingramfiles#

BlockRAM primitives that can be initialized with a data file. The BNF grammar
for this data file is simple:

@
FILE = LINE+
LINE = BIT+
BIT  = '0'
     | '1'
@

Consecutive @LINE@s correspond to consecutive memory addresses starting at @0@.
For example, a data file @memory.bin@ containing the 9-bit unsigned number
@7@ to @13@ looks like:

@
000000111
000001000
000001001
000001010
000001011
000001100
000001101
@

We can instantiate a BlockRAM using the content of the above file like so:

@
f
  :: Clock  tag enabled
  -> Signal tag (Unsigned 3)
  -> Signal tag (Unsigned 9)
f clk rd = 'Clash.Class.BitPack.unpack' '<$>' 'blockRamFile' clk d7 \"memory.bin\" rd (signal Nothing)
@

In the example above, we basically treat the BlockRAM as an synchronous ROM.
We can see that it works as expected:

@
__>>> import qualified Data.List as L__
__>>> L.tail $ sampleN 4 $ f systemClockGen (fromList [3..5])__
[10,11,12]
@

However, we can also interpret the same data as a tuple of a 6-bit unsigned
number, and a 3-bit signed number:

@
g
  :: Clock  tag Regular
  -> Signal tag (Unsigned 3)
  -> Signal tag (Unsigned 6,Signed 3)
g clk rd = 'Clash.Class.BitPack.unpack' '<$>' 'blockRamFile' clk d7 \"memory.bin\" rd (signal Nothing)
@

And then we would see:

@
__>>> import qualified Data.List as L__
__>>> L.tail $ sampleN 4 $ g systemClockGen (fromList [3..5])__
[(1,2),(1,3)(1,-4)]
@

-}

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

{-# LANGUAGE Unsafe #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- See: https://github.com/clash-lang/clash-compiler/commit/721fcfa9198925661cd836668705f817bddaae3c
-- as to why we need this.
{-# OPTIONS_GHC -fno-cpr-anal #-}

module Clash.Explicit.BlockRam.File
  ( -- * BlockRAM synchronized to an arbitrary clock
    blockRamFile
  , blockRamFilePow2
    -- * Internal
  , blockRamFile#
  , initMem
  )
where

import Data.Char             (digitToInt)
import Data.Maybe            (fromJust, isJust, listToMaybe)
import qualified Data.Vector as V
import GHC.Stack             (HasCallStack, withFrozenCallStack)
import GHC.TypeLits          (KnownNat)
import Numeric               (readInt)
import System.IO.Unsafe      (unsafePerformIO)

import Clash.Promoted.Nat    (SNat (..), pow2SNat)
import Clash.Sized.BitVector (BitVector)
import Clash.Signal.Internal (Clock(..), Signal (..), (.&&.))
import Clash.Signal.Bundle   (unbundle)
import Clash.Sized.Unsigned  (Unsigned)
import Clash.XException      (errorX, maybeIsX, seqX)


-- | Create a blockRAM with space for 2^@n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is 'undefined'
-- * __NB__: This function might not work for specific combinations of
-- code-generation backends and hardware targets. Please check the support table
-- below:
--
--     @
--                    | VHDL     | Verilog  | SystemVerilog |
--     ===============+==========+==========+===============+
--     Altera/Quartus | Broken   | Works    | Works         |
--     Xilinx/ISE     | Works    | Works    | Works         |
--     ASIC           | Untested | Untested | Untested      |
--     ===============+==========+==========+===============+
--     @
--
-- Additional helpful information:
--
-- * See "Clash.Prelude.BlockRam#usingrams" for more information on how to use a
-- Block RAM.
-- * Use the adapter 'readNew'' for obtaining write-before-read semantics like this: @readNew' clk (blockRamFilePow2' clk file) rd wrM@.
-- * See "Clash.Explicit.BlockRam.File#usingramfiles" for more information on how
-- to instantiate a Block RAM with the contents of a data file.
-- * See "Clash.Explicit.Fixed#creatingdatafiles" for ideas on how to create your
-- own data files.
blockRamFilePow2
  :: forall tag enabled n m
   . (KnownNat m, KnownNat n, HasCallStack)
  => Clock tag enabled
  -- ^ 'Clock' to synchronize to
  -> FilePath
  -- ^ File describing the initial content of the blockRAM
  -> Signal tag (Unsigned n)
  -- ^ Read address @r@
  -> Signal tag (Maybe (Unsigned n, BitVector m))
  -- ^ (write address @w@, value to write)
  -> Signal tag (BitVector m)
  -- ^ Value of the @blockRAM@ at address @r@ from the previous clock cycle
blockRamFilePow2 = \clk file rd wrM -> withFrozenCallStack
  (blockRamFile clk (pow2SNat (SNat @ n)) file rd wrM)
{-# INLINE blockRamFilePow2 #-}

-- | Create a blockRAM with space for @n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is 'undefined'
-- * __NB__: This function might not work for specific combinations of
-- code-generation backends and hardware targets. Please check the support table
-- below:
--
--     @
--                    | VHDL     | Verilog  | SystemVerilog |
--     ===============+==========+==========+===============+
--     Altera/Quartus | Broken   | Works    | Works         |
--     Xilinx/ISE     | Works    | Works    | Works         |
--     ASIC           | Untested | Untested | Untested      |
--     ===============+==========+==========+===============+
--     @
--
-- Additional helpful information:
--
-- * See "Clash.Explicit.BlockRam#usingrams" for more information on how to use a
-- Block RAM.
-- * Use the adapter 'readNew'' for obtaining write-before-read semantics like this: @readNew' clk (blockRamFile' clk size file) rd wrM@.
-- * See "Clash.Explicit.BlockRam.File#usingramfiles" for more information on how
-- to instantiate a Block RAM with the contents of a data file.
-- * See "Clash.Sized.Fixed#creatingdatafiles" for ideas on how to create your
-- own data files.
blockRamFile
  :: (KnownNat m, Enum addr, HasCallStack)
  => Clock tag enabled
  -- ^ 'Clock' to synchronize to
  -> SNat n
  -- ^ Size of the blockRAM
  -> FilePath
  -- ^ File describing the initial content of the blockRAM
  -> Signal tag addr
  -- ^ Read address @r@
  -> Signal tag (Maybe (addr, BitVector m))
  -- ^ (write address @w@, value to write)
  -> Signal tag (BitVector m)
  -- ^ Value of the @blockRAM@ at address @r@ from the previous
  -- clock cycle
blockRamFile = \clk sz file rd wrM ->
  let en       = isJust <$> wrM
      (wr,din) = unbundle (fromJust <$> wrM)
  in  withFrozenCallStack
      (blockRamFile# clk sz file (fromEnum <$> rd) en (fromEnum <$> wr) din)
{-# INLINE blockRamFile #-}

-- | blockRamFile primitive
blockRamFile#
  :: forall m tag enabled n
   . (KnownNat m, HasCallStack)
  => Clock tag enabled
  -- ^ 'Clock' to synchronize to
  -> SNat n
  -- ^ Size of the blockRAM
  -> FilePath
  -- ^ File describing the initial content of the blockRAM
  -> Signal tag Int
  -- ^ Read address @r@
  -> Signal tag Bool
  -- ^ Write enable
  -> Signal tag Int
  -- ^ Write address @w@
  -> Signal tag (BitVector m)
  -- ^ Value to write (at address @w@)
  -> Signal tag (BitVector m)
  -- ^ Value of the @blockRAM@ at address @r@ from the previous clock cycle
blockRamFile# clk _sz file rd wen = case clk of
  RegularClock _tag ->
    go
      ramI
      (withFrozenCallStack (errorX "blockRamFile#: intial value undefined"))
      rd
      wen
  EnabledClock _tag ena ->
    goE
      ramI
      (withFrozenCallStack (errorX "blockRamFile#: intial value undefined"))
      ena
      rd
      (ena .&&. wen)
  where
    -- no clock enable
    go
      :: V.Vector (BitVector m)
      -> BitVector m
      -> Signal tag Int
      -> Signal tag Bool
      -> Signal tag Int
      -> Signal tag (BitVector m)
      -> Signal tag (BitVector m)
    go !ram o (r :- rs) (e :- en) (w :- wr) (d :- din) =
      let ram' = upd ram e (fromEnum w) d
          o'   = ram V.! r
      in  o `seqX` o :- go ram' o' rs en wr din
      
    -- clock enable
    goE
      :: V.Vector (BitVector m)
      -> BitVector m
      -> Signal tag Bool
      -> Signal tag Int
      -> Signal tag Bool
      -> Signal tag Int
      -> Signal tag (BitVector m)
      -> Signal tag (BitVector m)
    goE !ram o (re :- res) (r :- rs) (e :- en) (w :- wr) (d :- din) =
      let ram' = upd ram e (fromEnum w) d
          o'   = if re then ram V.! r else o
      in  o `seqX` o :- goE ram' o' res rs en wr din

    upd ram we waddr d = case maybeIsX we of
      Nothing -> case maybeIsX waddr of
        Nothing -> V.map (const (seq waddr d)) ram
        Just wa -> ram V.// [(wa,d)]
      Just True -> case maybeIsX waddr of
        Nothing -> V.map (const (seq waddr d)) ram
        Just wa -> ram V.// [(wa,d)]
      _ -> ram

    content = unsafePerformIO (initMem file)

    ramI :: V.Vector (BitVector m)
    ramI = V.fromList content
{-# NOINLINE blockRamFile# #-}

-- | __NB:__ Not synthesizable
initMem :: KnownNat n => FilePath -> IO [BitVector n]
initMem = fmap (map parseBV . lines) . readFile
  where
    parseBV s = case parseBV' s of
                  Just i  -> fromInteger i
                  Nothing -> error ("Failed to parse: " ++ s)
    parseBV' = fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt
{-# NOINLINE initMem #-}

{-|
Copyright  :  (C) 2013-2016, University of Twente,
                  2017     , Google Inc.
                  2019     , Myrtle Software Ltd
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE Unsafe #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Explicit.Testbench
  ( -- * Testbench functions for circuits
    assert
  , stimuliGenerator
  , outputVerifier
  , outputVerifierBitVector
  )
where

import Control.Exception     (catch, evaluate)
import Debug.Trace           (trace)
import GHC.TypeLits          (KnownNat)
import Prelude               hiding ((!!), length)
import System.IO.Unsafe      (unsafeDupablePerformIO)

import Clash.Annotations.Primitive (hasBlackBox)
import Clash.Explicit.Signal
  (Clock, Reset, Signal, toEnable, fromList, register, unbundle)
import Clash.Signal          (mux, KnownDomain, Reset(..))
import Clash.Sized.Index     (Index)
import Clash.Sized.Internal.BitVector
  (BitVector, isLike)
import Clash.Sized.Vector    (Vec, (!!), length)
import Clash.XException      (ShowX (..), XException)

{- $setup
>>> :set -XTemplateHaskell -XDataKinds
>>> import Clash.Explicit.Prelude
>>> let testInput clk rst = stimuliGenerator clk rst $(listToVecTH [(1::Int),3..21])
>>> let expectedOutput clk rst = outputVerifier clk rst $(listToVecTH ([70,99,2,3,4,5,7,8,9,10]::[Int]))
-}

-- | Compares the first two 'Signal's for equality and logs a warning when they
-- are not equal. The second 'Signal' is considered the expected value. This
-- function simply returns the third 'Signal'' unaltered as its result. This
-- function is used by 'outputVerifier'.
--
--
-- __NB__: This function /can/ be used in synthesizable designs.
assert
  :: (KnownDomain tag dom, Eq a, ShowX a)
  => Clock tag
  -> Reset tag
  -> String
  -- ^ Additional message
  -> Signal tag a
  -- ^ Checked value
  -> Signal tag a
  -- ^ Expected value
  -> Signal tag b
  -- ^ Return value
  -> Signal tag b
assert clk (Reset _ _) msg checked expected returned =
  (\c e cnt r ->
      if eqX c e
         then r
         else trace (concat [ "\nOn cycle ", show cnt, " of ", show clk, ", "
                            , msg, " encountered an unexpected value:\n"
                            , "  Expected: ", showX e, "\n"
                            , "  Actual:   ", showX c
                            ]) r)
  <$> checked <*> expected <*> fromList [(0::Integer)..] <*> returned
  where
    eqX a b = unsafeDupablePerformIO (catch (evaluate (a == b))
                                            (\(_ :: XException) -> return False))
{-# NOINLINE assert #-}
{-# ANN assert hasBlackBox #-}

-- | The same as 'assert', but can handle don't care bits in it's expected value.
assertBitVector
  :: (KnownDomain tag dom, KnownNat n)
  => Clock tag
  -> Reset tag
  -> String
  -- ^ Additional message
  -> Signal tag (BitVector n)
  -- ^ Checked value
  -> Signal tag (BitVector n)
  -- ^ Expected value
  -> Signal tag b
  -- ^ Return value
  -> Signal tag b
assertBitVector clk _rst msg checked expected returned =
  (\c e cnt r ->
      if eqX c e
         then r
         else trace (concat [ "\ncycle(" ++ show clk ++ "): "
                            , show cnt
                            , ", "
                            , msg
                            , "\nexpected value: "
                            , showX e
                            , ", not equal to actual value: "
                            , showX c
                            ]) r)
  <$> checked <*> expected <*> fromList [(0::Integer)..] <*> returned
  where
    eqX a b = unsafeDupablePerformIO (catch (evaluate (a `isLike` b))
                                            (\(_ :: XException) -> return False))
{-# NOINLINE assertBitVector #-}
{-# ANN assertBitVector hasBlackBox #-}



-- | To be used as one of the functions to create the \"magical\" 'testInput'
-- value, which the Clash compiler looks for to create the stimulus generator
-- for the generated VHDL testbench.
--
-- Example:
--
-- @
-- testInput
--   :: KnownDomain tag dom
--   => Clock tag
--   -> Reset tag
--   -> 'Signal' tag Int
-- testInput clk rst = 'stimuliGenerator' clk rst $('Clash.Sized.Vector.listToVecTH' [(1::Int),3..21])
-- @
--
-- >>> sampleN 14 (testInput systemClockGen resetGen)
-- [1,1,3,5,7,9,11,13,15,17,19,21,21,21]
stimuliGenerator
  :: forall l tag dom  a
   . ( KnownNat l
     , KnownDomain tag dom )
  => Clock tag
  -- ^ Clock to which to synchronize the output signal
  -> Reset tag
  -> Vec l a
  -- ^ Samples to generate
  -> Signal tag a
  -- ^ Signal of given samples
stimuliGenerator clk rst samples =
    let (r,o) = unbundle (genT <$> register clk rst (toEnable (pure True)) 0 r)
    in  o
  where
    genT :: Index l -> (Index l,a)
    genT s = (s',samples !! s)
      where
        maxI = toEnum (length samples - 1)

        s' = if s < maxI
                then s + 1
                else s
{-# INLINABLE stimuliGenerator #-}

-- | To be used as one of the functions to generate the \"magical\" 'expectedOutput'
-- function, which the Clash compiler looks for to create the signal verifier
-- for the generated VHDL testbench.
--
-- Example:
--
-- @
-- expectedOutput
--   :: Clock tag -> Reset tag
--   -> 'Signal' tag Int -> 'Signal' tag Bool
-- expectedOutput clk rst = 'outputVerifier' clk rst $('Clash.Sized.Vector.listToVecTH' ([70,99,2,3,4,5,7,8,9,10]::[Int]))
-- @
--
-- >>> import qualified Data.List as List
-- >>> sampleN 12 (expectedOutput systemClockGen resetGen (fromList (0:[0..10] List.++ [10,10,10])))
-- <BLANKLINE>
-- On cycle 0 of <Clock: System>, outputVerifier encountered an unexpected value:
--   Expected: 70
--   Actual:   0
-- [False
-- On cycle 1 of <Clock: System>, outputVerifier encountered an unexpected value:
--   Expected: 70
--   Actual:   0
-- ,False
-- On cycle 2 of <Clock: System>, outputVerifier encountered an unexpected value:
--   Expected: 99
--   Actual:   1
-- ,False,False,False,False,False
-- On cycle 7 of <Clock: System>, outputVerifier encountered an unexpected value:
--   Expected: 7
--   Actual:   6
-- ,False
-- On cycle 8 of <Clock: System>, outputVerifier encountered an unexpected value:
--   Expected: 8
--   Actual:   7
-- ,False
-- On cycle 9 of <Clock: System>, outputVerifier encountered an unexpected value:
--   Expected: 9
--   Actual:   8
-- ,False
-- On cycle 10 of <Clock: System>, outputVerifier encountered an unexpected value:
--   Expected: 10
--   Actual:   9
-- ,False,True]
--
-- If your working with 'BitVector's containing don't care bit you should use 'outputVerifierBitVector'.
outputVerifier
  :: forall l tag dom  a
   . ( KnownNat l
     , KnownDomain tag dom
     , Eq a
     , ShowX a )
  => Clock tag
  -- ^ Clock to which the input signal is synchronized to
  -> Reset tag
  -> Vec l a
  -- ^ Samples to compare with
  -> Signal tag a
  -- ^ Signal to verify
  -> Signal tag Bool
  -- ^ Indicator that all samples are verified
outputVerifier clk rst samples i =
    let en    = toEnable (pure True)
        (s,o) = unbundle (genT <$> register clk rst en 0 s)
        (e,f) = unbundle o
        f'    = register clk rst en False f
        -- Only assert while not finished
    in  mux f' f' $ assert clk rst "outputVerifier" i e f'
  where
    genT :: Index l -> (Index l,(a,Bool))
    genT s = (s',(samples !! s,finished))
      where
        maxI = toEnum (length samples - 1)

        s' = if s < maxI
                then s + 1
                else s

        finished = s == maxI
{-# INLINABLE outputVerifier #-}

-- | Same as 'outputVerifier', but can handle don't care bits in it's expected
-- values.
outputVerifierBitVector
  :: forall l n tag dom
   . ( KnownNat l
     , KnownNat n
     , KnownDomain tag dom
     )
  => Clock tag
  -- ^ Clock to which the input signal is synchronized to
  -> Reset tag
  -> Vec l (BitVector n)
  -- ^ Samples to compare with
  -> Signal tag (BitVector n)
  -- ^ Signal to verify
  -> Signal tag Bool
  -- ^ Indicator that all samples are verified
outputVerifierBitVector clk rst samples i =
    let en    = toEnable (pure True)
        (s,o) = unbundle (genT <$> register clk rst en 0 s)
        (e,f) = unbundle o
        f'    = register clk rst en False f
        -- Only assert while not finished
    in  mux f' f' $ assertBitVector clk rst "outputVerifierBitVector" i e f'
  where
    genT :: Index l -> (Index l,(BitVector n,Bool))
    genT s = (s',(samples !! s,finished))
      where
        maxI = toEnum (length samples - 1)

        s' = if s < maxI
                then s + 1
                else s

        finished = s == maxI
{-# INLINABLE outputVerifierBitVector #-}

-- Make sure a enabled clock doesn't influence the values around it in a product type
--
-- The Netlist code used to think a enabled clock was 1 bit wide.
-- This caused the generated Verilog to shift the other bit of the clock into res1.
module EnabledClockWidth where

import Clash.Explicit.Prelude

topEntity
  :: Clock System Regular
  -> Signal System (Unsigned 8)
  -> (Signal System (Unsigned 8), Clock System Enabled, Signal System (Unsigned 8))
topEntity clk x = clkGater clk x
{-# NOINLINE topEntity #-}

clkGater :: Clock tag Regular -> Signal tag (Unsigned 8) -> (Signal tag (Unsigned 8), Clock tag Enabled, Signal tag (Unsigned 8))
clkGater clk x = (succ <$> x, clkOut, x)
  where
    clkOut = toEnabledClock clk ((== 3) <$> x)
{-# NOINLINE clkGater #-}

testBench :: Signal System Bool
testBench = done
  where
    testValues     = $(listToVecTH ([1..10]::[Unsigned 8]))
    testInput      = stimuliGenerator clk rst testValues
    expectedOutput = outputVerifier clk rst $ zip (succ <$> testValues) testValues
    (res1,_,res2)  = topEntity clk testInput
    done           = expectedOutput $ bundle (res1,res2)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen

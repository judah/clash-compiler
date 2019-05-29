module RegisterAS where

import Clash.Explicit.Prelude

testInput :: Vec 7 (Signed 8)
testInput = 1 :> 2 :> 3 :> 4 :> 5 :> 6 :> 7 :> Nil

resetInput
  :: KnownDomain tag dom
  => Clock tag
  -> Reset tag
  -> Enable tag
  -> Signal tag Bool
resetInput clk reset en
  = register clk reset en True
  $ register clk reset en False
  $ register clk reset en False
  $ register clk reset en True
  $ register clk reset en True
  $ pure False

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System (Signed 8)
topEntity clk rst en = head <$> r
  where
    r = register clk rst en testInput (flip rotateLeftS d1 <$> r)

topEntityAS clk rst en = topEntity clk arst en
  where
    arst = unsafeFromHighPolarity (resetInput clk rst en)
{-# NOINLINE topEntityAS #-}

testBench :: Signal System Bool
testBench = done
  where
    expectedOutput = outputVerifier clk rst (1 :> 1 :> 2 :> 1 :> 1 :> 1 :> 2 :> 3 :> Nil)
    done           = expectedOutput (topEntityAS clk rst enableGen)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen

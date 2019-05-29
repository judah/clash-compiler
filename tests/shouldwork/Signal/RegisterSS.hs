module RegisterSS where

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
  :: Clock XilinxSystem
  -> Reset XilinxSystem
  -> Enable XilinxSystem
  -> Signal XilinxSystem (Signed 8)
topEntity clk rst en = head <$> r
  where
    r = register clk rst en testInput (flip rotateLeftS d1 <$> r)

topEntitySS clk rst en = topEntity clk srst en
  where
    srst = unsafeFromHighPolarity (resetInput clk rst en)
{-# NOINLINE topEntitySS #-}

testBench :: Signal XilinxSystem Bool
testBench = done
  where
    expectedOutput = outputVerifier clk rst (1 :> 1 :> 2 :> 3 :> 1 :> 1 :> 2 :> 3 :> Nil)
    done           = expectedOutput (topEntitySS clk rst enableGen)
    clk            = tbClockGen (not <$> done)
    rst            = resetGen

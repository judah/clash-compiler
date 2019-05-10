module DelayN where

import Clash.Prelude
import qualified Clash.Signal.Delayed.Bundle as D
import Clash.Explicit.Testbench

delayer
  :: HiddenClockReset tag enabled polarity dom
  => DSignal tag 0 Int
  -> DSignal tag 2 Int
delayer = delayN d2 0

topEntity
  :: Clock System Regular
  -> Reset System polarity
  -> Signal System Int
  -> Signal System Int
topEntity = exposeClockReset (toSignal . delayer . fromSignal)

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst $ 1 :> 2 :> 3 :> 10 :> Nil
    expectedOutput = outputVerifier clk rst (0:>1:>1:>2:>3:>10:>Nil)
    done           = expectedOutput (topEntity clk rst testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen


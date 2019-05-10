module MAC where

import Clash.Prelude
import qualified Clash.Explicit.Prelude as E

topEntity
  :: Clock System Regular
  -> Reset System ActiveHigh
  -> Signal System (Signed 9)
  -> Signal System (Signed 9)
topEntity clk rst x =
  E.register clk rst 0 x


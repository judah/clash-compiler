module AlwaysHigh where

import Clash.Prelude

topEntity
  :: Clock System Regular
  -> Reset System polarity
  -> Signal System Bit
topEntity = exposeClockReset (register high (pure high))

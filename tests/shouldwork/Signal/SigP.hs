module SigP where

import Clash.Prelude

topEntity
  :: Clock System Regular
  -> Reset System polarity
  -> Signal System (Bool, Bool)
  -> (Signal System Bool, Signal System Bool)
topEntity = exposeClockReset (unbundle . register (False,False))

module HOClock where

import Clash.Prelude

topEntity
  :: Clock System Regular
  -> Reset System polarity
  -> Vec 8 (Signal System (Int,Int)) -> Vec 8 (Signal System (Int,Int))
topEntity = exposeClockReset (map (register (0,0)))

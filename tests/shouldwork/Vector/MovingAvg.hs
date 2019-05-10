module MovingAvg where

import Clash.Prelude

windowN
  :: HiddenClockReset tag enabled polarity dom
  => Default a
  => KnownNat n
  => Undefined a
  => SNat (n+1)
  -> Signal tag a
  -> Vec (n + 1) (Signal tag a)
windowN size = window

movingAvarageNaive size signal =  fold (+) <$> bundle (windowN size signal)

topEntity
  :: Clock System Regular
  -> Reset System polarity
  -> Signal System (Signed 9)
  -> Signal System (Signed 9)
topEntity = exposeClockReset (movingAvarageNaive d5)

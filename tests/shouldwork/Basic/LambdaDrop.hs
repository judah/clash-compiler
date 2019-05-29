module LambdaDrop where

import Clash.Prelude

topEntity :: Signal System (BitVector 2)
topEntity = (++#) <$> (pack <$> outport1) <*> (pack <$> outport2)
  where
    (outport1, outResp1) = gpio (decodeReq 1 req)
    (outport2, outResp2) = gpio (decodeReq 2 req)
    ramResp = ram (decodeReq 0 req)

    req = core $ (<|>) <$> ramResp <*> ((<|>) <$> outResp1 <*> outResp2)

core :: Signal tag (Maybe Bit) -> Signal tag Bit
core = fmap (maybe low id)
{-# NOINLINE core #-}

ram :: Signal tag Bit -> Signal tag (Maybe Bit)
ram = fmap pure
{-# NOINLINE ram #-}

decodeReq :: Integer -> Signal tag Bit -> Signal tag Bit
decodeReq 0 = fmap (const low)
decodeReq 1 = id
decodeReq _ = fmap complement
{-# NOINLINE decodeReq #-}

gpio :: Signal tag Bit -> (Signal tag Bit,Signal tag (Maybe Bit))
gpio i = (i,pure <$> i)
{-# NOINLINE gpio #-}

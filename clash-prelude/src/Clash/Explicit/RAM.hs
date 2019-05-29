{-|
Copyright  :  (C) 2015-2016, University of Twente,
                  2017     , Google Inc.
                  2019     , Myrtle Software Ltd
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

RAM primitives with a combinational read port.
-}

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# LANGUAGE Trustworthy #-}

-- See: https://github.com/clash-lang/clash-compiler/commit/721fcfa9198925661cd836668705f817bddaae3c
-- as to why we need this.
{-# OPTIONS_GHC -fno-cpr-anal #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Explicit.RAM
  ( -- * RAM synchronized to an arbitrary clock
    asyncRam
  , asyncRamPow2
    -- * Internal
  , asyncRam#
  )
where

import Data.Maybe            (fromJust, isJust)
import GHC.Stack             (HasCallStack, withFrozenCallStack)
import GHC.TypeLits          (KnownNat)
import qualified Data.Vector as V

import Clash.Explicit.Signal
  (unbundle, unsafeSynchronizer, KnownDomain, enable)
import Clash.Promoted.Nat    (SNat (..), snatToNum, pow2SNat)
import Clash.Signal.Internal (Clock (..), Signal (..), Enable, fromEnable)
import Clash.Sized.Unsigned  (Unsigned)
import Clash.XException      (errorX, maybeIsX)

-- | Create a RAM with space for 2^@n@ elements
--
-- * __NB__: Initial content of the RAM is 'undefined'
--
-- Additional helpful information:
--
-- * See "Clash.Prelude.BlockRam#usingrams" for more information on how to use a
-- RAM.
asyncRamPow2
  :: forall wtag rtag wconf rconf n a
   . ( KnownNat n
     , HasCallStack
     , KnownDomain wtag wconf
     , KnownDomain rtag rconf
     )
  => Clock wtag
  -- ^ 'Clock' to which to synchronize the write port of the RAM
  -> Clock rtag
  -- ^ 'Clock' to which the read address signal, @r@, is synchronized
  -> Enable wtag
  -- ^ Global enable
  -> Signal rtag (Unsigned n)
  -- ^ Read address @r@
  -> Signal wtag (Maybe (Unsigned n, a))
  -- ^ (write address @w@, value to write)
  -> Signal rtag a
  -- ^ Value of the @RAM@ at address @r@
asyncRamPow2 = \wclk rclk en rd wrM -> withFrozenCallStack
  (asyncRam wclk rclk en (pow2SNat (SNat @n)) rd wrM)
{-# INLINE asyncRamPow2 #-}


-- | Create a RAM with space for @n@ elements
--
-- * __NB__: Initial content of the RAM is 'undefined'
--
-- Additional helpful information:
--
-- * See "Clash.Explicit.BlockRam#usingrams" for more information on how to use a
-- RAM.
asyncRam
  :: ( Enum addr
     , HasCallStack
     , KnownDomain wtag wconf
     , KnownDomain rtag rconf
     )
  => Clock wtag
   -- ^ 'Clock' to which to synchronize the write port of the RAM
  -> Clock rtag
   -- ^ 'Clock' to which the read address signal, @r@, is synchronized
  -> Enable wtag
  -- ^ Global enable
  -> SNat n
  -- ^ Size @n@ of the RAM
  -> Signal rtag addr
  -- ^ Read address @r@
  -> Signal wtag (Maybe (addr, a))
  -- ^ (write address @w@, value to write)
  -> Signal rtag a
   -- ^ Value of the @RAM@ at address @r@
asyncRam = \wclk rclk gen sz rd wrM ->
  let en       = isJust <$> wrM
      (wr,din) = unbundle (fromJust <$> wrM)
  in  withFrozenCallStack
      (asyncRam# wclk rclk gen sz (fromEnum <$> rd) en (fromEnum <$> wr) din)
{-# INLINE asyncRam #-}

-- | RAM primitive
asyncRam#
  :: ( HasCallStack
     , KnownDomain wtag wconf
     , KnownDomain rtag rconf )
  => Clock wtag
  -- ^ 'Clock' to which to synchronize the write port of the RAM
  -> Clock rtag
  -- ^ 'Clock' to which the read address signal, @r@, is synchronized
  -> Enable wtag
  -- ^ Global enable
  -> SNat n
  -- ^ Size @n@ of the RAM
  -> Signal rtag Int
  -- ^ Read address @r@
  -> Signal wtag Bool
  -- ^ Write enable
  -> Signal wtag Int
  -- ^ Write address @w@
  -> Signal wtag a
  -- ^ Value to write (at address @w@)
  -> Signal rtag a
  -- ^ Value of the @RAM@ at address @r@
asyncRam# wclk rclk en sz rd we wr din =
    unsafeSynchronizer wclk rclk dout
  where
    rd'  = unsafeSynchronizer rclk wclk rd
    ramI = V.replicate
              (snatToNum sz)
              (withFrozenCallStack (errorX "asyncRam#: initial value undefined"))
    en' = fromEnable (enable en we)
    dout = go ramI rd' en' wr din

    go :: V.Vector a -> Signal wtag Int -> Signal wtag Bool
       -> Signal wtag Int -> Signal wtag a -> Signal wtag a
    go !ram (r :- rs) (e :- es) (w :- ws) (d :- ds) =
      let ram' = upd ram e (fromEnum w) d
          o    = ram V.! r
      in  o :- go ram' rs es ws ds

    upd ram we' waddr d = case maybeIsX we' of
      Nothing -> case maybeIsX waddr of
        Nothing -> V.map (const (seq waddr d)) ram
        Just wa -> ram V.// [(wa,d)]
      Just True -> case maybeIsX waddr of
        Nothing -> V.map (const (seq waddr d)) ram
        Just wa -> ram V.// [(wa,d)]
      _ -> ram
{-# NOINLINE asyncRam# #-}

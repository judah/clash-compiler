{-|
Copyright  :  (C) 2013-2016, University of Twente,
                  2017     , Google Inc.
                  2019     , Myrtle Software Ltd
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

__This is the <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/safe_haskell.html Safe> API only of "Clash.Explicit.Prelude"__

This module defines the explicitly clocked counterparts of the functions
defined in "Clash.Prelude".
-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE Safe #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Explicit.Prelude.Safe
  ( -- * Creating synchronous sequential circuits
    mealy
  , mealyB
  , moore
  , mooreB
  , registerB
    -- * Synchronizer circuits for safe clock domain crossing
  , dualFlipFlopSynchronizer
  , asyncFIFOSynchronizer
    -- * ROMs
  , asyncRom
  , asyncRomPow2
  , rom
  , romPow2
    -- * RAM primitives with a combinational read port
  , asyncRam
  , asyncRamPow2
    -- * BlockRAM primitives
  , blockRam
  , blockRamPow2
    -- ** BlockRAM read/write conflict resolution
  , readNew
    -- * Utility functions
  , isRising
  , isFalling
  , riseEvery
  , oscillate
    -- * Exported modules
    -- ** Synchronous signals
  , module Clash.Explicit.Signal
  , module Clash.Explicit.Signal.Delayed
    -- ** DataFlow interface
  , module Clash.Prelude.DataFlow
    -- ** Datatypes
    -- *** Bit vectors
  , module Clash.Sized.BitVector
  , module Clash.Prelude.BitIndex
  , module Clash.Prelude.BitReduction
    -- *** Arbitrary-width numbers
  , module Clash.Sized.Signed
  , module Clash.Sized.Unsigned
  , module Clash.Sized.Index
    -- *** Fixed point numbers
  , module Clash.Sized.Fixed
    -- *** Fixed size vectors
  , module Clash.Sized.Vector
    -- *** Perfect depth trees
  , module Clash.Sized.RTree
    -- ** Annotations
  , module Clash.Annotations.TopEntity
    -- ** Generics type-classes
  , Generic
  , Generic1
    -- ** Type-level natural numbers
  , module GHC.TypeLits
  , module GHC.TypeLits.Extra
  , module Clash.Promoted.Nat
  , module Clash.Promoted.Nat.Literals
  , module Clash.Promoted.Nat.TH
    -- ** Type-level strings
  , module Clash.Promoted.Symbol
    -- ** Type classes
    -- *** Clash
  , module Clash.Class.BitPack
  , module Clash.Class.Num
  , module Clash.Class.Resize
    -- *** Other
  , module Control.Applicative
  , module Data.Bits
      -- ** Exceptions
  , module Clash.XException
  , undefined
    -- ** Named types
  , module Clash.NamedTypes
    -- ** Haskell Prelude
    -- $hiding
  , module Prelude
  )
where

import Control.Applicative
import Data.Bits
import GHC.Generics (Generic, Generic1)
import GHC.Stack
import GHC.TypeLits
import GHC.TypeLits.Extra
import Prelude hiding
  ((++), (!!), concat, concatMap, drop, foldl, foldl1, foldr, foldr1, head, init,
   iterate, last, length, map, repeat, replicate, reverse, scanl, scanr, splitAt,
   tail, take, unzip, unzip3, zip, zip3, zipWith, zipWith3, undefined)

import Clash.Annotations.TopEntity
import Clash.Class.BitPack
import Clash.Class.Num
import Clash.Class.Resize
import Clash.NamedTypes

import Clash.Explicit.BlockRam
import Clash.Explicit.Mealy
import Clash.Explicit.Moore
import Clash.Explicit.RAM
import Clash.Explicit.ROM
import Clash.Explicit.Signal
import Clash.Explicit.Signal.Delayed
import Clash.Explicit.Synchronizer
  (dualFlipFlopSynchronizer, asyncFIFOSynchronizer)
import Clash.Prelude.BitIndex
import Clash.Prelude.BitReduction
import Clash.Prelude.DataFlow
import Clash.Prelude.ROM             (asyncRom, asyncRomPow2)
import Clash.Promoted.Nat
import Clash.Promoted.Nat.TH
import Clash.Promoted.Nat.Literals
import Clash.Promoted.Symbol
import Clash.Sized.BitVector
import Clash.Sized.Fixed
import Clash.Sized.Index
import Clash.Sized.RTree
import Clash.Sized.Signed
import Clash.Sized.Unsigned
import Clash.Sized.Vector
import Clash.XException

{- $setup
>>> :set -XDataKinds
>>> import Clash.Explicit.Prelude
>>> let rP clk rst en = registerB clk rst en (8::Int,8::Int)
-}


-- | Create a 'register' function for product-type like signals (e.g.
-- @('Signal' a, 'Signal' b)@)
--
-- @
-- rP :: Clock tag -> Reset tag -> Enable tag
--    -> ('Signal' tag Int, 'Signal' tag Int)
--    -> ('Signal' tag Int, 'Signal' tag Int)
-- rP clk rst en = 'registerB' clk rst en (8,8)
-- @
--
-- >>> simulateB (rP systemClockGen systemResetGen enableGen) [(1,1),(1,1),(2,2),(3,3)] :: [(Int,Int)]
-- [(8,8),(8,8),(1,1),(2,2),(3,3)...
-- ...
registerB
  :: ( KnownDomain tag dom
     , Undefined a
     , Bundle a )
  => Clock tag
  -> Reset tag
  -> Enable tag
  -> a
  -> Unbundled tag a
  -> Unbundled tag a
registerB clk rst en i =
  unbundle Prelude.. register clk rst en i Prelude.. bundle
{-# INLINE registerB #-}

-- | Give a pulse when the 'Signal' goes from 'minBound' to 'maxBound'
isRising
  :: ( KnownDomain tag dom
     , Undefined a
     , Bounded a
     , Eq a )
  => Clock tag
  -> Reset tag
  -> Enable tag
  -> a -- ^ Starting value
  -> Signal tag a
  -> Signal tag Bool
isRising clk rst en is s = liftA2 edgeDetect prev s
  where
    prev = register clk rst en is s
    edgeDetect old new = old == minBound && new == maxBound
{-# INLINABLE isRising #-}

-- | Give a pulse when the 'Signal' goes from 'maxBound' to 'minBound'
isFalling
  :: ( KnownDomain tag dom
     , Undefined a
     , Bounded a
     , Eq a )
  => Clock tag
  -> Reset tag
  -> Enable tag
  -> a -- ^ Starting value
  -> Signal tag a
  -> Signal tag Bool
isFalling clk rst en is s = liftA2 edgeDetect prev s
  where
    prev = register clk rst en is s
    edgeDetect old new = old == maxBound && new == minBound
{-# INLINABLE isFalling #-}

-- | Give a pulse every @n@ clock cycles. This is a useful helper function when
-- combined with functions like @'Clash.Explicit.Signal.regEn'@ or
-- @'Clash.Explicit.Signal.mux'@, in order to delay a register by a known amount.
riseEvery
  :: forall tag dom n
   . KnownDomain tag dom
  => Clock tag
  -> Reset tag
  -> Enable tag
  -> SNat n
  -> Signal tag Bool
riseEvery clk rst en SNat = moore clk rst en transfer output 0 (pure ())
  where
    output :: Index n -> Bool
    output = (== maxBound)

    transfer :: Index n -> () -> Index n
    transfer s _ = if (s == maxBound) then 0 else s+1
{-# INLINEABLE riseEvery #-}

-- | Oscillate a @'Bool'@ for a given number of cycles, given the starting state.
oscillate
  :: forall tag dom n
   . KnownDomain tag dom
  => Clock tag
  -> Reset tag
  -> Enable tag
  -> Bool
  -> SNat n
  -> Signal tag Bool
oscillate clk rst en begin SNat =
  moore clk rst en transfer snd (0, begin) (pure ())
 where
  transfer :: (Index n, Bool) -> () -> (Index n, Bool)
  transfer (s, i) _ =
    if s == maxBound
      then (0,   not i) -- reset state and oscillate output
      else (s+1, i)     -- hold current output
{-# INLINEABLE oscillate #-}

undefined :: HasCallStack => a
undefined = errorX "undefined"

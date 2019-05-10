{-|
  Copyright  :  (C) 2013-2016, University of Twente,
                    2017     , Google Inc.
                    2019     , Myrtle Software Ltd
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Whereas the output of a Mealy machine depends on /current transition/, the
  output of a Moore machine depends on the /previous state/.

  Moore machines are strictly less expressive, but may impose laxer timing
  requirements.
-}

{-# LANGUAGE Safe #-}

module Clash.Explicit.Moore
  ( -- * Moore machines with explicit clock and reset ports
    moore
  , mooreB
  , medvedev
  , medvedevB
  )
where

import           Clash.Explicit.Signal
  (KnownDomain, Bundle (..), Clock, Reset, Signal, register)
import           Clash.XException                 (Undefined)

{- $setup
>>> :set -XDataKinds -XTypeApplications
>>> import Clash.Explicit.Prelude
>>> let macT s (x,y) = x * y + s
>>> let mac clk rst = moore clk rst macT id 0
-}

-- | Create a synchronous function from a combinational function describing
-- a moore machine
--
-- @
-- macT
--   :: Int        -- Current state
--   -> (Int,Int)  -- Input
--   -> (Int,Int)  -- Updated state
-- macT s (x,y) = x * y + s
--
-- mac
--   :: 'Clock' mac Regular
--   -> 'Reset' mac Asynchronous
--   -> 'Signal' mac (Int, Int)
--   -> 'Signal' mac Int
-- mac clk rst = 'moore' clk rst macT id 0
-- @
--
-- >>> simulate (mac systemClockGen systemResetGen) [(0,0),(1,1),(2,2),(3,3),(4,4)]
-- [0,0,1,5,14...
-- ...
--
-- Synchronous sequential functions can be composed just like their
-- combinational counterpart:
--
-- @
-- dualMac
--   :: Clock tag enabled
--   -> Reset tag polarity
--   -> ('Signal' tag Int, 'Signal' tag Int)
--   -> ('Signal' tag Int, 'Signal' tag Int)
--   -> 'Signal' tag Int
-- dualMac clk rst (a,b) (x,y) = s1 + s2
--   where
--     s1 = 'moore' clk rst mac id 0 ('bundle' (a,x))
--     s2 = 'moore' clk rst mac id 0 ('bundle' (b,y))
-- @
moore
  :: ( KnownDomain tag dom
     , Undefined s )
  => Clock tag enabled
  -- ^ 'Clock' to synchronize to
  -> Reset tag polarity
  -> (s -> i -> s)
  -- ^ Transfer function in moore machine form: @state -> input -> newstate@
  -> (s -> o)
  -- ^ Output function in moore machine form: @state -> output@
  -> s
  -- ^ Initial state
  -> (Signal tag i -> Signal tag o)
  -- ^ Synchronous sequential function with input and output matching that
  -- of the moore machine
moore clk rst ft fo iS =
  \i -> let s' = ft <$> s <*> i
            s  = register clk rst iS s'
        in fo <$> s
{-# INLINABLE moore #-}

-- | Create a synchronous function from a combinational function describing
-- a moore machine without any output logic
medvedev
  :: ( KnownDomain tag dom
     , Undefined s )
  => Clock tag enabled
  -> Reset tag polarity
  -> (s -> i -> s)
  -> s
  -> (Signal tag i -> Signal tag s)
medvedev clk rst tr st = moore clk rst tr id st
{-# INLINE medvedev #-}

-- | A version of 'moore' that does automatic 'Bundle'ing
--
-- Given a functions @t@ and @o@ of types:
--
-- @
-- __t__ :: Int -> (Bool, Int) -> Int
-- __o__ :: Int -> (Int, Bool)
-- @
--
-- When we want to make compositions of @t@ and @o@ in @g@ using 'moore'', we have to
-- write:
--
-- @
-- g clk rst a b c = (b1,b2,i2)
--   where
--     (i1,b1) = 'unbundle' (moore clk rst t o 0 ('bundle' (a,b)))
--     (i2,b2) = 'unbundle' (moore clk rst t o 3 ('bundle' (i1,c)))
-- @
--
-- Using 'mooreB'' however we can write:
--
-- @
-- g clk rst a b c = (b1,b2,i2)
--   where
--     (i1,b1) = 'mooreB' clk rst t o 0 (a,b)
--     (i2,b2) = 'mooreB' clk rst t o 3 (i1,c)
-- @
mooreB
  :: ( KnownDomain tag dom
     , Undefined s
     , Bundle i
     , Bundle o )
  => Clock tag enabled
  -> Reset tag polarity
  -> (s -> i -> s)
  -- ^ Transfer function in moore machine form:
  -- @state -> input -> newstate@
  -> (s -> o)
  -- ^ Output function in moore machine form:
  -- @state -> output@
  -> s
  -- ^ Initial state
  -> (Unbundled tag i -> Unbundled tag o)
  -- ^ Synchronous sequential function with input and output matching that
  -- of the moore machine
mooreB clk rst ft fo iS i = unbundle (moore clk rst ft fo iS (bundle i))
{-# INLINE mooreB #-}

-- | A version of 'medvedev' that does automatic 'Bundle'ing
medvedevB
  :: ( KnownDomain tag dom
     , Undefined s
     , Bundle i
     , Bundle s )
  => Clock tag enabled
  -> Reset tag polarity
  -> (s -> i -> s)
  -> s
  -> (Unbundled tag i -> Unbundled tag s)
medvedevB clk rst tr st = mooreB clk rst tr id st
{-# INLINE medvedevB #-}

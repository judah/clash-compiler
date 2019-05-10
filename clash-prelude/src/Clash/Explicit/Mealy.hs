{-|
  Copyright  :  (C) 2013-2016, University of Twente,
                    2017     , Google Inc.
                    2019     , Myrtle Software Ltd
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Whereas the output of a Moore machine depends on the /previous state/, the
  output of a Mealy machine depends on /current transition/.

  Mealy machines are strictly more expressive, but may impose stricter timing
  requirements.
-}

{-# LANGUAGE Safe #-}

module Clash.Explicit.Mealy
  ( -- * Mealy machines with explicit clock and reset ports
    mealy
  , mealyB
  )
where

import           Clash.Explicit.Signal
  (KnownDomain, Bundle (..), Clock, Reset, Signal, register)
import           Clash.XException      (Undefined)

{- $setup
>>> :set -XDataKinds -XTypeApplications
>>> import Clash.Explicit.Prelude
>>> import qualified Data.List as L
>>> :{
let macT s (x,y) = (s',s)
      where
        s' = x * y + s
:}

>>> let mac clk rst = mealy clk rst macT 0
-}

-- | Create a synchronous function from a combinational function describing
-- a mealy machine
--
-- @
-- import qualified Data.List as L
--
-- macT
--   :: Int        -- Current state
--   -> (Int,Int)  -- Input
--   -> (Int,Int)  -- (Updated state, output)
-- macT s (x,y) = (s',s)
--   where
--     s' = x * y + s
--
-- mac
--   :: 'Clock' domain Regular
--   -> 'Reset' domain Asynchronous
--   -> 'Signal' tag (Int, Int)
--   -> 'Signal' tag Int
-- mac clk rst = 'mealy' clk rst macT 0
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
--   :: 'Clock' domain enabled -> 'Reset' domain synchronous
--   -> ('Signal' tag Int, 'Signal' tag Int)
--   -> ('Signal' tag Int, 'Signal' tag Int)
--   -> 'Signal' tag Int
-- dualMac clk rst (a,b) (x,y) = s1 + s2
--   where
--     s1 = 'mealy' clk rst mac 0 ('bundle' (a,x))
--     s2 = 'mealy' clk rst mac 0 ('bundle' (b,y))
-- @
mealy
  :: ( KnownDomain tag dom
     , Undefined s )
  => Clock tag enabled
  -- ^ 'Clock' to synchronize to
  -> Reset tag polarity
  -> (s -> i -> (s,o))
  -- ^ Transfer function in mealy machine form: @state -> input -> (newstate,output)@
  -> s
  -- ^ Initial state
  -> (Signal tag i -> Signal tag o)
  -- ^ Synchronous sequential function with input and output matching that
  -- of the mealy machine
mealy clk rst f iS =
  \i -> let (s',o) = unbundle $ f <$> s <*> i
            s      = register clk rst iS s'
        in  o
{-# INLINABLE mealy #-}

-- | A version of 'mealy' that does automatic 'Bundle'ing
--
-- Given a function @f@ of type:
--
-- @
-- __f__ :: Int -> (Bool,Int) -> (Int,(Int,Bool))
-- @
--
-- When we want to make compositions of @f@ in @g@ using 'mealy'', we have to
-- write:
--
-- @
-- g clk rst a b c = (b1,b2,i2)
--   where
--     (i1,b1) = 'unbundle' (mealy clk rst f 0 ('bundle' (a,b)))
--     (i2,b2) = 'unbundle' (mealy clk rst f 3 ('bundle' (i1,c)))
-- @
--
-- Using 'mealyB'' however we can write:
--
-- @
-- g clk rst a b c = (b1,b2,i2)
--   where
--     (i1,b1) = 'mealyB' clk rst f 0 (a,b)
--     (i2,b2) = 'mealyB' clk rst f 3 (i1,c)
-- @
mealyB
  :: ( KnownDomain tag dom
     , Undefined s
     , Bundle i
     , Bundle o )
  => Clock tag enabled
  -> Reset tag polarity
  -> (s -> i -> (s,o))
  -- ^ Transfer function in mealy machine form: @state -> input -> (newstate,output)@
  -> s
  -- ^ Initial state
  -> (Unbundled tag i -> Unbundled tag o)
 -- ^ Synchronous sequential function with input and output matching that
 -- of the mealy machine
mealyB clk rst f iS i = unbundle (mealy clk rst f iS (bundle i))
{-# INLINE mealyB #-}

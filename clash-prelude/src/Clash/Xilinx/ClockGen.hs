{-|
Copyright  :  (C) 2017, Google Inc
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

PLL and other clock-related components for Xilinx FPGAs
-}

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE GADTs            #-}
module Clash.Xilinx.ClockGen where

import Clash.Promoted.Symbol
import Clash.Signal.Internal
import Unsafe.Coerce

-- | A clock source that corresponds to the Xilinx PLL/MMCM component created
-- with the \"Clock Wizard\" with settings to provide a stable 'Clock' from
-- a single free-running input
--
-- Only works when configured with:
--
-- * 1 reference clock
-- * 1 output clock
-- * a reset port
-- * a locked port
--
-- You must use type applications to specify the output clock domain, e.g.:
--
-- @
-- type Dom100MHz = Dom \"A\" 10000
--
-- -- outputs a clock running at 100 MHz
-- clockWizard @@Dom100MHz (SSymbol @@"clkWizard50to100") clk50 rst
-- @
clockWizard
  :: forall tagIn tagOut periodIn periodOut edge init name
   . ( KnownDomain tagIn ('Domain tagIn periodIn edge 'Asynchronous init)
     , KnownDomain tagOut ('Domain tagOut periodOut edge 'Asynchronous init) )
  => SSymbol name
  -- ^ Name of the component, must correspond to the name entered in the
  -- \"Clock Wizard\" dialog.
  --
  -- For example, when you entered \"clockWizard50\", instantiate as follows:
  --
  -- > SSymbol @ "clockWizard50"
  -> Clock tagIn 'Regular
  -- ^ Free running clock (i.e. a clock pin connected to a crystal)
  -> Reset tagIn 'ActiveHigh
  -- ^ Reset for the PLL
  -> (Clock tagOut 'Regular, Signal tagOut Bool)
  -- ^ (Stable PLL clock, PLL lock)
clockWizard _ clk (ActiveHighReset rst) =
  (unsafeCoerce (toEnabledClock clk rst), unsafeCoerce rst)
{-# NOINLINE clockWizard #-}

-- | A clock source that corresponds to the Xilinx PLL/MMCM component created
-- with the \"Clock Wizard\", with settings to provide a stable 'Clock'
-- from differential free-running inputs.
--
-- Only works when configured with:
--
-- * 1 differential reference pair
-- * 1 output clock
-- * a reset port
-- * a locked port
--
-- You must use type applications to specify the output clock domain, e.g.:
--
-- @
-- type Dom100MHz = Dom \"A\" 10000
--
-- -- outputs a clock running at 100 MHz
-- clockWizardDifferential @@Dom100MHz (SSymbol @@"clkWizardD50to100") clk50N clk50P rst
-- @
clockWizardDifferential
  :: forall tagIn tagOut periodIn periodOut edge init name
   . ( KnownDomain tagIn ('Domain tagIn periodIn edge 'Asynchronous init)
     , KnownDomain tagOut ('Domain tagOut periodOut edge 'Asynchronous init) )
  => SSymbol name
  -- ^ Name of the component, must correspond to the name entered in the
  -- \"Clock Wizard\" dialog.
  --
  -- For example, when you entered \"clockWizardD50\", instantiate as follows:
  --
  -- > SSymbol @ "clockWizardD50"
  -> Clock tagIn 'Regular
  -- ^ Free running clock, negative phase
  -> Clock tagIn 'Regular
  -- ^ Free running clock, positive phase
  -> Reset tagIn 'ActiveHigh
  -- ^ Reset for the PLL
  -> (Clock tagOut 'Regular, Signal tagOut Bool)
  -- ^ (Stable PLL clock, PLL lock)
clockWizardDifferential _name clkP _clkN (ActiveHighReset rst) =
  (unsafeCoerce (toEnabledClock clkP rst), unsafeCoerce rst)
{-# NOINLINE clockWizardDifferential #-}

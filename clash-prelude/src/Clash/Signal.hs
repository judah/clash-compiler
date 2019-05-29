{-|
Copyright  :  (C) 2013-2016, University of Twente,
                  2016-2019, Myrtle Software Ltd,
                  2017     , Google Inc.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

Clash has synchronous 'Signal's in the form of:

@
'Signal' (tag :: 'Symbol') a
@

Where /a/ is the type of the value of the 'Signal', for example /Int/ or /Bool/,
and /tag/ is the /clock-/ (and /reset-/) domain to which the memory elements
manipulating these 'Signal's belong.

The type-parameter, /tag/, is of the kind 'Symbol' - a simple string. That
string refers to a single /synthesis domain/. A synthesis domain describes the
behavior of certain aspects of memory elements in it. More specifically, a
domain looks like:

@
'Domain'
  { _tag :: 'Symbol'
  -- ^ Domain name
  , _period :: 'Nat'
  -- ^ Clock period in /ps/
  , _edge :: 'ActiveEdge'
  -- ^ Determines which edge of the clock registers are sensitive to
  , _reset :: 'ResetKind'
  -- ^ Determines how components with reset lines respond to changes
  , _init :: 'InitBehavior'
  -- ^ Determines the initial (or "power up") value of various components
  }
@

Check the documentation of each of the types to see the various options Clash
provides. In order to specify a domain, an instance of 'KnownDomain' should be
made. Clash provides an implementation 'System' with some common options
chosen:

@
instance KnownDomain "System" ('Domain "System" 10000 'Rising 'Asynchronous 'Defined 'ActiveHigh) where
  knownDomain = SDomain SSymbol SNat SRising SAsynchronous SDefined SActiveHigh
@

In words, "System" is a synthesis domain with a clock running with a period
of 10000 /ps/. Memory elements respond to the rising edge of the clock,
asynchronously to changes in their resets, and have defined power up values
if applicable.

* __NB__: \"Bad things\"™  happen when you actually use a clock period of @0@,
so do __not__ do that!
* __NB__: You should be judicious using a clock with period of @1@ as you can
never create a clock that goes any faster!
* __NB__: Whether 'System' has good defaults depends on your target platform.
Check out 'IntelSystem' and 'XilinxSystem' too!
-}

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Signal
  ( -- * Synchronous signals
    Signal
  , BiSignalIn
  , BiSignalOut
  , BiSignalDefault(..)
    -- * Domain
  , KnownDomain(..)
  , ActiveEdge(..)
  , SActiveEdge(..)
  , InitBehavior(..)
  , SInitBehavior(..)
  , ResetKind(..)
  , SResetKind(..)
  , ResetPolarity(..)
  , Domain(..)
  , SDomain(..)
    -- ** Default domains
  , System
  , XilinxSystem
  , IntelSystem
  , vSystem
  , vIntelSystem
  , vXilinxSystem
    -- ** Domain utilities
  , VDomain(..)
  , vDomain
  , createDomain
  , knownVDomain
    -- * Clock
  , Clock
    -- * Reset
  , Reset(..)
  , fromSyncReset
  , unsafeToReset
  , unsafeFromReset
  , unsafeToHighPolarity
  , unsafeToLowPolarity
  , unsafeFromHighPolarity
  , unsafeFromLowPolarity
  , E.resetSynchronizer
    -- ** Enabling
  , Enable(..)
  , toEnable
  , fromEnable
  , S.enableGen
    -- * Hidden clocks and resets
    -- $hiddenclockandreset

    -- ** Hidden clock
  , HiddenClock
  , hideClock
  , exposeClock
  , withClock
  , hasClock
    -- ** Hidden reset
  , HiddenReset
  , hideReset
  , exposeReset
  , withReset
  , hasReset
    -- ** Hidden enable
  , HiddenEnable
  , hideEnable
  , exposeEnable
  , withEnable
  , hasEnable
    -- ** Hidden clock, reset, and enable
  , HiddenClockResetEnable
  , hideClockResetEnable
  , exposeClockResetEnable
  , withClockResetEnable
  , SystemClockResetEnable
    -- * Basic circuit functions
  , fastDelay
  , delay
  , delayMaybe
  , delayEn
  , register
  , regMaybe
  , regEn
  , mux
    -- * Simulation and testbench functions
  , clockGen
  , tbClockGen
  , resetGen
  , systemClockGen
  , tbSystemClockGen
  , systemResetGen
    -- * Boolean connectives
  , (.&&.), (.||.)
    -- * Product/Signal isomorphism
  , Bundle(..)
    -- * Simulation functions (not synthesizable)
  , simulate
  , simulateB
    -- ** lazy versions
  , simulate_lazy
  , simulateB_lazy
    -- * List \<-\> Signal conversion (not synthesizable)
  , sample
  , sampleN
  , fromList
    -- ** lazy versions
  , sample_lazy
  , sampleN_lazy
  , fromList_lazy
    -- * QuickCheck combinators
  , testFor
    -- * Type classes
    -- ** 'Eq'-like
  , (.==.), (./=.)
    -- ** 'Ord'-like
  , (.<.), (.<=.), (.>=.), (.>.)
    -- * Bisignal functions
  , veryUnsafeToBiSignalIn
  , readFromBiSignal
  , writeToBiSignal
  , mergeBiSignalOuts
  )
where

import           GHC.TypeLits
  (KnownNat, KnownSymbol, AppendSymbol, Symbol)
import           Data.Bits             (Bits) -- Haddock only
import           Data.Maybe            (isJust, fromJust)
import           Prelude
import           Test.QuickCheck       (Property, property)

import qualified Clash.Explicit.Signal as E
import           Clash.Explicit.Signal
  (System, resetSynchronizer, systemClockGen, systemResetGen, tbSystemClockGen)
import qualified Clash.Explicit.Signal as S
import           Clash.Hidden
import           Clash.Promoted.Nat    (SNat (..))
import           Clash.Promoted.Symbol (SSymbol (..))
import           Clash.Signal.Bundle   (Bundle (..))
import           Clash.Signal.BiSignal --(BisignalIn, BisignalOut, )
import           Clash.Signal.Internal hiding
  (sample, sample_lazy, sampleN, sampleN_lazy, simulate, simulate_lazy, testFor)
import qualified Clash.Signal.Internal as S
import           Clash.XException      (Undefined)

{- $setup
>>> :set -XFlexibleContexts -XTypeApplications
>>> import Clash.XException (printX)
>>> import Control.Applicative (liftA2)
>>> let oscillate = register False (not <$> oscillate)
>>> let count = regEn 0 oscillate (count + 1)
>>> :{
sometimes1 = s where
  s = register Nothing (switch <$> s)
  switch Nothing = Just 1
  switch _       = Nothing
:}

>>> :{
countSometimes = s where
  s     = regMaybe 0 (plusM (pure <$> s) sometimes1)
  plusM = liftA2 (liftA2 (+))
:}

-}

-- * Hidden clock and reset arguments

{- $hiddenclockandreset #hiddenclockandreset#
Clocks and resets are by default implicitly routed to their components. You can
see from the type of a component whether it has hidden clock or reset
arguments:

It has a hidden clock when it has a:

@
f :: 'HiddenClock' tag => ...
@

Constraint.

Or it has a hidden reset when it has a:

@
g :: 'HiddenReset' tag polarity => ...
@

Constraint.

Or it has both a hidden clock argument and a hidden reset argument when it
has a:

@
h :: 'HiddenClockReset' tag  => ..
@

Constraint.

Given a component with an explicit clock and reset arguments, you can turn them
into hidden arguments using 'hideClock' and 'hideReset'. So given a:

@
f :: Clock tag -> Reset tag -> Signal tag a -> ...
@

You hide the clock and reset arguments by:

@
-- g :: 'HiddenClockReset' tag  => Signal tag a -> ...
g = 'hideClockReset' f
@

Or, alternatively, by:

@
-- h :: HiddenClockResetEnable tag dom => Signal tag a -> ...
h = f 'hasClock' 'hasReset'
@

=== Assigning explicit clock and reset arguments to hidden clocks and resets

Given a component:

@
f :: HiddenClockResetEnable tag dom
  => Signal tag Int
  -> Signal tag Int
@

which has hidden clock and routed reset arguments, we expose those hidden
arguments so that we can explicitly apply them:

@
-- g :: Clock tag -> Reset tag -> Signal tag Int -> Signal tag Int
g = 'exposeClockResetEnable' f
@

or, alternatively, by:

@
-- h :: Clock tag -> Reset tag -> Signal tag Int -> Signal tag Int
h clk rst = withClock clk rst f
@

Similarly, there are 'exposeClock' and 'exposeReset' to connect just expose
the hidden clock or the hidden reset argument.

You will need to explicitly apply clocks and resets when you want to use
components such as PPLs and 'resetSynchronizer':

@
topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System Int
  -> Signal System Int
topEntity clk rst =
  let (pllOut,pllStable) = 'Clash.Intel.ClockGen.altpll' (SSymbol \@\"altpll50\") clk rst
      rstSync            = 'resetSynchronizer' pllOut ('unsafeToAsyncReset' pllStable)
  in  'exposeClockResetEnable' f pllOut rstSync
@

or, using the alternative method:

@
topEntity2
  :: Clock System
  -> Reset System
  -> Signal System Int
  -> Signal System Int
topEntity2 clk rst =
  let (pllOut,pllStable) = 'Clash.Intel.ClockGen.altpll' (SSymbol \@\"altpll50\") clk rst
      rstSync            = 'resetSynchronizer' pllOut ('unsafeToAsyncReset' pllStable)
  in  'withClockReset' pllOut rstSync f
@

-}

type HiddenClockName tag = AppendSymbol tag "_clk"
type HiddenResetName tag = AppendSymbol tag "_rst"
type HiddenEnableName tag = AppendSymbol tag "_en"

-- | A /constraint/ that indicates the component has a hidden 'Clock'
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
type HiddenClock tag dom =
  ( Hidden (HiddenClockName tag) (Clock tag)
  , KnownDomain tag dom )

-- | A /constraint/ that indicates the component needs a 'Reset'
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
type HiddenReset tag dom =
  ( Hidden (HiddenResetName tag) (Reset tag)
  , KnownDomain tag dom )

-- | A /constraint/ that indicates the component needs a 'Enable'
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
type HiddenEnable tag dom =
  ( Hidden (HiddenEnableName tag) (Enable tag)
  , KnownDomain tag dom )

-- | A /constraint/ that indicates the component needs a 'Clock', a 'Reset',
-- and an 'Enable' belonging to the same tag.
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
type HiddenClockResetEnable tag dom =
  ( HiddenClock tag dom
  , HiddenReset tag dom
  , HiddenEnable tag dom
  )

-- | A /constraint/ that indicates the component needs a 'Clock', a 'Reset',
-- and an 'Enable' belonging to the 'System' tag.
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
type SystemClockResetEnable =
  ( Hidden (HiddenClockName System) (Clock System)
  , Hidden (HiddenResetName System) (Reset System)
  , Hidden (HiddenEnableName System) (Enable System)
  )

-- | Expose the hidden 'Clock' argument of a component, so it can be applied
-- explicitly
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
exposeClock
  :: forall tag dom  r
   . (HiddenClock tag dom => r)
  -- ^ The component with a hidden clock
  -> (KnownDomain tag dom => Clock tag -> r)
  -- ^ The component with its clock argument exposed
exposeClock = \f clk -> expose @(HiddenClockName tag) f clk
{-# INLINE exposeClock #-}

-- | Hide the 'Clock' argument of a component, so it can be routed implicitly.
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
hideClock
  :: forall tag dom  r
   . HiddenClock tag dom
  => (Clock tag -> r)
  -- ^ Function whose clock argument you want to hide
  -> r
hideClock = \f -> f (fromLabel @(HiddenClockName tag))
{-# INLINE hideClock #-}

-- | Connect an explicit 'Clock' to a function with a hidden 'Clock' argument.
--
-- @
-- withClock = 'flip' exposeClock
-- @
withClock
  :: forall tag dom  r
   . KnownDomain tag dom
  => Clock tag
  -- ^ The 'Clock' we want to connect
  -> (HiddenClock tag dom => r)
  -- ^ The function with a hidden 'Clock' argument
  -> r
withClock = \clk f -> expose @(HiddenClockName tag) f clk
{-# INLINE withClock #-}

-- | Connect a hidden 'Clock' to an argument where a normal 'Clock' argument
-- was expected.
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
hasClock
  :: forall tag dom
   . HiddenClock tag dom
  => Clock tag
hasClock = fromLabel @(HiddenClockName tag)
{-# INLINE hasClock #-}

-- | Expose the hidden 'Reset' argument of a component, so it can be applied
-- explicitly
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
exposeReset
  :: forall tag dom r
   . (HiddenReset tag dom => r)
  -- ^ The component with a hidden reset
  -> (KnownDomain tag dom => Reset tag -> r)
  -- ^ The component with its reset argument exposed
exposeReset = \f rst -> expose @(HiddenResetName tag) f rst
{-# INLINE exposeReset #-}

-- | Hide the 'Reset' argument of a component, so it can be routed implicitly.
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
hideReset
  :: forall tag dom r
   . HiddenReset tag dom
  => (Reset tag -> r)
  -- ^ Component whose reset argument you want to hide
  -> r
hideReset = \f -> f (fromLabel @(HiddenResetName tag))
{-# INLINE hideReset #-}

-- | Connect an explicit 'Reset' to a function with a hidden 'Reset' argument.
--
-- @
-- withReset = 'flip' exposeReset
-- @
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
withReset
  :: forall tag dom r
   . KnownDomain tag dom
  => Reset tag
  -- ^ The 'Reset' we want to connect
  -> (HiddenReset tag dom => r)
  -- ^ The function with a hidden 'Reset' argument
  -> r
withReset = \rst f -> expose @(HiddenResetName tag) f rst
{-# INLINE withReset #-}

-- | Connect a hidden 'Reset' to an argument where a normal 'Reset' argument
-- was expected.
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
hasReset
  :: forall tag dom
   . HiddenReset tag dom
  => Reset tag
hasReset = fromLabel @(HiddenResetName tag)
{-# INLINE hasReset #-}

-- | Expose the hidden 'Enable' argument of a component, so it can be applied
-- explicitly
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
exposeEnable
  :: forall tag dom r
   . (HiddenEnable tag dom => r)
  -- ^ The component with a hidden reset
  -> (KnownDomain tag dom => Enable tag -> r)
  -- ^ The component with its reset argument exposed
exposeEnable = \f rst -> expose @(HiddenEnableName tag) f rst
{-# INLINE exposeEnable #-}

-- | Hide the 'Enable' argument of a component, so it can be routed implicitly.
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
hideEnable
  :: forall tag dom r
   . HiddenEnable tag dom
  => (Enable tag -> r)
  -- ^ Component whose reset argument you want to hide
  -> r
hideEnable = \f -> f (fromLabel @(HiddenEnableName tag))
{-# INLINE hideEnable #-}

-- | Connect an explicit 'Enable' to a function with a hidden 'Enable' argument.
--
-- @
-- withEnable = 'flip' exposeEnable
-- @
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
withEnable
  :: forall tag dom r
   . KnownDomain tag dom
  => Enable tag
  -- ^ The 'Enable' we want to connect
  -> (HiddenEnable tag dom => r)
  -- ^ The function with a hidden 'Enable' argument
  -> r
withEnable = \rst f -> expose @(HiddenEnableName tag) f rst
{-# INLINE withEnable #-}

-- | Connect a hidden 'Enable' to an argument where a normal 'Enable' argument
-- was expected.
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
hasEnable
  :: forall tag dom
   . HiddenEnable tag dom
  => Enable tag
hasEnable = fromLabel @(HiddenEnableName tag)
{-# INLINE hasEnable #-}


-- | Expose the hidden 'Clock', 'Reset', and 'Enable' arguments of a component,
-- so they can be applied explicitly
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
--
-- === __Example__
--
-- @
-- topEntity :: Vec 2 (Vec 3 (Unsigned 8)) -> Vec 6 (Unsigned 8)
-- topEntity = concat
--
-- testBench :: Signal System Bool
-- testBench = done
--   where
--     testInput      = pure ((1 :> 2 :> 3 :> Nil) :> (4 :> 5 :> 6 :> Nil) :> Nil)
--     expectedOutput = outputVerifier ((1:>2:>3:>4:>5:>6:>Nil):>Nil)
--     done           = exposeClockResetEnable (expectedOutput (topEntity <$> testInput)) clk rst
--     clk            = tbSystemClockGen (not <\$\> done)
--     rst            = systemResetGen
-- @
exposeClockResetEnable
  :: forall tag dom r
   . (HiddenClockResetEnable tag dom => r)
  -- ^ The component with hidden clock, reset, and enable arguments
  -> (KnownDomain tag dom => Clock tag -> Reset tag -> Enable tag -> r)
  -- ^ The component with its clock, reset, and enable arguments exposed
exposeClockResetEnable =
  \f clk rst en ->
    expose
      @(HiddenEnableName tag)
      ( expose
          @(HiddenResetName tag)
          ( expose
              @(HiddenClockName tag)
              f
              clk )
          rst
      )
      en
{-# INLINE exposeClockResetEnable #-}

-- -- | Hide the 'Clock' and 'Reset' arguments of a component, so they can be
-- -- routed implicitly
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
hideClockResetEnable
  :: forall tag dom r
   . HiddenClockResetEnable tag dom
  => (KnownDomain tag dom => Clock tag -> Reset tag -> Enable tag -> r)
  -- ^ Component whose clock, reset, and enable argument you want to hide
  -> r
hideClockResetEnable =
  \f ->
    f
      (fromLabel @(HiddenClockName tag))
      (fromLabel @(HiddenResetName tag))
      (fromLabel @(HiddenEnableName tag))
{-# INLINE hideClockResetEnable #-}

-- | Connect an explicit 'Clock' and 'Reset' to a function with a hidden
-- 'Clock' and 'Reset' argument. In order for this function to work as
-- expected, you need to explicitly relate the /tag/ in the arguments with
-- the result. You can do this either by supplying a type signature:
--
-- @
-- h :: Clock System -> Reset System -> Signal System Int
-- h = withClockReset clk rst f
-- @
--
-- Or by type applying both sides:
--
-- @
-- g = withClockReset @System clk rst (f @System)
-- @
--
-- If omitted, GHC / Clash will fail to infer the type you might expect. This
-- is a known limitation in GHC / Haskell for now. Monomorphism isn't
-- a requirement. I.e., the following is fine:
--
-- @
-- i :: Clock tag -> Reset tag -> Signal tag Int
-- i = withClockReset clk rst f
-- @
--
-- as well as
--
-- @
-- j = withClockReset @tag clk rst (f @tag)
-- @
--
-- .. as they both explicitly "connect" the /tag/ in the arguments and result.
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
withClockResetEnable
  :: forall tag r dom
   . KnownDomain tag dom
  => Clock tag
  -- ^ The 'Clock' we want to connect
  -> Reset tag
  -- ^ The 'Reset' we want to connect
  -> Enable tag
  -- ^ The 'Enable' we want to connect
  -> (HiddenClockResetEnable tag dom => r)
  -- ^ The function with a hidden 'Clock', hidden 'Reset', and hidden
  -- 'Enable' argument
  -> r
withClockResetEnable =
  \clk rst en f ->
    expose
      @(HiddenEnableName tag)
      ( expose
          @(HiddenResetName tag)
          ( expose
              @(HiddenClockName tag)
              f
              clk)
          rst )
      en
{-# INLINE withClockResetEnable #-}

-- * Basic circuit functions

-- | Special version of 'delay' that doesn't take enable signals of any kind. Is
-- comparable to Intel's "Hyper-Register"s. Initial value will be undefined.
fastDelay
  :: forall tag dom a
   . ( HiddenClock tag dom
     , Undefined a )
  => Signal tag a
  -> Signal tag a
fastDelay =
  E.fastDelay (fromLabel @(HiddenClockName tag))
{-# INLINE fastDelay #-}

-- | 'delay' @s@ delays the values in 'Signal' @s@ for once cycle, the value
-- at time 0 is /dflt/.
--
-- >>> sampleN @System 3 (delay 0 (fromList [1,2,3,4]))
-- [0,1,2]
delay
  :: forall tag dom  a
   . ( Undefined a
     , HiddenClock tag dom
     , HiddenEnable tag dom )
  => a
  -- ^ Default value
  -> Signal tag a
  -- ^ Signal to delay
  -> Signal tag a
delay dflt i =
  delay#
    (fromLabel @(HiddenClockName tag))
    (fromLabel @(HiddenEnableName tag))
    dflt
    i
{-# INLINE delay #-}

-- | Version of 'delay' that only updates when its second argument is a 'Just'
-- value.
--
-- >>> let input = fromList [Just 1, Just 2, Nothing, Nothing, Just 5, Just 6, Just (7::Int)]
-- >>> sampleN @System 7 (delayMaybe 0 input)
-- [0,1,2,2,2,5,6]
delayMaybe
  :: forall tag dom  a
   . ( Undefined a
     , HiddenClock tag dom
     , HiddenEnable tag dom )
  => a
  -- ^ Initial value
  -> Signal tag (Maybe a)
  -> Signal tag a
delayMaybe dflt i =
  E.delayMaybe
    (fromLabel @(HiddenClockName tag))
    (fromLabel @(HiddenEnableName tag))
    dflt
    i
{-# INLINE delayMaybe #-}

-- | Version of 'delay' that only updates when its second argument is asserted.
--
-- >>> let input = fromList [1,2,3,4,5,6,7::Int]
-- >>> let enable = fromList [True,True,False,False,True,True,True]
-- >>> sampleN @System 7 (delayEn 0 enable input)
-- [0,1,2,2,2,5,6]
delayEn
  :: forall tag dom  a
   . ( Undefined a
     , HiddenClock tag dom
     , HiddenEnable tag dom )
  => a
  -- ^ Initial value
  -> Signal tag Bool
  -- ^ Enable
  -> Signal tag a
  -> Signal tag a
delayEn dflt en i =
  E.delayEn
    (fromLabel @(HiddenClockName tag))
    (fromLabel @(HiddenEnableName tag))
    dflt
    en
    i
{-# INLINE delayEn #-}

-- | 'register' @i s@ delays the values in 'Signal' @s@ for one cycle, and sets
-- the value at time 0 to @i@
--
-- >>> sampleN @System 5 (register 8 (fromList [1,1,2,3,4]))
-- [8,8,1,2,3]
register
  :: forall tag dom a
   . ( KnownDomain tag dom
     , Undefined a )
  => HiddenClockResetEnable tag dom
  => a
  -- ^ Reset value
  --
  -- 'register' outputs the reset value when the reset value is active
  -> Signal tag a
  -> Signal tag a
register i s =
  E.register
    (fromLabel @(HiddenClockName tag))
    (fromLabel @(HiddenResetName tag))
    (fromLabel @(HiddenEnableName tag))
    i
    s
{-# INLINE register #-}
infixr 3 `register`

-- | Version of 'register' that only updates its content when its second
-- argument is a 'Just' value. So given:
--
-- @
-- sometimes1 = s where
--   s = 'register' Nothing (switch '<$>' s)
--
--   switch Nothing = Just 1
--   switch _       = Nothing
--
-- countSometimes = s where
--   s     = 'regMaybe' 0 (plusM ('pure' '<$>' s) sometimes1)
--   plusM = 'liftA2' (liftA2 (+))
-- @
--
-- We get:
--
-- >>> sampleN @System 9 sometimes1
-- [Nothing,Nothing,Just 1,Nothing,Just 1,Nothing,Just 1,Nothing,Just 1]
-- >>> sampleN @System 9 countSometimes
-- [0,0,0,1,1,2,2,3,3]
regMaybe
  :: forall tag dom a
   . ( HiddenClockResetEnable tag dom
     , Undefined a )
  => a
  -- ^ Reset value. 'regMaybe' outputs the reset value when the reset is active.
  -> Signal tag (Maybe a)
  -> Signal tag a
regMaybe initial iM =
  E.regMaybe
    (fromLabel @(HiddenClockName tag))
    (fromLabel @(HiddenResetName tag))
    (fromLabel @(HiddenEnableName tag))
    initial
    iM
{-# INLINE regMaybe #-}
infixr 3 `regMaybe`

-- | Version of 'register' that only updates its content when its second argument
-- is asserted. So given:
--
-- @
-- oscillate = 'register' False ('not' '<$>' oscillate)
-- count     = 'regEn' 0 oscillate (count + 1)
-- @
--
-- We get:
--
-- >>> sampleN @System 9 oscillate
-- [False,False,True,False,True,False,True,False,True]
-- >>> sampleN @System 9 count
-- [0,0,0,1,1,2,2,3,3]
regEn
  :: forall tag dom a
   . ( HiddenClockResetEnable tag dom
     , Undefined a )
  => a
  -- ^ Reset value
  --
  -- 'regEn' outputs the reset value when the reset value is active
  -> Signal tag Bool
  -> Signal tag a
  -> Signal tag a
regEn initial en i =
  E.regEn
    (fromLabel @(HiddenClockName tag))
    (fromLabel @(HiddenResetName tag))
    (fromLabel @(HiddenEnableName tag))
    initial
    en
    i
{-# INLINE regEn #-}

-- * Signal -> List conversion

-- | Get an infinite list of samples from a 'Clash.Signal.Signal'
--
-- The elements in the list correspond to the values of the 'Signal'
-- at consecutive clock cycles
--
-- > sample s == [s0, s1, s2, s3, ...
--
-- __NB__: This function is not synthesizable
sample
  :: forall tag dom a
   . ( KnownDomain tag dom
     , Undefined a )
  => (HiddenClockResetEnable tag dom => Signal tag a)
  -- ^ 'Signal' we want to sample, whose source potentially has a hidden clock
  -- (and reset)
  -> [a]
sample s =
  case knownDomain @tag of
    SDomain tag _ _ _ _ _ ->
      let clk = Clock tag in
      let rst = Reset tag (True :- pure False) in
      let en = S.enableGen in
      S.sample (exposeClockResetEnable s clk rst en)
{-# NOINLINE sample #-}

-- | Get a list of /n/ samples from a 'Signal'
--
-- The elements in the list correspond to the values of the 'Signal'
-- at consecutive clock cycles
--
-- > sampleN @System 3 s == [s0, s1, s2]
--
-- __NB__: This function is not synthesizable
sampleN
  :: forall tag dom a
   . ( KnownDomain tag dom
     , Undefined a )
  => Int
  -- ^ The number of samples we want to see
  -> (HiddenClockResetEnable tag dom => Signal tag a)
  -- ^ 'Signal' we want to sample, whose source potentially has a hidden clock
  -- (and reset)
  -> [a]
sampleN n s =
  case knownDomain @tag of
    SDomain tag _ _ _ _ _ ->
      let clk = Clock tag in
      let rst = Reset tag (True :- pure False) in
      let en = S.enableGen in
      S.sampleN n (exposeClockResetEnable s clk rst en)
{-# NOINLINE sampleN #-}

-- | /Lazily/ get an infinite list of samples from a 'Clash.Signal.Signal'
--
-- The elements in the list correspond to the values of the 'Signal'
-- at consecutive clock cycles
--
-- > sample s == [s0, s1, s2, s3, ...
--
-- __NB__: This function is not synthesizable
sample_lazy
  :: forall tag dom a
   . KnownDomain tag dom
  => (HiddenClockResetEnable tag dom => Signal tag a)
  -- ^ 'Signal' we want to sample, whose source potentially has a hidden clock
  -- (and reset)
  -> [a]
sample_lazy s =
  case knownDomain @tag of
    SDomain tag _ _ _ _ _ ->
      let clk = Clock tag in
      let rst = Reset tag (True :- pure False) in
      let en = S.enableGen in
      S.sample_lazy (exposeClockResetEnable s clk rst en)
{-# NOINLINE sample_lazy #-}

-- | Lazily get a list of /n/ samples from a 'Signal'
--
-- The elements in the list correspond to the values of the 'Signal'
-- at consecutive clock cycles
--
-- > sampleN @System 3 s == [s0, s1, s2]
--
-- __NB__: This function is not synthesizable
sampleN_lazy
  :: forall tag dom a
   . KnownDomain tag dom
  => Int
  -> (HiddenClockResetEnable tag dom => Signal tag a)
  -- ^ 'Signal' we want to sample, whose source potentially has a hidden clock
  -- (and reset)
  -> [a]
sampleN_lazy n s =
  case knownDomain @tag of
    SDomain tag _ _ _ _ _ ->
      let clk = Clock tag in
      let rst = Reset tag (True :- pure False) in
      let en = S.enableGen in
      S.sampleN_lazy n (exposeClockResetEnable s clk rst en)
{-# NOINLINE sampleN_lazy #-}

-- * Simulation functions

-- | Simulate a (@'Signal' a -> 'Signal' b@) function given a list of samples
-- of type /a/
--
-- >>> simulate @System (register 8) [1, 2, 3]
-- [8,1,2,3...
-- ...
--
-- Where 'System' denotes the /domain/ to simulate on.
--
-- __NB__: This function is not synthesizable
simulate
  :: forall tag dom a b
   . ( KnownDomain tag dom
     , Undefined a
     , Undefined b )
  => (HiddenClockResetEnable tag dom =>
      Signal tag a -> Signal tag b)
  -- ^ 'Signal' we want to sample, whose source potentially has a hidden clock
  -- (and reset)
  -> [a]
  -> [b]
simulate f =
  case knownDomain @tag of
    SDomain tag _ _ _ _ _ ->
      let clk = Clock tag in
      let rst = Reset tag (True :- pure False) in
      let en = S.enableGen in
      -- TODO: Explain why we skip the the first value here
      tail . S.simulate (exposeClockResetEnable f clk rst en) . dup1
{-# NOINLINE simulate #-}

-- | /Lazily/ simulate a (@'Signal' a -> 'Signal' b@) function given a list of
-- samples of type /a/
--
-- >>> simulate @System (register 8) [1, 2, 3]
-- [8,1,2,3...
-- ...
--
-- __NB__: This function is not synthesizable
simulate_lazy
  :: forall tag dom a b
   . KnownDomain tag dom
  => (HiddenClockResetEnable tag dom =>
      Signal tag a -> Signal tag b)
  -- ^ Function we want to simulate, whose components potentially have a hidden
  -- clock (and reset)
  -> [a]
  -> [b]
simulate_lazy f =
  case knownDomain @tag of
    SDomain tag _ _ _ _ _ ->
      let clk = Clock tag in
      let rst = Reset tag (True :- pure False) in
      let en = S.enableGen in
      tail . S.simulate_lazy (exposeClockResetEnable f clk rst en) . dup1
{-# NOINLINE simulate_lazy #-}

-- | Simulate a (@'Unbundled' a -> 'Unbundled' b@) function given a list of
-- samples of type @a@
--
-- >>> simulateB @System (unbundle . register (8,8) . bundle) [(1,1), (2,2), (3,3)] :: [(Int,Int)]
-- [(8,8),(1,1),(2,2),(3,3)...
-- ...
--
-- __NB__: This function is not synthesizable
simulateB
  :: forall tag dom a b
   . ( KnownDomain tag dom
     , Bundle a
     , Bundle b
     , Undefined a
     , Undefined b )
  => (HiddenClockResetEnable tag dom =>
      Unbundled tag a -> Unbundled tag b)
  -- ^ Function we want to simulate, whose components potentially have a hidden
  -- clock (and reset)
  -> [a]
  -> [b]
simulateB f =
  case knownDomain @tag of
    SDomain tag _ _ _ _ _ ->
      let clk = Clock tag in
      let rst = Reset tag (True :- pure False) in
      let en = S.enableGen in
      tail . S.simulateB (exposeClockResetEnable f clk rst en) . dup1
{-# NOINLINE simulateB #-}

-- | /Lazily/ simulate a (@'Unbundled' a -> 'Unbundled' b@) function given a
-- list of samples of type @a@
--
-- >>> simulateB @System (unbundle . register (8,8) . bundle) [(1,1), (2,2), (3,3)] :: [(Int,Int)]
-- [(8,8),(1,1),(2,2),(3,3)...
-- ...
--
-- __NB__: This function is not synthesizable
simulateB_lazy
  :: forall tag dom a b
   . ( KnownDomain tag dom
     , Bundle a
     , Bundle b )
  => (HiddenClockResetEnable tag dom =>
      Unbundled tag a -> Unbundled tag b)
  -- ^ Function we want to simulate, whose components potentially have a hidden
  -- clock (and reset)
  -> [a]
  -> [b]
simulateB_lazy f =
  case knownDomain @tag of
    SDomain tag _ _ _ _ _ ->
      let clk = Clock tag in
      let rst = Reset tag (True :- pure False) in
      let en = S.enableGen in
      tail . S.simulateB_lazy (exposeClockResetEnable f clk rst en) . dup1
{-# NOINLINE simulateB_lazy #-}

dup1 :: [a] -> [a]
dup1 (x:xs) = x:x:xs
dup1 _      = error "empty list"

-- * QuickCheck combinators

-- |  @testFor n s@ tests the signal /s/ for /n/ cycles.
testFor
  :: KnownDomain tag dom
  => Int
  -- ^ The number of cycles we want to test for
  -> (HiddenClockResetEnable tag dom => Signal tag Bool)
  -- ^ 'Signal' we want to evaluate, whose source potentially has a hidden clock
  -- (and reset)
  -> Property
testFor n s = property (and (Clash.Signal.sampleN n s))

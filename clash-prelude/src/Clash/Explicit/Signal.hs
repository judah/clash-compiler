{-|
Copyright  :  (C) 2013-2016, University of Twente,
                  2016-2019, Myrtle Software,
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
instance KnownDomain "System" ('Domain "System" 10000 'Rising 'Asynchronous 'Defined) where
  knownDomain = SDomain SSymbolSNat SRising SAsynchronous SDefined
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

=== Explicit clocks and resets, and meta-stability #metastability#

When using multiple clocks and/or reset lines there are ways to accidentally
introduce situations that are prone to
<https://en.wikipedia.org/wiki/Metastability_in_electronics metastability>.
These bugs are incredibly hard to debug as they often cannot be simulated, so
it's best to prevent them in the first place. This section outlines the
situations in which metastability arises and how to prevent it.

In total four types of clocks and resets exist. The different clock types are
annotated in their types:

@
'Clock' tag 'Regular'
'Clock' tag 'Enabled'
@

While reset types tend not to change in a single design and are therefore
encoded in a /synthesis domain/:

@
'Domain' "SyncExample" _period _edge 'Synchronous' _init
'Domain' "AsyncExample" _period _edge 'Asynchronous' _init
@

See the previous section on how to use domains.

We now go over the combinations over these clock and reset line combinations
and explain when they can potentially introduce situations prone to
meta-stability:

    *   /Reset situation 1/:

        @
        f :: Reset "SyncExample" polarity -> Reset "SyncExample" polarity -> ..
        f x y = ..
        @

        There are no problems here, because although /x/ and /y/ can have
        different values, components to these reset lines are reset
        /synchronously/, and there is no metastability situation.

    *   /Reset situation 2/:

        @
        g :: Reset "AsyncExample" polarity -> Reset "AsyncExample" polarity -> ..
        g x y = ..
        @

        This situation can be prone to metastability, because although /x/ and
        /y/ belong to the same /domain/ according to their tag, there is no
        guarantee that they actually originate from the same source. This means
        that one component can enter its reset state asynchronously to another
        component, inducing metastability in the other component.

        * This is the reason why `fromAsyncReset` does not exist, while
         `fromSyncReset` does. Instead, for async resets you should use
         `unsafeFromReset` which is explicitly marked /unsafe/.

    *   /Reset situation 3/:

        @
        h :: Reset "AsyncExample" polarity -> Reset "SyncExample" polarity -> ..
        h x y = ..
        @

        This situation is prone to metastability too, because again, one
        component can enter its reset state asynchronously to the other,
        inducing metastability in the other component.

          * Although in a standalone context, converting between
          @'Reset' "SyncExample" polarity@ and @'Signal' "SyncExample" 'Bool'@
          would be safe from a metastability point of view, it is not when we're
          in a context where asynchronous resets exist too. That is why
          'unsafeToActive{High,Low}Reset' are prefixed with the word /unsafe/.

    *   /Clock situations 1, 2, and 3/:

        @
        k :: Clock tag Regular -> Clock tag Regular -> ..
        k x y = ..

        l :: Clock tag Regular -> Clock tag Enabled -> ..
        l x y = ..

        m :: Clock tag Enabled -> Clock tag Enabled -> ..
        m x y = ..
        @

        All the above situations are potentially prone to metastability, because
        even though /x/ and /y/ belong to the same /domain/ according to their
        tag, there is no guarantee that they actually originate from the same
        source. They could hence be connected to completely unrelated clock
        sources, and components can then induce metastable states in others.

-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns          #-}

{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Explicit.Signal
  ( -- * Synchronous signal
    Signal
    -- * Domain
  , KnownDomain(..)
  , ActiveEdge(..)
  , SActiveEdge(..)
  , InitBehavior(..)
  , SInitBehavior(..)
  , ResetKind(..)
  , SResetKind(..)
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
  , Clock, ClockKind (..)
  , freqCalc
    -- ** Synchronization primitive
  , unsafeSynchronizer
    -- ** Clock gating
  , toEnabledClock
    -- * Reset
  , Reset(..)
  , ResetPolarity(..)
  , fromSyncReset
  , syncPolarity
  , toHighPolarity
  , toLowPolarity
  , unsafeFromReset
  , unsafeToActiveHighReset
  , unsafeToActiveLowReset
  , resetSynchronizer
    -- * Basic circuit functions
  , delay
  , delayMaybe
  , delayEn
  , register
  , regMaybe
  , regEn
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
  )
where

import Data.Maybe            (isJust, fromJust)

import Clash.Signal.Bundle   (Bundle (..))
import Clash.Signal.Internal
import Clash.XException      (Undefined)

{- $setup
>>> :set -XDataKinds -XTypeApplications -XFlexibleInstances -XMultiParamTypeClasses
>>> import Clash.Explicit.Prelude
>>> import qualified Data.List as L
>>> :{
instance KnownDomain "Dom2" ('Domain "Dom2" 2 'Rising 'Asynchronous 'Defined) where
  knownDomain = SDomain SSymbolSNat SRising SAsynchronous SDefined
:}

>>> :{
instance KnownDomain "Dom7" ('Domain "Dom7" 7 'Rising 'Asynchronous 'Defined) where
  knownDomain = SDomain SSymbolSNat SRising SAsynchronous SDefined
:}

>>> type Dom2 = "Dom2"
>>> type Dom7 = "Dom7"
>>> let clk2 = clockGen @Dom2
>>> let clk7 = clockGen @Dom7
>>> let oversampling clkA clkB dflt = delay clkB dflt . unsafeSynchronizer clkA clkB . delay clkA dflt
>>> let almostId clkA clkB dflt = delay clkB dflt . unsafeSynchronizer clkA clkB . delay clkA dflt . unsafeSynchronizer clkB clkA . delay clkB dflt
>>> let oscillate clk rst = let s = register clk rst False (not <$> s) in s
>>> let count clk rst = let s = regEn clk rst 0 (oscillate clk rst) (s + 1) in s
>>> :{
sometimes1 clk rst = s where
  s = register clk rst Nothing (switch <$> s)
  switch Nothing = Just 1
  switch _       = Nothing
:}

>>> :{
countSometimes clk rst = s where
  s = regMaybe clk rst 0 (plusM (pure <$> s) (sometimes1 clk rst))
  plusM = liftA2 (liftA2 (+))
:}

-}

-- **Clock
-- | Clock generator for the 'System' clock tag.
--
-- __NB__: should only be used for simulation, and __not__ for the /testBench/
-- function. For the /testBench/ function, used 'tbSystemClockGen'
systemClockGen
  :: Clock System 'Regular
systemClockGen = clockGen

-- | Clock generator for the 'System' clock tag.
--
-- __NB__: can be used in the /testBench/ function
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
--     done           = exposeClockReset (expectedOutput (topEntity <$> testInput)) clk rst
--     clk            = 'tbSystemClockGen' (not <\$\> done)
--     rst            = systemResetGen
-- @
tbSystemClockGen
  :: Signal System Bool
  -> Clock System 'Regular
tbSystemClockGen = tbClockGen

-- | Reset generator for the 'System' clock tag.
--
-- __NB__: should only be used for simulation or the \testBench\ function.
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
--     done           = exposeClockReset (expectedOutput (topEntity <$> testInput)) clk rst
--     clk            = tbSystemClockGen (not <\$\> done)
--     rst            = 'systemResetGen'
-- @
systemResetGen :: Reset System 'ActiveHigh
systemResetGen = resetGen

-- | Normally, asynchronous resets can be both asynchronously asserted and
-- de-asserted. Asynchronous de-assertion can induce meta-stability in the
-- component which is being reset. To ensure this doesn't happen,
-- 'resetSynchronizer' ensures that de-assertion of a reset happens
-- synchronously. Assertion of the reset remains asynchronous.
--
-- Note that asynchronous assertion does not induce meta-stability in the
-- component whose reset is asserted. However, when a component \"A\" in another
-- clock or reset tag depends on the value of a component \"B\" being
-- reset, then asynchronous assertion of the reset of component \"B"\ can induce
-- meta-stability in component \"A\". To prevent this from happening you need
-- to use a proper synchronizer, for example one of the synchronizers in
-- "Clash.Explicit.Synchronizer"
--
-- === __Example__
--
-- @
-- topEntity
--   :: Clock  System Regular
--   -> Reset  System ActiveHigh
--   -> Signal System Bit
--   -> Signal System (BitVector 8)
-- topEntity clk rst key1 =
--     let  (pllOut,pllStable) = altpll (SSymbol @"altpll50") clk rst
--          rstSync            = 'resetSynchronizer' pllOut (unsafeToActiveHighReset pllStable)
--     in   exposeClockReset leds pllOut rstSync
--   where
--     key1R  = isRising 1 key1
--     leds   = mealy blinkerT (1, False, 0) key1R
-- @
resetSynchronizer
  :: KnownDomain tag dom
  => Clock tag enabled
  -> Reset tag polarity
  -> Reset tag polarity
resetSynchronizer clk@(clockTag -> tag) rst@(toHighPolarity -> hRst) =
  case knownDomainByTag tag of
    SDomain _tag _period _edge SAsynchronous _init ->
      let r1 = register clk hRst True (pure False)
          r2 = register clk hRst True r1
      in  syncPolarity rst (unsafeToActiveHighReset r2)
    _ ->
      -- Reset is already synchronous, nothing to do!
      rst


-- | Calculate the period, in __ps__, given a frequency in __Hz__
--
-- i.e. to calculate the clock period for a circuit to run at 240 MHz we get
--
-- >>> freqCalc 240e6
-- 4167
--
-- __NB__: This function is /not/ synthesizable
freqCalc :: Double -> Integer
freqCalc freq = ceiling ((1.0 / freq) / 1.0e-12)

-- ** Synchronization primitive
-- | The 'unsafeSynchronizer' function is a primitive that must be used to
-- connect one clock domain to the other, and will be synthesized to a (bundle
-- of) wire(s) in the eventual circuit. This function should only be used as
-- part of a proper synchronization component, such as the following dual
-- flip-flop synchronizer:
--
-- @
-- dualFlipFlop
--   :: Clock domA enabledA
--   -> Clock domB enabledB
--   -> Bit
--   -> Signal tagA Bit
--   -> Signal tagB Bit
-- dualFlipFlop clkA clkB dflt = 'delay' clkB dflt . 'delay' clkB dflt
--                             . 'unsafeSynchronizer' clkA clkB
-- @
--
-- The 'unsafeSynchronizer' works in such a way that, given 2 clocks:
--
-- @
-- instance KnownDomain "Dom7" ('Domain "Dom7" 2 'Rising 'Asynchronous 'Defined) where
--   knownDomain = SDomain SSymbolSNat SRising SAsynchronous SDefined
--
-- clk7 :: 'Clock' "Dom7" Regular
-- clk7 = 'clockGen'
-- @
--
-- and
--
-- @
-- instance KnownDomain "Dom2" ('Domain "Dom2" 2 'Rising 'Asynchronous 'Defined) where
--   knownDomain = SDomain SSymbolSNat SRising SAsynchronous SDefined
--
-- clk2 :: 'Clock' "Dom2" Regular
-- clk2 = 'clockGen'
-- @
--
-- Oversampling followed by compression is the identity function plus 2 initial
-- values:
--
-- @
-- 'delay' clkB dflt $
-- 'unsafeSynchronizer' clkA clkB $
-- 'delay' clkA dflt $
-- 'unsafeSynchronizer' clkB clkA $
-- 'delay' clkB s
--
-- ==
--
-- dflt :- dflt :- s
-- @
--
-- Something we can easily observe:
--
-- @
-- oversampling clkA clkB dflt = 'delay' clkB dflt
--                             . 'unsafeSynchronizer' clkA clkB
--                             . 'delay' clkA dflt
-- almostId clkA clkB dflt = 'delay' clkB dflt
--                         . 'unsafeSynchronizer' clkA clkB
--                         . 'delay' clkA dflt
--                         . 'unsafeSynchronizer' clkB clkA
--                         . 'delay' clkB dflt
-- @
--
-- >>> sampleN 37 (oversampling clk7 clk2 0 (fromList [(1::Int)..10]))
-- [0,0,1,1,1,2,2,2,2,3,3,3,4,4,4,4,5,5,5,6,6,6,6,7,7,7,8,8,8,8,9,9,9,10,10,10,10]
-- >>> sampleN 12 (almostId clk2 clk7 0 (fromList [(1::Int)..10]))
-- [0,0,1,2,3,4,5,6,7,8,9,10]
unsafeSynchronizer
  :: KnownDomain tag1 conf1
  => KnownDomain tag2 conf2
  => Clock tag1 enabled1
  -- ^ 'Clock' of the incoming signal
  -> Clock tag2 enabled2
  -- ^ 'Clock' of the outgoing signal
  -> Signal tag1 a
  -> Signal tag2 a
unsafeSynchronizer clk1 clk2 s = s'
  where
    t1 = clockPeriod clk1
    t2 = clockPeriod clk2
    s' | t1 < t2   = compress   t2 t1 s
       | t1 > t2   = oversample t1 t2 s
       | otherwise = same s
{-# NOINLINE unsafeSynchronizer #-}

same :: Signal tag1 a -> Signal tag2 a
same (s :- ss) = s :- same ss

oversample :: Int -> Int -> Signal tag1 a -> Signal tag2 a
oversample high low (s :- ss) = s :- oversampleS (reverse (repSchedule high low)) ss

oversampleS :: [Int] -> Signal tag1 a -> Signal tag2 a
oversampleS sched = oversample' sched
  where
    oversample' []     s       = oversampleS sched s
    oversample' (d:ds) (s:-ss) = prefixN d s (oversample' ds ss)

    prefixN 0 _ s = s
    prefixN n x s = x :- prefixN (n-1) x s

compress :: Int -> Int -> Signal tag1 a -> Signal tag2 a
compress high low s = compressS (repSchedule high low) s

compressS :: [Int] -> Signal tag1 a -> Signal tag2 a
compressS sched = compress' sched
  where
    compress' []     s           = compressS sched s
    compress' (d:ds) ss@(s :- _) = s :- compress' ds (dropS d ss)

    dropS 0 s         = s
    dropS n (_ :- ss) = dropS (n-1) ss

repSchedule :: Int -> Int -> [Int]
repSchedule high low = take low $ repSchedule' low high 1
  where
    repSchedule' cnt th rep
      | cnt < th  = repSchedule' (cnt+low) th (rep + 1)
      | otherwise = rep : repSchedule' (cnt + low) (th + high) 1

-- * Basic circuit functions

-- | \"@'delay' clk s@\" delays the values in 'Signal' /s/ for once cycle, the
-- value at time 0 is /dflt/.
--
-- >>> sampleN 3 (delay systemClockGen 0 (fromList [1,2,3,4]))
-- [0,1,2]
delay
  :: Undefined a
  => Clock tag enabled
  -- ^ Clock
  -> a
  -- ^ Default value
  -> Signal tag a
  -> Signal tag a
delay = delay#
{-# INLINE delay #-}

-- | Version of 'delay' that only updates when its third argument is a 'Just'
-- value.
--
-- >>> let input = fromList [Just 1, Just 2, Nothing, Nothing, Just 5, Just 6, Just (7::Int)]
-- >>> sampleN 7 (delayMaybe systemClockGen 0 input)
-- [0,1,2,2,2,5,6]
delayMaybe
  :: Undefined a
  => Clock tag enabled
  -- ^ Clock
  -> a
  -- ^ Initial value
  -> Signal tag (Maybe a)
  -> Signal tag a
delayMaybe clk dflt i =
  delayEn clk dflt (isJust <$> i) (fromJust <$> i)
{-# INLINE delayMaybe #-}

-- | Version of 'delay' that only updates when its third argument is asserted.
--
-- >>> let input = fromList [1,2,3,4,5,6,7::Int]
-- >>> let enable = fromList [True,True,False,False,True,True,True]
-- >>> sampleN 7 (delayEn systemClockGen 0 enable input)
-- [0,1,2,2,2,5,6]
delayEn
  :: Undefined a
  => Clock tag enabled
  -- ^ Clock
  -> a
  -- ^ Initial value
  -> Signal tag Bool
  -- ^ Enable
  -> Signal tag a
  -> Signal tag a
delayEn clk dflt en i =
  delay (toEnabledClock clk en) dflt i
{-# INLINE delayEn #-}

-- | \"@'register' clk rst i s@\" delays the values in 'Signal' /s/ for one
-- cycle, and sets the value to @i@ the moment the reset becomes 'False'.
--
-- >>> sampleN 5 (register systemClockGen resetGen 8 (fromList [1,1,2,3,4]))
-- [8,8,1,2,3]
register
  :: ( KnownDomain tag dom
     , Undefined a )
  => Clock tag enabled
  -- ^ clock
  -> Reset tag polarity
  -- ^ Reset (active-high), 'register' outputs the reset value when the
  -- reset value becomes 'True'
  -> a
  -- ^ Reset value
  -> Signal tag a
  -> Signal tag a
register clk rst initial i =
  register# clk (toHighPolarity rst) initial initial i
{-# INLINE register #-}

-- | Version of 'register' that only updates its content when its fourth
-- argument is a 'Just' value. So given:
--
-- @
-- sometimes1 clk rst = s where
--   s = 'register' clk rst Nothing (switch '<$>' s)
--
--   switch Nothing = Just 1
--   switch _       = Nothing
--
-- countSometimes clk rst = s where
--   s     = 'regMaybe' clk rst 0 (plusM ('pure' '<$>' s) (sometimes1 clk rst))
--   plusM = liftA2 (liftA2 (+))
-- @
--
-- We get:
--
-- >>> sampleN 9 (sometimes1 systemClockGen resetGen)
-- [Nothing,Nothing,Just 1,Nothing,Just 1,Nothing,Just 1,Nothing,Just 1]
-- >>> sampleN 9 (count systemClockGen resetGen)
-- [0,0,0,1,1,2,2,3,3]
regMaybe
  :: ( KnownDomain tag dom
     , Undefined a )
  => Clock tag enabled
  -- ^ Clock
  -> Reset tag polarity
  -- ^ Reset (active-high), 'regMaybe' outputs the reset value when the
  -- reset value becomes 'True'
  -> a
  -- ^ Reset value
  -> Signal tag (Maybe a)
  -> Signal tag a
regMaybe clk rst initial iM =
  register (toEnabledClock clk (fmap isJust iM)) rst initial (fmap fromJust iM)
{-# INLINE regMaybe #-}

-- | Version of 'register' that only updates its content when its fourth
-- argument is asserted. So given:
--
-- @
-- oscillate clk rst = let s = 'register' clk rst False (not \<$\> s) in s
-- count clk rst     = let s = 'regEn clk rst 0 (oscillate clk rst) (s + 1) in s
-- @
--
-- We get:
--
-- >>> sampleN 9 (oscillate systemClockGen resetGen)
-- [False,False,True,False,True,False,True,False,True]
-- >>> sampleN 9 (count systemClockGen resetGen)
-- [0,0,0,1,1,2,2,3,3]
regEn
  :: ( KnownDomain tag dom
     , Undefined a
     )
  => Clock tag clk
  -- ^ Clock
  -> Reset tag polarity
  -- ^ Reset (active-high), 'regEn' outputs the reset value when the
  -- reset value becomes 'True'
  -> a
  -- ^ Reset value
  -> Signal tag Bool
  -- ^ Enable signal
  -> Signal tag a
  -> Signal tag a
regEn clk rst initial en i =
  register (toEnabledClock clk en) rst initial i
{-# INLINE regEn #-}

-- * Product/Signal isomorphism

-- | Simulate a (@'Unbundled' a -> 'Unbundled' b@) function given a list of
-- samples of type /a/
--
-- >>> simulateB (unbundle . register systemClockGen resetGen (8,8) . bundle) [(1,1), (1,1), (2,2), (3,3)] :: [(Int,Int)]
-- [(8,8),(8,8),(1,1),(2,2),(3,3)...
-- ...
--
-- __NB__: This function is not synthesizable
simulateB
  :: (Bundle a, Bundle b, Undefined a, Undefined b)
  => (Unbundled tag1 a -> Unbundled tag2 b)
  -- ^ The function we want to simulate
  -> [a]
  -- ^ Input samples
  -> [b]
simulateB f = simulate (bundle . f . unbundle)

-- | /Lazily/ simulate a (@'Unbundled' a -> 'Unbundled' b@) function given a
-- list of samples of type /a/
--
-- >>> simulateB (unbundle . register systemClockGen resetGen (8,8) . bundle) [(1,1), (1,1), (2,2), (3,3)] :: [(Int,Int)]
-- [(8,8),(8,8),(1,1),(2,2),(3,3)...
-- ...
--
-- __NB__: This function is not synthesizable
simulateB_lazy
  :: (Bundle a, Bundle b)
  => (Unbundled tag1 a -> Unbundled tag2 b)
  -- ^ The function we want to simulate
  -> [a]
  -- ^ Input samples
  -> [b]
simulateB_lazy f = simulate_lazy (bundle . f . unbundle)

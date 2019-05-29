{-|
Copyright  :  (C) 2013-2016, University of Twente,
                  2017-2019, Myrtle Software Ltd
                  2017,      Google Inc.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE ViewPatterns           #-}

{-# LANGUAGE Unsafe #-}

-- See: https://github.com/clash-lang/clash-compiler/commit/721fcfa9198925661cd836668705f817bddaae3c
-- as to why we need this.
{-# OPTIONS_GHC -fno-cpr-anal #-}

{-# OPTIONS_HADDOCK show-extensions not-home #-}

module Clash.Signal.Internal
  ( -- * Datatypes
    Signal(..)
  , head#
  , tail#
    -- * Domains
  , KnownDomain(..)
  , knownDomainByTag
  , ActiveEdge(..)
  , SActiveEdge(..)
  , InitBehavior(..)
  , SInitBehavior(..)
  , ResetKind(..)
  , SResetKind(..)
  , ResetPolarity(..)
  , SResetPolarity(..)
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
  , isSensitiveToRisingEdge
  , isAsynchronous
  , hasDefinedInitialValues
  , isActiveHigh
    -- * Clocks
  , Clock (..)
  , clockTag
  , clockPeriod
    -- ** Enabling
  , Enable(..)
  , toEnable
  , fromEnable
  , enableGen
    -- * Resets
  , Reset(..)
  , fromSyncReset
  , unsafeToReset
  , unsafeFromReset
  , unsafeToHighPolarity
  , unsafeToLowPolarity
  , unsafeFromHighPolarity
  , unsafeFromLowPolarity
    -- * Basic circuits
  , delay#
  , register#
  , mux
    -- * Simulation and testbench functions
  , clockGen
  , tbClockGen
  , resetGen
    -- * Boolean connectives
  , (.&&.), (.||.)
    -- * Simulation functions (not synthesizable)
  , simulate
    -- ** lazy version
  , simulate_lazy
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
    -- ** 'Functor'
  , mapSignal#
    -- ** 'Applicative'
  , signal#
  , appSignal#
    -- ** 'Foldable'
  , foldr#
    -- ** 'Traversable'
  , traverse#
  -- * EXTREMELY EXPERIMENTAL
  , joinSignal#
  )
where

import Type.Reflection            (Typeable)
import Control.Applicative        (liftA2, liftA3)
import Control.DeepSeq            (NFData)
import Clash.Annotations.Primitive (hasBlackBox)
import Data.Coerce                (coerce)
import Data.Data                  (Data)
import Data.Default.Class         (Default (..))
import Data.Hashable              (Hashable)
import GHC.Generics               (Generic)
import GHC.TypeLits               (KnownSymbol, Nat, Symbol)
import Language.Haskell.TH.Syntax -- (Lift (..), Q, Dec)
import Test.QuickCheck            (Arbitrary (..), CoArbitrary(..), Property,
                                   property)

import Clash.Promoted.Nat         (SNat (..), snatToNum, snatToInteger)
import Clash.Promoted.Symbol      (SSymbol (..), ssymbolToString)
import Clash.XException           (Undefined, errorX, deepseqX, defaultSeqX)

{- $setup
>>> :set -XDataKinds
>>> :set -XMagicHash
>>> :set -XTypeApplications
>>> import Clash.Promoted.Nat
>>> import Clash.XException
>>> type System = "System"
>>> let systemClockGen = clockGen @System
>>> let systemResetGen = resetGen @System
>>> import Clash.Explicit.Signal (register)
>>> let registerS = register
>>> let registerA = register
-}


-- * Signal
data ActiveEdge
  = Rising
  -- ^ Elements are sensitive to the rising edge (low-to-high) of the clock.
  | Falling
  -- ^ Elements are sensitive to the falling edge (high-to-low) of the clock.
  deriving (Show, Eq, Ord, Generic, NFData, Data, Hashable)

data SActiveEdge (edge :: ActiveEdge) where
  SRising  :: SActiveEdge 'Rising
  -- See 'Rising' ^
  SFalling :: SActiveEdge 'Falling
  -- See 'Falling' ^

instance Show (SActiveEdge edge) where
  show SRising = "SRising"
  show SFalling = "SFalling"

data ResetKind
  = Asynchronous
  -- ^ Elements respond /asynchronously/ to changes in their reset input. This
  -- means that they do /not/ wait for the next active clock edge, but respond
  -- immediately instead. Common on Intel FPGA platforms.
  | Synchronous
  -- ^ Elements respond /synchronously/ to changes in their reset input. This
  -- means that changes in their reset input won't take effect until the next
  -- active clock edge. Common on Xilinx FPGA platforms.
  deriving (Show, Eq, Ord, Generic, NFData, Data, Hashable)

-- | GADT version of 'ResetKind'
data SResetKind (resetKind :: ResetKind) where
  SAsynchronous :: SResetKind 'Asynchronous
  -- See 'Asynchronous' ^

  SSynchronous  :: SResetKind 'Synchronous
  -- See 'Synchronous' ^

instance Show (SResetKind reset) where
  show SAsynchronous = "SAsynchronous"
  show SSynchronous = "SSynchronous"

-- | Determines the value for which a reset line is considered "active"
data ResetPolarity
  = ActiveHigh
  -- ^ Reset is considered active if underlying signal is 'True'.
  | ActiveLow
  -- ^ Reset is considered active if underlying signal is 'False'.
  deriving (Eq, Ord, Show, Generic, NFData, Data, Hashable)

-- | GADT version of 'ResetPolarity'
data SResetPolarity (polarity :: ResetPolarity) where
  SActiveHigh :: SResetPolarity 'ActiveHigh
  -- See: 'ActiveHigh' ^

  SActiveLow :: SResetPolarity 'ActiveLow
  -- See: 'ActiveLow' ^

instance Show (SResetPolarity polarity) where
  show SActiveHigh = "SActiveHigh"
  show SActiveLow = "SActiveLow"

data InitBehavior
  = Undefined
  -- ^ Power up value of memory elements is /undefined/.
  | Defined
  -- ^ If applicable, power up value of a memory element is defined. Applies to
  -- 'register's for example, but not to 'blockRam'.
  deriving (Show, Eq, Ord, Generic, NFData, Data, Hashable)

data SInitBehavior (init :: InitBehavior) where
  SUndefined :: SInitBehavior 'Undefined
  -- See: 'Undefined' ^

  SDefined :: SInitBehavior 'Defined
  -- See: 'Defined' ^

instance Show (SInitBehavior init) where
  show SUndefined = "SUndefined"
  show SDefined = "SDefined"

-- | A domain with a name (@Symbol@). Configures the behavior of various aspects
-- of a circuits. See the documentation of this record's field types for more
-- information on the options. See the module documentation of 'Clash.Signal'
-- or 'KnownDomain' for more information on how to /use/ domains in your design.
data Domain
  = Domain
  { _tag :: Symbol
  -- ^ Domain name
  , _period :: Nat
  -- ^ Period of clock in /ps/
  , _edge :: ActiveEdge
  -- ^ Determines which edge of the clock registers are sensitive to
  , _reset :: ResetKind
  -- ^ Determines how components with reset lines respond to changes
  , _init :: InitBehavior
  -- ^ Determines the initial (or "power up") value of various components
  , _polarity :: ResetPolarity
  -- ^ Determines whether resets are active high or active low
  }
  deriving (Typeable)

-- | GADT version of 'Domain'
data SDomain (tag :: Symbol) (dom :: Domain) where
  SDomain
    :: SSymbol tag
    -- Domain name ^
    -> SNat period
    -- Period of clock in /ps/ ^
    -> SActiveEdge edge
    -- Determines which edge of the clock registers are sensitive to ^
    -> SResetKind reset
    -- Determines how components with reset lines respond to changes ^
    -> SInitBehavior init
    -- Determines the initial (or "power up") value of various components ^
    -> SResetPolarity polarity
    -- Determines whether resets are active high or active low ^
    -> SDomain tag ('Domain tag period edge reset init polarity)

instance Show (SDomain tag dom) where
  show (SDomain tag period edge reset init_ polarity) =
    unwords
      [ "SDomain"
      , show tag
      , show period
      , show edge
      , show reset
      , show init_
      , show polarity
      ]

-- | A 'KnownDomain' constraint indicates that a circuit's behavior depends on
-- some properties of a domain. See 'Domain' for more information.
class KnownSymbol tag => KnownDomain (tag :: Symbol) (dom :: Domain) | tag -> dom where
  -- | Returns 'SDomain' corresponding to an instance's 'Domain'.
  --
  -- Example usage:
  -- > knownDomain @System
  --
  knownDomain :: SDomain tag dom

--class KnownActiveEdge (tag :: Symbol) (edge :: ActiveEdge) | tag -> edge where
--class KnownResetKind (tag :: Symbol) (reset :: ResetKind) | tag -> reset where
--class KnownInitBehavior (tag :: Symbol) (init :: InitBehavior) | tag -> init where
--class KnownResetPolarity (tag :: Symbol) (polarity :: ResetPolarity) | tag -> polarity where

-- | Whether domain's memory elements are sensitive to a rising edge
isSensitiveToRisingEdge
  :: forall tag dom
   . KnownDomain tag dom
  => Bool
isSensitiveToRisingEdge =
  case knownDomain @tag of
    SDomain _tag _period SRising _sync _init _polarity -> True
    SDomain _tag _period SFalling _sync _init _polarity -> False
{-# INLINE isSensitiveToRisingEdge #-}

-- | Whether domain has asynchronous resets
isAsynchronous
  :: forall tag dom
   . KnownDomain tag dom
  => Bool
isAsynchronous =
  case knownDomain @tag of
    SDomain _tag _period _edge SAsynchronous _init _polarity -> True
    SDomain _tag _period _edge SSynchronous _init _polarity -> False
{-# INLINE isAsynchronous #-}

-- | Whether domain has defined initial values
hasDefinedInitialValues
  :: forall tag dom
   . KnownDomain tag dom
  => Bool
hasDefinedInitialValues =
  case knownDomain @tag of
    SDomain _tag _period _edge _sync SDefined _polarity -> True
    SDomain _tag _period _edge _sync SUndefined _polarity -> False
{-# INLINE hasDefinedInitialValues #-}

-- | Whether resets are active high
isActiveHigh
  :: forall tag dom
   . KnownDomain tag dom
  => Bool
isActiveHigh =
  case knownDomain @tag of
    SDomain _tag _period _edge _sync _init SActiveHigh -> True
    SDomain _tag _period _edge _sync _init SActiveLow -> False
{-# INLINE isActiveHigh #-}

-- | Version of 'knownDomain accepts a SSymbol. For example:
--
-- >>> knownDomainByTag (SSymbol @"System")
-- SDomain System d10000 SRising SAsynchronous SDefined SActiveHigh
knownDomainByTag
  :: forall tag dom
   . KnownDomain tag dom
  => SSymbol tag
  -> SDomain tag dom
knownDomainByTag =
  const knownDomain
{-# INLINE knownDomainByTag #-}

-- | A /clock/ (and /reset/) tag with clocks running at 100 MHz
instance KnownDomain System ('Domain System 10000 'Rising 'Asynchronous 'Defined 'ActiveHigh) where
  knownDomain = SDomain SSymbol SNat SRising SAsynchronous SDefined SActiveHigh

-- | System instance with defaults set for Xilinx FPGAs
instance KnownDomain XilinxSystem ('Domain XilinxSystem 10000 'Rising 'Synchronous 'Defined 'ActiveHigh) where
  knownDomain = SDomain SSymbol SNat SRising SSynchronous SDefined SActiveHigh

-- | System instance with defaults set for Intel FPGAs
instance KnownDomain IntelSystem ('Domain IntelSystem 10000 'Rising 'Asynchronous 'Defined 'ActiveHigh) where
  knownDomain = SDomain SSymbol SNat SRising SAsynchronous SDefined SActiveHigh

-- | Convenience value to allow easy "subclassing" of System domain. Should
-- be used in combination with 'createDomain'. For example, if you just want to
-- change the period but leave all other settings in tact use:
--
-- > $(createDomain vSystem{vTag="System10", vPeriod=10})
--
vSystem :: VDomain
vSystem = vDomain (knownDomain @System)
type System = "System"


-- | Convenience value to allow easy "subclassing" of IntelSystem domain. Should
-- be used in combination with 'createDomain'. For example, if you just want to
-- change the period but leave all other settings in tact use:
--
-- > $(createDomain vIntelSystem{vTag="Intel10", vPeriod=10})
--
vIntelSystem :: VDomain
vIntelSystem = vDomain (knownDomain @IntelSystem)
type IntelSystem = "IntelSystem"

-- | Convenience value to allow easy "subclassing" of XilinxSystem domain. Should
-- be used in combination with 'createDomain'. For example, if you just want to
-- change the period but leave all other settings in tact use:
--
-- > $(createDomain vXilinxSystem{vTag="Xilinx10", vPeriod=10})
--
vXilinxSystem :: VDomain
vXilinxSystem = vDomain (knownDomain @XilinxSystem)
type XilinxSystem = "XilinxSystem"

-- | Same as SDomain but allows for easy updates through record update syntax.
-- Should be used in combination with 'vDomain' and 'createDomain'. Example:
--
-- > $(createDomain (knownVDomain @System){vTag="System10", vPeriod=10})
--
-- This duplicates the settings in the "System" domain, replaces the name and
-- period, and creates an instance for it. As most users often want to update
-- the system domain, a shortcut is available in the form:
--
-- > $(createDomain vSystem{vTag="System10", vPeriod=10})
--
data VDomain
  = VDomain
  { vTag    :: String
  -- ^ Corresponds to '_tag' on 'Domain'
  , vPeriod :: Integer
  -- ^ Corresponds to '_period' on 'Domain'
  , vEdge   :: ActiveEdge
  -- ^ Corresponds to '_edge' on 'Domain'
  , vReset  :: ResetKind
  -- ^ Corresponds to '_reset' on 'Domain'
  , vInit   :: InitBehavior
  -- ^ Corresponds to '_init' on 'Domain'
  , vPolarity :: ResetPolarity
  -- ^ Corresponds to '_polarity' on 'Domain'
  }

-- | Like 'knownDomain but yields a 'VDomain'. Should only be used in
-- combination with 'createDomain'.
knownVDomain
  :: forall tag dom
   . KnownDomain tag dom
  => VDomain
knownVDomain =
  vDomain (knownDomain @tag)

-- | Convert 'SDomain' to 'VDomain'. Should be used in combination with
-- 'createDomain' only.
vDomain :: SDomain tag dom -> VDomain
vDomain (SDomain tag period edge reset init_ polarity) =
  VDomain
    (ssymbolToString tag)
    (snatToInteger period)
    (case edge of {SRising -> Rising; SFalling -> Falling})
    (case reset of {SAsynchronous -> Asynchronous; SSynchronous -> Synchronous})
    (case init_ of {SDefined -> Defined; SUndefined -> Undefined})
    (case polarity of {SActiveHigh -> ActiveHigh; SActiveLow -> ActiveLow})

-- | Convenience method to express new domains in terms of others.
--
-- > $(createDomain (knownVDomain @System){vTag="System10", vPeriod=10})
--
-- This duplicates the settings in the "System" domain, replaces the name and
-- period, and creates an instance for it. As most users often want to update
-- the system domain, a shortcut is available in the form:
--
-- > $(createDomain vSystem{vTag="System10", vPeriod=10})
--
createDomain :: VDomain -> Q [Dec]
createDomain (VDomain tag period edge reset init_ polarity) = do
  kdType <- [t| KnownDomain $tagT ('Domain $tagT $periodT $edgeT $resetKindT $initT $polarityT ) |]
  sDom <- [| SDomain SSymbol SNat $edgeE $resetKindE $initE $polarityE |]
  let kdImpl = FunD 'knownDomain [Clause [] (NormalB sDom) []]
  pure  [InstanceD Nothing [] kdType [kdImpl]]
 where

  edgeE =
    pure $
    case edge of
      Rising -> ConE 'SRising
      Falling -> ConE 'SFalling

  resetKindE =
    pure $
    case reset of
      Asynchronous -> ConE 'SAsynchronous
      Synchronous -> ConE 'SSynchronous

  initE =
    pure $
    case init_ of
      Undefined -> ConE 'SUndefined
      Defined -> ConE 'SDefined

  polarityE =
    pure $
    case polarity of
      ActiveHigh -> ConE 'SActiveHigh
      ActiveLow -> ConE 'SActiveLow

  tagT    = pure (LitT (StrTyLit tag))
  periodT = pure (LitT (NumTyLit period))

  edgeT =
    pure $
    case edge of
      Rising -> PromotedT 'Rising
      Falling -> PromotedT 'Falling

  resetKindT =
    pure $
    case reset of
      Asynchronous -> PromotedT 'Asynchronous
      Synchronous -> PromotedT 'Synchronous

  initT =
    pure $
    case init_ of
      Undefined -> PromotedT 'Undefined
      Defined -> PromotedT 'Defined

  polarityT =
    pure $
    case polarity of
      ActiveHigh -> PromotedT 'ActiveHigh
      ActiveLow -> PromotedT 'ActiveLow


infixr 5 :-
{- | Clash has synchronous 'Signal's in the form of:

@
'Signal' (tag :: 'Symbol') a
@

Where /a/ is the type of the value of the 'Signal', for example /Int/ or /Bool/,
and /tag/ is the /clock-/ (and /reset-/) domain to which the memory elements
manipulating these 'Signal's belong.

The type-parameter, /tag/, is of the kind 'Symbol' - a simple string. That
string refers to a single /synthesis domain/. A synthesis domain describes the
behavior of certain aspects of memory elements in it.

See the module documentation of 'Clash.Signal' for more information about
domains.
-}
data Signal (tag :: Symbol) a
  -- | The constructor, @(':-')@, is __not__ synthesizable.
  = a :- Signal tag a

head# :: Signal tag a -> a
head# (x' :- _ )  = x'

tail# :: Signal tag a -> Signal tag a
tail# (_  :- xs') = xs'

instance Show a => Show (Signal tag a) where
  show (x :- xs) = show x ++ " " ++ show xs

instance Lift a => Lift (Signal tag a) where
  lift ~(x :- _) = [| signal# x |]

instance Default a => Default (Signal tag a) where
  def = signal# def

instance Functor (Signal tag) where
  fmap = mapSignal#

mapSignal# :: (a -> b) -> Signal tag a -> Signal tag b
mapSignal# f (a :- as) = f a :- mapSignal# f as
{-# NOINLINE mapSignal# #-}
{-# ANN mapSignal# hasBlackBox #-}

instance Applicative (Signal tag) where
  pure  = signal#
  (<*>) = appSignal#

signal# :: a -> Signal tag a
signal# a = let s = a :- s in s
{-# NOINLINE signal# #-}
{-# ANN signal# hasBlackBox #-}

appSignal# :: Signal tag (a -> b) -> Signal tag a -> Signal tag b
appSignal# (f :- fs) xs@(~(a :- as)) = f a :- (xs `seq` appSignal# fs as) -- See [NOTE: Lazy ap]
{-# NOINLINE appSignal# #-}
{-# ANN appSignal# hasBlackBox #-}

{- NOTE: Lazy ap
Signal's ap, i.e (Applicative.<*>), must be lazy in it's second argument:

> appSignal :: Signal' clk (a -> b) -> Signal' clk a -> Signal' clk b
> appSignal (f :- fs) ~(a :- as) = f a :- appSignal fs as

because some feedback loops, such as the loop described in 'system' in the
example at http://hackage.haskell.org/package/clash-prelude-0.10.10/docs/Clash-Prelude-BlockRam.html,
will lead to "Exception <<loop>>".

However, this "naive" lazy version is _too_ lazy and induces spaceleaks.
The current version:

> appSignal# :: Signal' clk (a -> b) -> Signal' clk a -> Signal' clk b
> appSignal# (f :- fs) xs@(~(a :- as)) = f a :- (xs `seq` appSignal# fs as)

Is lazy enough to handle the earlier mentioned feedback loops, but doesn't leak
(as much) memory like the "naive" lazy version, because the Signal constructor
of the second argument is evaluated as soon as the tail of the result is evaluated.
-}


-- | __WARNING: EXTREMELY EXPERIMENTAL__
--
-- The circuit semantics of this operation are unclear and/or non-existent.
-- There is a good reason there is no 'Monad' instance for 'Signal''.
--
-- Is currently treated as 'id' by the Clash compiler.
joinSignal# :: Signal tag (Signal tag a) -> Signal tag a
joinSignal# ~(xs :- xss) = head# xs :- joinSignal# (mapSignal# tail# xss)
{-# NOINLINE joinSignal# #-}
{-# ANN joinSignal# hasBlackBox #-}

instance Num a => Num (Signal tag a) where
  (+)         = liftA2 (+)
  (-)         = liftA2 (-)
  (*)         = liftA2 (*)
  negate      = fmap negate
  abs         = fmap abs
  signum      = fmap signum
  fromInteger = signal# . fromInteger

-- | __NB__: Not synthesizable
--
-- __NB__: In \"@'foldr' f z s@\":
--
-- * The function @f@ should be /lazy/ in its second argument.
-- * The @z@ element will never be used.
instance Foldable (Signal tag) where
  foldr = foldr#

-- | __NB__: Not synthesizable
--
-- __NB__: In \"@'foldr#' f z s@\":
--
-- * The function @f@ should be /lazy/ in its second argument.
-- * The @z@ element will never be used.
foldr# :: (a -> b -> b) -> b -> Signal tag a -> b
foldr# f z (a :- s) = a `f` (foldr# f z s)
{-# NOINLINE foldr# #-}
{-# ANN foldr# hasBlackBox #-}

instance Traversable (Signal tag) where
  traverse = traverse#

traverse# :: Applicative f => (a -> f b) -> Signal tag a -> f (Signal tag b)
traverse# f (a :- s) = (:-) <$> f a <*> traverse# f s
{-# NOINLINE traverse# #-}
{-# ANN traverse# hasBlackBox #-}

-- * Clocks, resets, and enables

-- | A signal of booleans, indicating whether a component is enabled. No special
-- meaning is implied, it's up to the component itself to decide how to respond
-- to its enable line. It is used throughout Clash as a global enable signal.
newtype Enable tag = Enable (Signal tag Bool)

-- | Convert 'Enable' construct to its underlying representation: a signal of
-- bools.
fromEnable :: Enable tag -> Signal tag Bool
fromEnable = coerce
{-# INLINE fromEnable #-}

-- | Convert a signal of bools to an 'Enable' construct
toEnable :: Signal tag Bool -> Enable tag
toEnable = coerce
{-# INLINE toEnable #-}

-- | Enable generator for some domain. Is simply always True.
enableGen :: Enable tag
enableGen = toEnable (pure True)

-- | A clock signal belonging to a domain named /tag/.
data Clock (tag :: Symbol) = Clock (SSymbol tag)

instance Show (Clock tag) where
  show (Clock tag) = "<Clock: " ++ show tag ++ ">"

-- | Get the clock period of a 'Clock' (in /ps/) as a 'Num'
clockPeriod
  :: forall tag dom a
   . KnownDomain tag dom
  => Num a
  => Clock tag
  -> a
clockPeriod (Clock _) =
  case knownDomain @tag of
    SDomain _tag period _edge _reset _init _polarity ->
      snatToNum period

-- | Extract tag symbol from Clock
clockTag
  :: Clock tag
  -> SSymbol tag
clockTag (Clock tag) = tag

-- | Clock generator for simulations. Do __not__ use this clock generator for
-- for the /testBench/ function, use 'tbClockGen' instead.
--
-- To be used like:
--
-- @
-- clkSystem = clockGen @System
-- @
--
-- See 'Domain' for more information on how to use synthesis domains.
clockGen
  :: KnownDomain tag dom
  => Clock tag
clockGen = Clock SSymbol
{-# NOINLINE clockGen #-}
{-# ANN clockGen hasBlackBox #-}

-- | Clock generator to be used in the /testBench/ function.
--
-- To be used like:
--
-- @
-- clkSystem en = tbClockGen @System en
-- @
--
-- === __Example__
--
-- @
-- -- Fast domain: twice as fast as "Slow"
-- instance KnownDomain "Fast" ('Domain "Fast" 1 'Rising 'Asynchronous 'Defined 'ActiveHigh) where
--   knownDomain = SDomain SSymbol SNat SRising SAsynchronous SDefined SActiveHigh
--
-- -- Slow domain: twice as slow as "Fast"
-- instance KnownDomain "Slow" ('Domain "Slow" 2 'Rising 'Asynchronous 'Defined 'ActiveHigh) where
--   knownDomain = SDomain SSymbol SNat SRising SAsynchronous SDefined SActiveHigh
-- 
-- topEntity
--   :: Clock "Fast" Regular
--   -> Reset "Fast" Asynchronous
--   -> Clock "Slow" Regular
--   -> Signal "Fast" (Unsigned 8)
--   -> Signal "Slow" (Unsigned 8, Unsigned 8)
-- topEntity clk1 rst1 clk2 i =
--   let h = register clk1 rst1 0 (register clk1 rst1 0 i)
--       l = register clk1 rst1 0 i
--   in  unsafeSynchronizer clk1 clk2 (bundle (h,l))
--
-- testBench
--   :: Signal "Slow" Bool
-- testBench = done
--   where
--     testInput      = stimuliGenerator clkA1 rstA1 $(listToVecTH [1::Unsigned 8,2,3,4,5,6,7,8])
--     expectedOutput = outputVerifier   clkB2 rstB2 $(listToVecTH [(0,0) :: (Unsigned 8, Unsigned 8),(1,2),(3,4),(5,6),(7,8)])
--     done           = expectedOutput (topEntity clkA1 rstA1 clkB2 testInput)
--     done'          = not \<$\> done
--     clkA1          = 'tbClockGen' \@"Fast" (unsafeSynchronizer clkB2 clkA1 done')
--     clkB2          = 'tbClockGen' \@"Slow" done'
--     rstA1          = resetGen \@"Fast"
--     rstB2          = resetGen \@"Slow"
-- @
tbClockGen
  :: KnownDomain tag dom
  => Signal tag Bool
  -> Clock tag
tbClockGen _clk = clockGen
{-# NOINLINE tbClockGen #-}
{-# ANN tbClockGen hasBlackBox #-}

-- | Asynchronous reset generator, for simulations and the /testBench/ function.
--
-- To be used like:
--
-- @
-- rstSystem = resetGen @System
-- @
--
-- See 'clockGen' for example usage.
--
resetGen
  :: KnownDomain tag dom
  => Reset tag
resetGen = Reset SSymbol (True :- pure False)
{-# NOINLINE resetGen #-}
{-# ANN resetGen hasBlackBox #-}

-- | A reset signal belonging to a domain called /tag/.
--
-- The underlying representation of resets is 'Bool'.
data Reset tag = Reset (SSymbol tag) (Signal tag Bool)

-- | Convert a reset to an active high reset. Has no effect if reset is already
-- an active high reset. Is unsafe because it can introduce:
--
-- * <Clash-Explicit-Signal.html#metastability meta-stability>
unsafeToHighPolarity
  :: forall tag dom
   . KnownDomain tag dom
  => Reset tag
  -> Signal tag Bool
unsafeToHighPolarity (unsafeFromReset -> r) =
  if isActiveHigh @tag then r else not <$> r
{-# INLINE unsafeToHighPolarity #-}

-- | Convert a reset to an active low reset. Has no effect if reset is already
-- an active low reset. It is unsafe because it can introduce:
--
-- * <Clash-Explicit-Signal.html#metastability meta-stability>
unsafeToLowPolarity
  :: forall tag dom
   . KnownDomain tag dom
  => Reset tag
  -> Signal tag Bool
unsafeToLowPolarity (unsafeFromReset -> r) =
  if isActiveHigh @tag then not <$> r else r
{-# INLINE unsafeToLowPolarity #-}

-- | 'unsafeFromReset' is unsafe because it can introduce:
--
-- * <Clash-Explicit-Signal.html#metastability meta-stability>
--
-- when used in combination with an asynchronous reset. Use 'fromReset' if
-- you're using a synchronous one.
unsafeFromReset
  :: Reset tag
  -> Signal tag Bool
unsafeFromReset (Reset _tag r) = r
{-# NOINLINE unsafeFromReset #-}
{-# ANN unsafeFromReset hasBlackBox #-}

-- | It is safe to treat synchronous resets as @Bool@ signals
fromSyncReset
  :: KnownDomain tag ('Domain tag _period _edge 'Synchronous _init polarity)
  => Reset tag
  -> Signal tag Bool
fromSyncReset = unsafeFromReset
{-# INLINE fromSyncReset #-}

-- | 'unsafeToReset' is unsafe. For asynchronous resets it is unsafe
-- because it can introduce combinatorial loops. In case of synchronous resets
-- it can lead to <Clash-Explicit-Signal.html#metastability meta-stability>
-- issues in the presence of asynchronous resets.
--
-- NB: This function does nothign to
unsafeToReset
  :: KnownDomain tag dom
  => Signal tag Bool
  -> Reset tag
unsafeToReset r = Reset SSymbol r
{-# NOINLINE unsafeToReset #-}
{-# ANN unsafeToReset hasBlackBox #-}

-- | Interpret a signal of bools as an active high reset and convert it to
-- a reset signal corresponding to the domain's setting.
unsafeFromHighPolarity
  :: forall tag dom
   . KnownDomain tag dom
  => Signal tag Bool
  -- ^ Reset signal that's 'True' when active, and 'False' when inactive.
  -> Reset tag
unsafeFromHighPolarity r =
  unsafeToReset (if isActiveHigh @tag then r else not <$> r)

-- | Interpret a signal of bools as an active low reset and convert it to
-- a reset signal corresponding to the domain's setting.
unsafeFromLowPolarity
  :: forall tag dom
   . KnownDomain tag dom
  => Signal tag Bool
  -- ^ Reset signal that's 'False' when active, and 'True' when inactive.
  -> Reset tag
unsafeFromLowPolarity r =
  unsafeToReset (if isActiveHigh @tag then not <$> r else r)


infixr 2 .||.
-- | The above type is a generalisation for:
--
-- @
-- __(.||.)__ :: 'Clash.Signal.Signal' 'Bool' -> 'Clash.Signal.Signal' 'Bool' -> 'Clash.Signal.Signal' 'Bool'
-- @
--
-- It is a version of ('||') that returns a 'Clash.Signal.Signal' of 'Bool'
(.||.) :: Applicative f => f Bool -> f Bool -> f Bool
(.||.) = liftA2 (||)

infixr 3 .&&.
-- | The above type is a generalisation for:
--
-- @
-- __(.&&.)__ :: 'Clash.Signal.Signal' 'Bool' -> 'Clash.Signal.Signal' 'Bool' -> 'Clash.Signal.Signal' 'Bool'
-- @
--
-- It is a version of ('&&') that returns a 'Clash.Signal.Signal' of 'Bool'
(.&&.) :: Applicative f => f Bool -> f Bool -> f Bool
(.&&.) = liftA2 (&&)

-- [Note: register strictness annotations]
--
-- In order to produce the first (current) value of the register's output
-- signal, 'o', we don't need to know the shape of either input (enable or
-- value-in).  This is important, because both values might be produced from
-- the output in a feedback loop, so we can't know their shape (pattern
-- match) them until we have produced output.
--
-- Thus, we use lazy pattern matching to delay inspecting the shape of
-- either argument until output has been produced.
--
-- However, both arguments need to be evaluated to WHNF as soon as possible
-- to avoid a space-leak.  Below, we explicitly reduce the value-in signal
-- using 'seq' as the tail of our output signal is produced.  On the other
-- hand, because the value of the tail depends on the value of the enable
-- signal 'e', it will be forced by the 'if'/'then' statement and we don't
-- need to 'seq' it explicitly.

delay#
  :: forall tag a dom
   . ( KnownDomain tag dom
     , Undefined a )
  => Clock tag
  -> Enable tag
  -> a
  -> Signal tag a
  -> Signal tag a
delay# (Clock _tag) (fromEnable -> en) dflt =
    go dflt en
  where
    go o (e :- es) as@(~(x :- xs)) =
      let o' = if e then x else o
      -- See [Note: register strictness annotations]
      in  o `defaultSeqX` o :- (as `seq` go o' es xs)
{-# NOINLINE delay# #-}
{-# ANN delay# hasBlackBox #-}

-- | A register with a power up and reset value. Power up values are not
-- supported on all platforms, please consult the manual of your target platform
-- and check the notes below.
--
-- Xilinx: power up values and reset values MUST be the same. If they are not,
-- the Xilinx tooling __will ignore the reset value__ and use the power up value
-- instead. Source: MIA
--
-- Intel: power up values and reset values MUST be the same. If they are not,
-- the Intel tooling __will ignore the power up value__ and use the reset value
-- instead. Source: https://www.intel.com/content/www/us/en/programmable/support/support-resources/knowledge-base/solutions/rd01072011_91.html
register#
  :: forall tag dom a
   . ( KnownDomain tag dom
     , Undefined a )
  => Clock tag
  -> Reset tag
  -> Enable tag
  -> a
  -- ^ Power up value
  -> a
  -- ^ Reset value
  -> Signal tag a
  -> Signal tag a
register# (Clock tag) rst (fromEnable -> ena) powerUpVal resetVal =
  case knownDomainByTag tag of
    SDomain _tag _period _edge SSynchronous _init _polarity ->
      goSync powerUpVal (unsafeFromReset rst) ena
    SDomain _tag _period _edge SAsynchronous _init _polarity ->
      goAsync powerUpVal (unsafeFromReset rst) ena
 where
  goSync
    :: a
    -> Signal tag Bool
    -> Signal tag Bool
    -> Signal tag a
    -> Signal tag a
  goSync o rt@(~(r :- rs)) enas@(~(e :- es)) as@(~(x :- xs)) =
    let oE = if e then x else o
        oR = if r then resetVal else oE
        -- [Note: register strictness annotations]
    in  o `defaultSeqX` o :- (rt `seq` enas `seq` as `seq` goSync oR rs es xs)

  goAsync
    :: a
    -> Signal tag Bool
    -> Signal tag Bool
    -> Signal tag a
    -> Signal tag a
  goAsync o (r :- rs) enas@(~(e :- es)) as@(~(x :- xs)) =
    let oR = if r then resetVal else o
        oE = if r then resetVal else (if e then x else o)
        -- [Note: register strictness annotations]
    in  oR `defaultSeqX` oR :- (as `seq` enas `seq` goAsync oE rs es xs)
{-# NOINLINE register# #-}
{-# ANN register# hasBlackBox #-}

-- | The above type is a generalisation for:
--
-- @
-- __mux__ :: 'Clash.Signal.Signal' 'Bool' -> 'Clash.Signal.Signal' a -> 'Clash.Signal.Signal' a -> 'Clash.Signal.Signal' a
-- @
--
-- A multiplexer. Given "@'mux' b t f@", output @t@ when @b@ is 'True', and @f@
-- when @b@ is 'False'.
mux :: Applicative f => f Bool -> f a -> f a -> f a
mux = liftA3 (\b t f -> if b then t else f)
{-# INLINE mux #-}

infix 4 .==.
-- | The above type is a generalisation for:
--
-- @
-- __(.==.)__ :: 'Eq' a => 'Clash.Signal.Signal' a -> 'Clash.Signal.Signal' a -> 'Clash.Signal.Signal' 'Bool'
-- @
--
-- It is a version of ('==') that returns a 'Clash.Signal.Signal' of 'Bool'
(.==.) :: (Eq a, Applicative f) => f a -> f a -> f Bool
(.==.) = liftA2 (==)

infix 4 ./=.
-- | The above type is a generalisation for:
--
-- @
-- __(./=.)__ :: 'Eq' a => 'Clash.Signal.Signal' a -> 'Clash.Signal.Signal' a -> 'Clash.Signal.Signal' 'Bool'
-- @
--
-- It is a version of ('/=') that returns a 'Clash.Signal.Signal' of 'Bool'
(./=.) :: (Eq a, Applicative f) => f a -> f a -> f Bool
(./=.) = liftA2 (/=)

infix 4 .<.
-- | The above type is a generalisation for:
--
-- @
-- __(.<.)__ :: 'Ord' a => 'Clash.Signal.Signal' a -> 'Clash.Signal.Signal' a -> 'Clash.Signal.Signal' 'Bool'
-- @
--
-- It is a version of ('<') that returns a 'Clash.Signal.Signal' of 'Bool'
(.<.) :: (Ord a, Applicative f) => f a -> f a -> f Bool
(.<.) = liftA2 (<)

infix 4 .<=.
-- | The above type is a generalisation for:
--
-- @
-- __(.<=.)__ :: 'Ord' a => 'Clash.Signal.Signal' a -> 'Clash.Signal.Signal' a -> 'Clash.Signal.Signal' 'Bool'
-- @
--
-- It is a version of ('<=') that returns a 'Clash.Signal.Signal' of 'Bool'
(.<=.) :: (Ord a, Applicative f) => f a -> f a -> f Bool
(.<=.) = liftA2 (<=)

infix 4 .>.
-- | The above type is a generalisation for:
--
-- @
-- __(.>.)__ :: 'Ord' a => 'Clash.Signal.Signal' a -> 'Clash.Signal.Signal' a -> 'Clash.Signal.Signal' 'Bool'
-- @
--
-- It is a version of ('>') that returns a 'Clash.Signal.Signal' of 'Bool'
(.>.) :: (Ord a, Applicative f) => f a -> f a -> f Bool
(.>.) = liftA2 (>)

infix 4 .>=.
-- | The above type is a generalisation for:
--
-- @
-- __(.>=.)__ :: 'Ord' a => 'Clash.Signal.Signal' a -> 'Clash.Signal.Signal' a -> 'Clash.Signal.Signal' 'Bool'
-- @
--
--  It is a version of ('>=') that returns a 'Clash.Signal.Signal' of 'Bool'
(.>=.) :: (Ord a, Applicative f) => f a -> f a -> f Bool
(.>=.) = liftA2 (>=)

instance Fractional a => Fractional (Signal tag a) where
  (/)          = liftA2 (/)
  recip        = fmap recip
  fromRational = signal# . fromRational

instance Arbitrary a => Arbitrary (Signal tag a) where
  arbitrary = liftA2 (:-) arbitrary arbitrary

instance CoArbitrary a => CoArbitrary (Signal tag a) where
  coarbitrary xs gen = do
    n <- arbitrary
    coarbitrary (take (abs n) (sample_lazy xs)) gen

-- | The above type is a generalisation for:
--
-- @
-- __testFor__ :: 'Int' -> 'Clash.Signal.Signal' Bool -> 'Property'
-- @
--
-- @testFor n s@ tests the signal @s@ for @n@ cycles.
testFor :: Foldable f => Int -> f Bool -> Property
testFor n = property . and . take n . sample

-- * List \<-\> Signal conversion (not synthesizable)

-- | The above type is a generalisation for:
--
-- @
-- __sample__ :: 'Clash.Signal.Signal' a -> [a]
-- @
--
-- Get an infinite list of samples from a 'Clash.Signal.Signal'
--
-- The elements in the list correspond to the values of the 'Clash.Signal.Signal'
-- at consecutive clock cycles
--
-- > sample s == [s0, s1, s2, s3, ...
--
-- __NB__: This function is not synthesizable
sample :: (Foldable f, Undefined a) => f a -> [a]
sample = foldr (\a b -> deepseqX a (a : b)) []

-- | The above type is a generalisation for:
--
-- @
-- __sampleN__ :: Int -> 'Clash.Signal.Signal' a -> [a]
-- @
--
-- Get a list of @n@ samples from a 'Clash.Signal.Signal'
--
-- The elements in the list correspond to the values of the 'Clash.Signal.Signal'
-- at consecutive clock cycles
--
-- > sampleN 3 s == [s0, s1, s2]
--
-- __NB__: This function is not synthesizable
sampleN :: (Foldable f, Undefined a) => Int -> f a -> [a]
sampleN n = take n . sample

-- | Create a 'Clash.Signal.Signal' from a list
--
-- Every element in the list will correspond to a value of the signal for one
-- clock cycle.
--
-- >>> sampleN 2 (fromList [1,2,3,4,5])
-- [1,2]
--
-- __NB__: This function is not synthesizable
fromList :: Undefined a => [a] -> Signal tag a
fromList = Prelude.foldr (\a b -> deepseqX a (a :- b)) (errorX "finite list")

-- * Simulation functions (not synthesizable)

-- | Simulate a (@'Clash.Signal.Signal' a -> 'Clash.Signal.Signal' b@) function
-- given a list of samples of type @a@
--
-- >>> simulate (register systemClockGen resetGen enableGen 8) [1, 1, 2, 3]
-- [8,8,1,2,3...
-- ...
--
-- __NB__: This function is not synthesizable
simulate :: (Undefined a, Undefined b) => (Signal tag1 a -> Signal tag2 b) -> [a] -> [b]
simulate f = sample . f . fromList

-- | The above type is a generalisation for:
--
-- @
-- __sample__ :: 'Clash.Signal.Signal' a -> [a]
-- @
--
-- Get an infinite list of samples from a 'Clash.Signal.Signal'
--
-- The elements in the list correspond to the values of the 'Clash.Signal.Signal'
-- at consecutive clock cycles
--
-- > sample s == [s0, s1, s2, s3, ...
--
-- __NB__: This function is not synthesizable
sample_lazy :: Foldable f => f a -> [a]
sample_lazy = foldr (:) []

-- | The above type is a generalisation for:
--
-- @
-- __sampleN__ :: Int -> 'Clash.Signal.Signal' a -> [a]
-- @
--
-- Get a list of @n@ samples from a 'Clash.Signal.Signal'
--
-- The elements in the list correspond to the values of the 'Clash.Signal.Signal'
-- at consecutive clock cycles
--
-- > sampleN 3 s == [s0, s1, s2]
--
-- __NB__: This function is not synthesizable
sampleN_lazy :: Foldable f => Int -> f a -> [a]
sampleN_lazy n = take n . sample_lazy

-- | Create a 'Clash.Signal.Signal' from a list
--
-- Every element in the list will correspond to a value of the signal for one
-- clock cycle.
--
-- >>> sampleN 2 (fromList [1,2,3,4,5] :: Signal System Int)
-- [1,2]
--
-- __NB__: This function is not synthesizable
fromList_lazy :: [a] -> Signal tag a
fromList_lazy = Prelude.foldr (:-) (error "finite list")

-- * Simulation functions (not synthesizable)

-- | Simulate a (@'Clash.Signal.Signal' a -> 'Clash.Signal.Signal' b@) function
-- given a list of samples of type @a@
--
-- >>> simulate (register systemClockGen resetGen enableGen 8) [1, 1, 2, 3]
-- [8,8,1,2,3...
-- ...
--
-- __NB__: This function is not synthesizable
simulate_lazy :: (Signal tag1 a -> Signal tag2 b) -> [a] -> [b]
simulate_lazy f = sample_lazy . f . fromList_lazy

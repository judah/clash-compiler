{-|
Copyright  :  (C) 2013-2016, University of Twente,
                  2017-2019, Myrtle Software Ltd, Google Inc.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

The Product/Signal isomorphism
-}

{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}

{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Signal.Bundle
  ( Bundle (..)
  )
where

import GHC.TypeLits                 (KnownNat, Symbol)
import Prelude                      hiding (head, map, tail)

import Clash.NamedTypes             ((:::))
import Clash.Signal.Bundle.Internal (deriveBundleTuples)
import Clash.Signal.Internal        (Signal (..))
import Clash.Sized.BitVector        (Bit, BitVector)
import Clash.Sized.Fixed            (Fixed)
import Clash.Sized.Index            (Index)
import Clash.Sized.Signed           (Signed)
import Clash.Sized.Unsigned         (Unsigned)
import Clash.Sized.Vector           (Vec, traverse#, lazyV)
import Clash.Sized.RTree            (RTree, lazyT)

-- | Isomorphism between a 'Clash.Signal.Signal' of a product type (e.g. a tuple) and a
-- product type of 'Clash.Signal.Signal''s.
--
-- Instances of 'Bundle' must satisfy the following laws:
--
-- @
-- 'bundle' . 'unbundle' = 'id'
-- 'unbundle' . 'bundle' = 'id'
-- @
--
-- By default, 'bundle' and 'unbundle', are defined as the identity, that is,
-- writing:
--
-- @
-- data D = A | B
--
-- instance Bundle D
-- @
--
-- is the same as:
--
-- @
-- data D = A | B
--
-- instance Bundle D where
--   type 'Unbundled'' clk D = 'Signal'' clk D
--   'bundle'   _ s = s
--   'unbundle' _ s = s
-- @
--
class Bundle a where
  type Unbundled (tag :: Symbol) a = res | res -> tag a
  type Unbundled tag a = Signal tag a
  -- | Example:
  --
  -- @
  -- __bundle__ :: ('Signal' tag a, 'Signal' tag b) -> 'Signal' tag (a,b)
  -- @
  --
  -- However:
  --
  -- @
  -- __bundle__ :: 'Signal' tag 'Clash.Sized.BitVector.Bit' -> 'Signal' tag 'Clash.Sized.BitVector.Bit'
  -- @
  bundle :: Unbundled tag a -> Signal tag a

  {-# INLINE bundle #-}
  default bundle :: (Signal tag a ~ Unbundled tag a)
                 => Unbundled tag a -> Signal tag a
  bundle s = s
  -- | Example:
  --
  -- @
  -- __unbundle__ :: 'Signal' tag (a,b) -> ('Signal' tag a, 'Signal' tag b)
  -- @
  --
  -- However:
  --
  -- @
  -- __unbundle__ :: 'Signal' tag 'Clash.Sized.BitVector.Bit' -> 'Signal' tag 'Clash.Sized.BitVector.Bit'
  -- @
  unbundle :: Signal tag a -> Unbundled tag a

  {-# INLINE unbundle #-}
  default unbundle :: (Unbundled tag a ~ Signal tag a)
                   => Signal tag a -> Unbundled tag a
  unbundle s = s

instance Bundle Bool
instance Bundle Integer
instance Bundle Int
instance Bundle Float
instance Bundle Double
instance Bundle (Maybe a)
instance Bundle (Either a b)

instance Bundle Bit
instance Bundle (BitVector n)
instance Bundle (Index n)
instance Bundle (Fixed rep int frac)
instance Bundle (Signed n)
instance Bundle (Unsigned n)

-- | Note that:
--
-- > bundle   :: () -> Signal tag ()
-- > unbundle :: Signal tag () -> ()
instance Bundle () where
  type Unbundled t () = t ::: ()
  -- ^ This is just to satisfy the injectivity annotation
  bundle   u = pure u
  unbundle _ = ()

deriveBundleTuples ''Bundle ''Unbundled 'bundle 'unbundle

instance KnownNat n => Bundle (Vec n a) where
  type Unbundled t (Vec n a) = Vec n (Signal t a)
  -- The 'Traversable' instance of 'Vec' is not synthesizable, so we must
  -- define 'bundle' as a primitive.
  bundle   = vecBundle#
  unbundle = sequenceA . fmap lazyV

{-# NOINLINE vecBundle# #-}
vecBundle# :: Vec n (Signal t a) -> Signal t (Vec n a)
vecBundle# = traverse# id

instance KnownNat d => Bundle (RTree d a) where
  type Unbundled t (RTree d a) = RTree d (Signal t a)
  bundle   = sequenceA
  unbundle = sequenceA . fmap lazyT

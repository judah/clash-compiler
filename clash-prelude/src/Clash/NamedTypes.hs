{- |
Copyright  :  (C) 2017, Myrtle Software Ltd, QBayLogic, Google Inc.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

Add inline documentation to types:

@
fifo
  :: Clock tag enabled
  -> Reset tag polarity
  -> SNat addrSize
  -> "read request" ::: Signal tag Bool
  -> "write request" ::: Signal tag (Maybe (BitVector dataSize))
  -> ( "q"     ::: Signal tag (BitVector dataSize)
     , "full"  ::: Signal tag Bool
     , "empty" ::: Signal tag Bool
     )
@

which can subsequently be inspected in the interactive environment:

>>> import Clash.Explicit.Prelude
>>> :t fifo @System
fifo @System
  :: Clock System enabled
     -> Reset System polarity
     -> SNat addrSize
     -> ("read request" ::: Signal System Bool)
     -> ("write request" ::: Signal System (Maybe (BitVector dataSize)))
     -> ("q" ::: Signal System (BitVector dataSize),
         "full" ::: Signal System Bool, "empty" ::: Signal System Bool)

-}

{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE Safe #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.NamedTypes
  ((:::))
where

type (name :: k) ::: a = a
-- ^ Annotate a type with a name

{- $setup
>>> :set -XDataKinds -XTypeOperators -XNoImplicitPrelude
>>> import Clash.Explicit.Prelude
>>> :{
let fifo
      :: Clock tag enabled
      -> Reset tag polarity
      -> SNat addrSize
      -> "read request" ::: Signal tag Bool
      -> "write request" ::: Signal tag (Maybe (BitVector dataSize))
      -> ( "q"     ::: Signal tag (BitVector dataSize)
         , "full"  ::: Signal tag Bool
         , "empty" ::: Signal tag Bool
         )
    fifo = Clash.Explicit.Prelude.undefined
:}

-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Module:      Control.Remote.WithoutAsync.Monad.Packet.Query
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.Packet.Query
  ( -- * The remote applicative
    QueryPacket(..)
  ) where

import           Control.Remote.Packet.Applicative

-- | A Remote Applicative, that can encode both commands and procedures, bundled together.
newtype QueryPacket prim a = QueryPacket (ApplicativePacket prim a)
  deriving (Functor, Applicative)


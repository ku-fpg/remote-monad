{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Module:      Control.Remote.WithoutAsync.Monad.Packet.Query
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.WithoutAsync.Packet.Query
  ( -- * The remote applicative
    QueryPacket(..)
  ) where

import           Control.Remote.WithoutAsync.Packet.Applicative
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict

import           Control.Natural

-- | A Remote Applicative, that can encode both commands and procedures, bundled together.
newtype QueryPacket p a = QueryPacket (ApplicativePacket p a)
  deriving (Functor, Applicative)


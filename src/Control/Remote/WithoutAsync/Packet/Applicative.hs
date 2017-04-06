{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module:      Control.Remote.WithoutAsync.Monad.Packet.Applicative
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.WithoutAsync.Packet.Applicative
  ( -- * The remote applicative
    ApplicativePacket(..)
  ) where


import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

import Control.Natural

-- | A Remote Applicative, that can encode both commands and procedures, bundled together.

data ApplicativePacket (q :: * -> *) (a :: *) where
   Query     :: q a                        -> ApplicativePacket q a
   Zip       :: (x -> y -> z)
             -> ApplicativePacket q x 
             -> ApplicativePacket q y      -> ApplicativePacket q z
   Pure      :: a                          -> ApplicativePacket q a  

instance Functor (ApplicativePacket q) where
  fmap f g = pure f <*> g

instance Applicative (ApplicativePacket q) where
  pure a = Pure a
  g <*> h = Zip ($) g h

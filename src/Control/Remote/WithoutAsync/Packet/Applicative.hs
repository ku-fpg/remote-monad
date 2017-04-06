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

data ApplicativePacket (p :: * -> *) (a :: *) where
   Procedure :: p a                        -> ApplicativePacket p a
   Zip       :: (x -> y -> z)
             -> ApplicativePacket p x 
             -> ApplicativePacket p y      -> ApplicativePacket p z
   Pure      :: a                          -> ApplicativePacket p a  

instance Functor (ApplicativePacket p) where
  fmap f g = pure f <*> g

instance Applicative (ApplicativePacket p) where
  pure a = Pure a
  g <*> h = Zip ($) g h

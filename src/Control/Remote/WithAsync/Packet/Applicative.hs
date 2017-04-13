{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module:      Control.Remote.WithAsync.Monad.Packet.Applicative
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.WithAsync.Packet.Applicative
  ( -- * The remote applicative
    ApplicativePacket(..)
  ) where


import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

import qualified Control.Remote.WithAsync.Packet.Strong as Strong
import Control.Natural

-- | A Remote Applicative, that can encode both commands and procedures, bundled together.

data ApplicativePacket (cp :: * -> (* -> *) -> * -> *) (a :: *) where
   Procedure :: cp c p a               -> ApplicativePacket cp a
   Zip       :: (x -> y -> z)
             -> ApplicativePacket cp x 
             -> ApplicativePacket cp y -> ApplicativePacket cp z
   Pure      :: a                      -> ApplicativePacket cp a  

instance Functor (ApplicativePacket cp) where
  fmap f g = pure f <*> g

instance Applicative (ApplicativePacket cp) where
  pure a = Pure a
  g <*> h = Zip ($) g h

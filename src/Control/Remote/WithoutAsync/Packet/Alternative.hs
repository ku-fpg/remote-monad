{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module:      Control.Remote.WithoutAsync.Monad.Packet.Alternative
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.WithoutAsync.Packet.Alternative
  ( -- * The remote applicative
    AlternativePacket(..)
  ) where


import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

import Control.Natural



-- | A Remote Applicative, that can encode both commands and procedures, bundled together.

data AlternativePacket (p :: * -> *) (a :: *) where
   Procedure :: p a                       -> AlternativePacket p a
   Zip       :: (x -> y -> z)
             -> AlternativePacket p x 
             -> AlternativePacket p y     -> AlternativePacket p z
   Pure      :: a                         -> AlternativePacket p a  
   Alt       :: AlternativePacket p a
             -> AlternativePacket p a     -> AlternativePacket p a
   Empty     ::                              AlternativePacket p a

instance Functor (AlternativePacket p) where
  fmap f g = pure f <*> g

instance Applicative (AlternativePacket p) where
  pure a = Pure a
  g <*> h = Zip ($) g h

instance Alternative (AlternativePacket p) where
  g <|> h = g `Alt` h
  empty   = Empty


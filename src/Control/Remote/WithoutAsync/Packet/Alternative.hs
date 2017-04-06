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

data AlternativePacket (q :: * -> *) (a :: *) where
   Query :: q a                       -> AlternativePacket q a
   Zip   :: (x -> y -> z)
         -> AlternativePacket q x 
         -> AlternativePacket q y     -> AlternativePacket q z
   Pure  :: a                         -> AlternativePacket q a  
   Alt   :: AlternativePacket q a
         -> AlternativePacket q a     -> AlternativePacket q a
   Empty ::                              AlternativePacket q a

instance Functor (AlternativePacket q) where
  fmap f g = pure f <*> g

instance Applicative (AlternativePacket q) where
  pure a = Pure a
  g <*> h = Zip ($) g h

instance Alternative (AlternativePacket q) where
  g <|> h = g `Alt` h
  empty   = Empty


{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module:      Control.Remote.Packet.IfPacket
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.Packet.If
  ( -- * The remote applicative
    IfPacket(..)
    -- * Utility
  , superCommand
  ) where


import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

import qualified Control.Remote.Packet.Strong as Strong
import Control.Natural



-- | A Remote Applicative, that can encode both commands and procedures, bundled together.

data IfPacket (c :: *) (p :: * -> *) (a :: *) where
   Command   :: c                  -> IfPacket c p ()
   Procedure :: p a                -> IfPacket c p a
   Zip       :: (x -> y -> z)      -> IfPacket c p x
             -> IfPacket c p y     -> IfPacket c p z
   Pure      :: a                  -> IfPacket c p a
   Alt       :: IfPacket c p a
             -> IfPacket c p a     -> IfPacket c p a
   Empty     ::                       IfPacket c p a
   If        :: IfPacket c p Bool  -> IfPacket c p a
             -> IfPacket c p a     -> IfPacket c p a

instance Functor (IfPacket c p) where
  fmap f g = pure f <*> g

instance Applicative (IfPacket c p) where
  pure a = Pure a
  g <*> h = Zip ($) g h

instance Alternative (IfPacket c p) where
  g <|> h = g `Alt` h
  empty   = Empty

-- | This simulates a 'IfPacket', to see if it only contains commands, and if so,
-- returns the static result. The commands still need executed. The term super-command
-- is a play on Hughes' super-combinator terminology.

superCommand :: IfPacket c p a -> Maybe a
superCommand (Pure a)        = pure a
superCommand (Command c)     = pure ()
superCommand (Procedure _)   = Nothing
superCommand (Alt g h)       = Nothing
superCommand (Zip ($) g h)   = ($) <$> superCommand g <*> superCommand h
superCommand (If _ _ _)      = Nothing


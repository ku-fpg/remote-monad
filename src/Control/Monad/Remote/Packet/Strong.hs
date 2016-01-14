{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module:      Control.Monad.Remote.Packet.Strong
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Monad.Remote.Packet.Strong where

import qualified Control.Monad.Remote.Packet.Weak as Weak
import Control.Natural


-- A Strong Packet, that can encode a list of commands, terminated by an optional procedure.

data Packet c p a where
   Command   :: c -> Packet c p b -> Packet c p b
   Procedure :: p a               -> Packet c p a
   Pure      :: a                 -> Packet c p a

-- promote a Weak packet transport, into a Strong packet transport.
-- Note this unbundles the Strong packet, but does provide the API.
toPacket :: (Applicative m) => (Weak.Packet c p ~> m) -> (Packet c p ~> m)
toPacket f (Command c pk) = f (Weak.Command c)   *> toPacket f pk
toPacket f (Procedure p)  = f (Weak.Procedure p)
toPacket f (Pure a)       = pure a


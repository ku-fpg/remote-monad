{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module:      Control.Remote.Monad.Packet.Strong
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.Monad.Packet.Strong where

import qualified Control.Remote.Monad.Packet.Weak as Weak
import Control.Natural


-- A Strong Packet, that can encode a list of commands, terminated by an optional procedure.

data Packet c p a where
   Command   :: c -> Packet c p b -> Packet c p b
   Procedure :: p a               -> Packet c p a
   Pure      :: a                 -> Packet c p a

-- promote a Weak packet transport, into a Strong packet transport.
-- Note this unbundles the Strong packet, but does provide the API.
sendPacket :: (Applicative m) => (Weak.Packet c p ~> m) -> (Packet c p ~> m)
sendPacket f (Command c pk) = f (Weak.Command c)   *> sendPacket f pk
sendPacket f (Procedure p)  = f (Weak.Procedure p)
sendPacket f (Pure a)       = pure a


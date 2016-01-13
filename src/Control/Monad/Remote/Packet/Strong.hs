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
toPacket :: (Weak.WeakSend f, Applicative m) => (f c p ~> m) -> (Packet c p ~> m)
toPacket f (Command c pk) = f (Weak.command c)   *> toPacket f pk
toPacket f (Procedure p)  = f (Weak.procedure p)
toPacket f (Pure a)       = pure a

class Weak.WeakSend f => StrongSend f where
   command   :: c   -> f c p a -> f c p a
   procedure :: p a            -> f c p a
   done      :: a              -> f c p a 

instance StrongSend Packet where
  command   = Command
  procedure = Procedure
  done      = Pure
  
instance Weak.WeakSend Packet where
  command   c = Control.Monad.Remote.Packet.Strong.command c (done ())
  procedure p = Control.Monad.Remote.Packet.Strong.procedure p


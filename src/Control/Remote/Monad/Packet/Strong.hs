{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module:      Control.Remote.Monad.Strong.Strong
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.Monad.Packet.Strong where

import qualified Control.Remote.Monad.Packet.Weak as Weak
import           Control.Remote.Monad.Packet.Weak (Weak)
import           Control.Natural


-- A Strong Strong, that can encode a list of commands, terminated by an optional procedure.

data Strong c p a where
   Command   :: c -> Strong c p b -> Strong c p b
   Procedure :: p a               -> Strong c p a
   Pure      :: a                 -> Strong c p a

-- promote a Weak packet transport, into a Strong packet transport.
-- Note this unbundles the Strong packet, but does provide the API.
sendStrong :: (Applicative m) => (Weak c p ~> m) -> (Strong c p ~> m)
sendStrong f (Command c pk) = f (Weak.Command c)   *> sendStrong f pk
sendStrong f (Procedure p)  = f (Weak.Procedure p)
sendStrong f (Pure a)       = pure a


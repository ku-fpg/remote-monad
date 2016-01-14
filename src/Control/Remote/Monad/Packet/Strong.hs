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


-- | A Strong Packet, that can encode a list of commands, terminated by an optional procedure.

data Strong c p a where
   Command   :: c -> Strong c p b -> Strong c p b
   Procedure :: p a               -> Strong c p a
   Pure      :: a                 -> Strong c p a

-- | promote a Weak packet transporter, into a Strong packet transporter.
runStrong :: (Applicative m) => (Weak c p ~> m) -> (Strong c p ~> m)
runStrong f (Command c pk) = f (Weak.Command c)   *> runStrong f pk
runStrong f (Procedure p)  = f (Weak.Procedure p)
runStrong f (Pure a)       = pure a

-- | A Hughes-style version of 'Strong', with efficent append.
newtype HStrong c p = HStrong (Strong c p ~> Strong c p)


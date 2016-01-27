{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module:      Control.Remote.Applicative
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.Applicative 
  ( -- * The remote applicative
    RemoteApplicative
    -- * The primitive lift functions
  , command
  , procedure
    -- * The run functions
  , ApplicativePacket()
  , runApplicative
  , runWeakApplicative
  , runStrongApplicative
  ) where


import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

import           Control.Remote.Monad.Packet.Applicative
import qualified Control.Remote.Monad.Packet.Strong as Strong
import           Control.Remote.Monad.Packet.Strong (Strong, HStrong(..))
import qualified Control.Remote.Monad.Packet.Weak as Weak
import           Control.Remote.Monad.Packet.Weak (Weak)
import           Control.Natural

-- | promote a command into the applicative
command :: c -> RemoteApplicative c p ()
command c = Command (pure ()) c

-- | promote a command into the applicative
procedure :: p a -> RemoteApplicative c p a
procedure p = Procedure (pure id) p


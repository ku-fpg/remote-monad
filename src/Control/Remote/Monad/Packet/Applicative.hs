{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module:      Control.Remote.Monad.Packet.Applicative
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.Monad.Packet.Applicative
  ( -- * The remote applicative
    RemoteApplicative(..)
    -- * Utility
  , superCommand
  ) where


import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

import qualified Control.Remote.Monad.Packet.Strong as Strong
import           Control.Remote.Monad.Packet.Strong (Strong, HStrong(..))
import qualified Control.Remote.Monad.Packet.Weak as Weak
import           Control.Remote.Monad.Packet.Weak (Weak)
import Control.Natural

-- | A Remote Applicative, that can encode both commands and procedures, bundled together.

data RemoteApplicative (c :: *) (p :: * -> *) (a :: *) where
   Command   :: RemoteApplicative c p b        -> c   -> RemoteApplicative c p b
   Procedure :: RemoteApplicative c p (a -> b) -> p a -> RemoteApplicative c p b
   Pure      :: a                                     -> RemoteApplicative c p a  

-- | This simulates a 'RemoteApplicative', to see if it only contains commands, and if so,
-- returns the static result. The commands still need executed. The term super-command
-- is a play on Hughes' super-combinator terminology.

superCommand :: RemoteApplicative c p a -> Maybe a
superCommand (Pure a)        = Just a
superCommand (Command g _)   = superCommand g
superCommand (Procedure _ _) = Nothing

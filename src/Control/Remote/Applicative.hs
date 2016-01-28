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
    RemoteApplicative(..) -- for now
    -- * The primitive lift functions
  , command
  , procedure
    -- * The run functions
  , ApplicativePacket'()
  , runApplicative
  , runWeakApplicative
  , runStrongApplicative
  , runApplicativeApplicative
  ) where


import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

import           Control.Remote.Monad.Packet.Applicative as A
import qualified Control.Remote.Monad.Packet.Strong as Strong
import           Control.Remote.Monad.Packet.Strong (Strong, HStrong(..))
import qualified Control.Remote.Monad.Packet.Weak as Weak
import           Control.Remote.Monad.Packet.Weak (Weak)
import           Control.Natural

newtype RemoteApplicative c p a = RemoteApplicative (ApplicativePacket c p a)

instance Functor (RemoteApplicative c p) where
  fmap f (RemoteApplicative g) = RemoteApplicative (fmap f g)

instance Applicative (RemoteApplicative c p) where
  pure a = RemoteApplicative (pure a)
  (RemoteApplicative f) <*> (RemoteApplicative g) = RemoteApplicative (f <*> g)

-- | promote a command into the applicative
command :: c -> RemoteApplicative c p ()
command c = RemoteApplicative (Command (pure ()) c)

-- | promote a command into the applicative
procedure :: p a -> RemoteApplicative c p a
procedure p = RemoteApplicative (Procedure (pure id) p)

class ApplicativePacket' f where
  -- | This overloaded function chooses the best bundling strategy
  --   based on the type of the handler your provide.
  runApplicative :: (Monad m) => (f c p ~> m) -> (RemoteApplicative c p ~> m)

instance ApplicativePacket' Weak where
  runApplicative = runWeakApplicative

instance ApplicativePacket' Strong where
  runApplicative = runStrongApplicative

instance ApplicativePacket' ApplicativePacket where
  runApplicative = runApplicativeApplicative

-- | The weak remote applicative, that sends commands and procedures piecemeal.
runWeakApplicative :: forall m c p . (Applicative m) => (Weak c p ~> m) -> (RemoteApplicative c p ~> m)
runWeakApplicative f (RemoteApplicative (Command   g c)) = runWeakApplicative f (RemoteApplicative g) <*  f (Weak.Command c)
runWeakApplicative f (RemoteApplicative (Procedure g p)) = runWeakApplicative f (RemoteApplicative g) <*> f (Weak.Procedure p)
runWeakApplicative f (RemoteApplicative (Pure        a)) = pure a

-- | The strong remote applicative, that bundles together commands.
runStrongApplicative :: forall m c p . (Monad m) => (Strong c p ~> m) -> (RemoteApplicative c p ~> m)
runStrongApplicative f (RemoteApplicative p) = do
    (r,HStrong h) <- runStateT (go p) (HStrong id)
    f $ h $ Strong.Done
    return r
  where
    go :: forall a . ApplicativePacket c p a -> StateT (HStrong c p) m a
    go (Pure a)        = return a
    go (Command g c)   = do
        r <- go g
        modify (\ (HStrong cs) -> HStrong (cs . Strong.Command c))
        return r
    go (Procedure g p) = do
        r1 <- go g
        HStrong cs <- get 
        put (HStrong id)
        r2 <- lift $ f $ cs $ Strong.Procedure $ p
        return $ r1 r2

-- | The applicative remote applicative, that is the identity function.
runApplicativeApplicative :: (ApplicativePacket c p ~> m) -> (RemoteApplicative c p ~> m)
runApplicativeApplicative f (RemoteApplicative m) = f m

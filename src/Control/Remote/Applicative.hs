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
    RemoteApplicativeT
  , RemoteApplicative (..)
    -- * The primitive lift functions
  , command
  , procedure
    -- * The run functions
  , RunApplicative(runApplicativeT)
  , runApplicative
  , runWeakApplicativeT
  , runStrongApplicativeT
  , runApplicativeApplicativeT
  ) where


import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Monad.Identity
import Control.Category ((>>>))

import           Control.Remote.Monad.Packet.Applicative as A
import qualified Control.Remote.Monad.Packet.Strong as Strong
import           Control.Remote.Monad.Packet.Strong (StrongPacket, HStrongPacket(..))
import qualified Control.Remote.Monad.Packet.Weak as Weak
import           Control.Remote.Monad.Packet.Weak (WeakPacket)
import           Control.Remote.Monad.Types as T
import           Control.Natural

type RemoteApplicative c p  = RemoteApplicativeT c p Identity 

-- | promote a command into the applicative
command :: c -> RemoteApplicativeT c p m ()
command c = T.Command c

-- | promote a command into the applicative
procedure :: p a -> RemoteApplicativeT c p m a
procedure p = T.Procedure p

-- | 'RunApplicative' is the overloading for choosing the appropriate bundling strategy for applicative.
class RunApplicative f where
  -- | This overloaded function chooses the appropriate bundling strategy
  --   based on the type of the handler your provide.
  runApplicativeT :: (Monad m) => (f c p :~> m) -> (RemoteApplicativeT c p m:~> m)

instance RunApplicative WeakPacket where
  runApplicativeT = runWeakApplicativeT

instance RunApplicative StrongPacket where
  runApplicativeT = runStrongApplicativeT

instance RunApplicative ApplicativePacket where
  runApplicativeT = runApplicativeApplicativeT

runApplicative :: (RunApplicative f, Monad m) => (f c p :~> m) -> (RemoteApplicative c p :~> m)
runApplicative f = (nat $ mapRemoteApplicativeT (return . runIdentity)) >>> runApplicativeT f

-- | The weak remote applicative, that sends commands and procedures piecemeal.
runWeakApplicativeT :: forall m c p . (Applicative m) => (WeakPacket c p :~> m) -> (RemoteApplicativeT c p m :~> m)
runWeakApplicativeT (Nat f) = nat go 
  where
    go :: forall a . RemoteApplicativeT c p m a -> m a
    go (T.Command   c) = f (Weak.Command c)
    go (T.Procedure p) = f (Weak.Procedure p)
    go (T.Ap f g)      = go f <*> go g
    go (T.Pure      a) = pure a
    go (T.Local     m) = m

-- | The strong remote applicative, that bundles together commands.
runStrongApplicativeT :: forall m c p . (Monad m) => (StrongPacket c p :~> m) -> (RemoteApplicativeT c p m :~> m)
runStrongApplicativeT (Nat f) = nat $ \ p -> do
    (r,HStrongPacket h) <- runStateT (go p) (HStrongPacket id)
    f $ h $ Strong.Done
    return r
  where
    go :: forall a . T.RemoteApplicativeT c p m a -> StateT (HStrongPacket c p) m a
    go (T.Pure a)        = return a
    go (T.Command c)   = do
        modify (\ (HStrongPacket cs) -> HStrongPacket (cs . Strong.Command c))
        return ()
    go (T.Procedure p) = do
        HStrongPacket cs <- get
        put (HStrongPacket id)
        r2 <- lift $ f $ cs $ Strong.Procedure $ p
        return $ r2
    go (T.Ap f g)      = go f <*> go g
    go (T.Local m)     = lift m

-- | The applicative remote applicative, that is the identity function.
runApplicativeApplicativeT :: forall m c p . (ApplicativePacket c p :~> m) -> (RemoteApplicativeT c p m :~> m)
runApplicativeApplicativeT (Nat f) = nat $ go2
  where
    go2 :: forall a . RemoteApplicativeT c p m a -> m a
    go2 (T.Local m) =  m
    go2 otherwise = f  $ go otherwise

    go :: forall a . T.RemoteApplicativeT c p m a -> ApplicativePacket c p a
    go (T.Pure a)      = A.Pure a
    go (T.Command   c) = A.Command  c
    go (T.Procedure p) = A.Procedure p
    go (T.Ap g h)      = A.Zip ($) (go g) (go h)

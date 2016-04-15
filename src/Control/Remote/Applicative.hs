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
    RemoteLocalApplicative
  , RemoteApplicative
    -- * The primitive lift functions
  , command
  , procedure
    -- * The run functions
  , RunApplicative(runLocalApplicative)
  , runApplicative
  , runWeakApplicative
  , runStrongApplicative
  , runApplicativeApplicative
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

type RemoteApplicative = RemoteLocalApplicative Identity 

-- | promote a command into the applicative
command :: c -> RemoteLocalApplicative l c p ()
command c = T.Command c

-- | promote a command into the applicative
procedure :: p a -> RemoteLocalApplicative l c p a
procedure p = T.Procedure p

-- | 'RunApplicative' is the overloading for choosing the appropriate bundling strategy for applicative.
class RunApplicative f where
  -- | This overloaded function chooses the appropriate bundling strategy
  --   based on the type of the handler your provide.
  runLocalApplicative :: (Monad m) => (l :~> m)->(f c p :~> m) -> (RemoteLocalApplicative l c p:~> m)

instance RunApplicative WeakPacket where
  runLocalApplicative = runWeakApplicative

instance RunApplicative StrongPacket where
  runLocalApplicative = runStrongApplicative

instance RunApplicative ApplicativePacket where
  runLocalApplicative = runApplicativeApplicative

runApplicative :: (RunApplicative f, Monad m) => (f c p :~> m) -> (RemoteApplicative c p :~> m)
runApplicative = runLocalApplicative $ nat (return . runIdentity)

-- | The weak remote applicative, that sends commands and procedures piecemeal.
runWeakApplicative :: forall m c p l . (Applicative m) => (l :~> m) -> (WeakPacket c p :~> m) -> (RemoteLocalApplicative l c p :~> m)
runWeakApplicative (Nat lf) (Nat rf) = nat go 
  where
    go :: forall a . RemoteLocalApplicative l c p a -> m a
    go (T.Command   c) = rf (Weak.Command c)
    go (T.Procedure p) = rf (Weak.Procedure p)
    go (T.Ap g h)      = go g <*> go h
    go (T.Pure      a) = pure a
    go (T.Local     m) = lf m

-- | The strong remote applicative, that bundles together commands.
runStrongApplicative :: forall m c p l . (Monad m) => (l :~> m) -> (StrongPacket c p :~> m) -> (RemoteLocalApplicative l c p :~> m)
runStrongApplicative (Nat lf) (Nat rf) = nat $ \ p -> do
    (r,HStrongPacket h) <- runStateT (go p) (HStrongPacket id)
    rf $ h $ Strong.Done
    return r
  where
    go :: forall a . T.RemoteLocalApplicative l c p a -> StateT (HStrongPacket c p) m a
    go (T.Pure a)        = return a
    go (T.Command c)   = do
        modify (\ (HStrongPacket cs) -> HStrongPacket (cs . Strong.Command c))
        return ()
    go (T.Procedure p) = do
        HStrongPacket cs <- get
        put (HStrongPacket id)
        r2 <- lift $ rf $ cs $ Strong.Procedure $ p
        return $ r2
    go (T.Ap g h)      = go g <*> go h
    go (T.Local m)     = lift $ lf m

-- | The applicative remote applicative, that is the identity function.
runApplicativeApplicative :: forall m c p l . (l :~> m) -> (ApplicativePacket c p :~> m) -> (RemoteLocalApplicative l c p :~> m)
runApplicativeApplicative (Nat lf) (Nat rf) = nat $ go2
  where
    go2 :: forall a . RemoteLocalApplicative l c p a -> m a
    go2 (T.Local m) =  lf m
    go2 otherwise = rf  $ go otherwise

    go :: forall a . T.RemoteLocalApplicative l c p a -> ApplicativePacket c p a
    go (T.Pure a)      = A.Pure a
    go (T.Command   c) = A.Command  c
    go (T.Procedure p) = A.Procedure p
    go (T.Ap g h)      = A.Zip ($) (go g) (go h)

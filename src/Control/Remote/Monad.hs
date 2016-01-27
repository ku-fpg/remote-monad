{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module:      Control.Remote.Monad.Packet.Weak
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.Monad where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

import qualified Control.Remote.Applicative as A
import Control.Remote.Monad.Packet.Weak as Weak
import Control.Remote.Monad.Packet.Strong as Strong

import Control.Natural

data Remote c p a where
   Appl        :: A.RemoteApplicative c p a -> Remote c p a
   Bind        :: A.RemoteApplicative c p a -> (a -> Remote c p b) -> Remote c p b
  
instance Functor (Remote c p) where
  fmap f m = pure f <*> m

instance Applicative (Remote c p) where
  pure a                = Appl (pure a)
  Appl f   <*> Appl g   = Appl (f <*> g)
  Appl f   <*> Bind m k = Bind (pure (,) <*> f <*> m) (\ (a,b) -> fmap a $ k b)
  Bind m k <*> r        = Bind m (\ a -> k a <*> r)

instance Monad (Remote c p) where
  return = pure
  Appl m >>= k    = Bind m k
  Bind m k >>= k2 = Bind m (\ a -> k a >>= k2)
  
  m1 >> m2 = m1 *> m2 -- This improves our bundling opportunities

-- | promote a command into the remote monad
command :: c -> Remote c p ()
command = Appl . A.command

-- | promote a procedure into the remote monad
procedure :: p a -> Remote c p a
procedure = Appl . A.procedure 

class SendMonad f where
  sendMonad :: (Monad m) => (f c p ~> m) -> (Remote c p ~> m)

runWeakMonad :: (Monad m, A.SendApplicative f) => (f c p ~> m) -> (Remote c p ~> m)
runWeakMonad f (Appl g)   = A.sendApplicative f g
runWeakMonad f (Bind g k) = A.sendApplicative f g >>= runWeakMonad f . k

instance SendMonad Weak where
  sendMonad = runWeakMonad

instance SendMonad Strong where
  sendMonad = runStrongMonad

-- promote a Strong packet transport, into a Monad packet transport.
-- This is the classical remote monad.
runStrongMonad :: forall m c p . (Monad m) => (Strong c p ~> m) -> (Remote c p ~> m)
runStrongMonad f p = do
    (r,HStrong h) <- runStateT (go2 p) (HStrong id)
    f $ h $ Strong.Done
    return r
  where
    go2 :: forall a . Remote c p a -> StateT (HStrong c p) m a
    go2 (Appl app)   = go app
    go2 (Bind app k) = go app >>= \ a -> go2 (k a)

    go :: forall a . A.RemoteApplicative c p a -> StateT (HStrong c p) m a
    go (A.Pure a)        = return a
    go (A.Command g c)   = do
        r <- go g
        modify (\ (HStrong cs) -> HStrong (cs . Strong.Command c))
        return r
    go (A.Procedure g p) = do
        r1 <- go g
        HStrong cs <- get 
        put (HStrong id)
        r2 <- lift $ f $ cs $ Strong.Procedure $ p
        return $ r1 r2

instance SendMonad A.RemoteApplicative where
  sendMonad = runApplicativeMonad

-- promote a Strong packet transport, into a Monad packet transport.
-- This is the classical remote monad.
runApplicativeMonad :: forall m c p . (Monad m) => (A.RemoteApplicative c p ~> m) -> (Remote c p ~> m)
runApplicativeMonad f p = do
    (r,h) <- runStateT (go2 p) (pure ())
    f $ h
    return r
  where
    go2 :: forall a . Remote c p a -> StateT (A.RemoteApplicative c p ()) m a
    go2 (Appl app)   = go app
    go2 (Bind app k) = go app >>= \ a -> go2 (k a)

    go :: forall a . A.RemoteApplicative c p a -> StateT (A.RemoteApplicative c p ()) m a
    go ap = case A.superCommand ap of
                Nothing -> do
                  ap' <- get
                  put (pure ())
                  lift $ f $ (ap' *>  ap)
                Just a -> do
                  modify (\ ap' -> ap' <* ap)
                  return a


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

import qualified Control.Remote.Applicative as A
import Control.Remote.Monad.Packet.Weak as Weak
import Control.Remote.Monad.Packet.Strong as Strong

import Control.Natural

data Remote c p a where
   Appl        :: A.Remote c p a -> Remote c p a
   Bind        :: A.Remote c p a -> (a -> Remote c p b) -> Remote c p b
  
instance Functor (Remote c p) where
  fmap f m = pure f <*> m

instance Applicative (Remote c p) where
  pure a                = Appl (pure a)
  Appl f   <*> Appl g   = Appl (f <*> g)
  Appl f   <*> Bind m k = Bind (pure (,) <*> f <*> m) (\ (a,b) -> fmap a $ k b)
  Bind m k <*> r        = Bind m (\ a -> k a <*> r)

instance Monad (Remote c p) where
  return = pure
  Bind m k >>= k2 = Bind m (\ a -> k a >>= k2)

command :: c -> Remote c p ()
command = Appl . A.command

procedure :: p a -> Remote c p a
procedure = Appl . A.procedure 

runWeakMonad :: (Monad m, A.SendApplicative f) => (f c p ~> m) -> (Remote c p ~> m)
runWeakMonad f (Appl g)   = A.sendApplicative f g
runWeakMonad f (Bind g k) = A.sendApplicative f g >>= runWeakMonad f . k

runStrongMonad :: forall m c p . (Monad m) => (Strong c p ~> m) -> (Remote c p ~> m)
runStrongMonad f p = go p $ \ cs a -> do
    f $ cs $ Strong.Pure ()
    return a
  where
    go :: forall a r k b . Remote c p a -> ((forall b . Strong c p b -> Strong c p b) -> a -> m b) -> m b
    go (Appl app)    k = go' app k
    go (Bind app k0) k = go' app $ \ cs r -> go (k0 r) k -- cs is ignored ==> WRONG


    go' :: forall a r k b . A.Remote c p a -> ((forall b . Strong c p b -> Strong c p b) -> a -> m b) -> m b
    go' (A.Pure a)        k = k id a
    go' (A.Command g c)   k = go' g $ \ cs -> k (cs . Strong.Command c)
    go' (A.Procedure g p) k = go' g $ \ cs r -> do
        a <- f $ cs $ Strong.Procedure p
        k id (r a)
{-
runApplicativeMonad :: forall m c p . (Monad m) => (A.Remote c p ~> m) -> (Remote c p ~> m)
runApplicativeMonad f p = go p $ \ cs a -> do
    f $ cs $ pure ()
    return a
  where
    go :: forall a r k b . Remote c p a -> ((forall b . A.Remote c p b -> A.Remote c p b) -> a -> m b) -> m b
    go (Appl app)    k = go' app k
    go (Bind app k0) k = go' app $ \ cs r -> go (k0 r) k

    go' :: forall a r k b . A.Remote c p a -> ((forall b . A.Remote c p b -> A.Remote c p b) -> a -> m b) -> m b
    go' ap k = case A.superCommand ap of
                  Nothing -> do
                      a <- f ( ap)
                      k id
                  do a <- runP (p0 *> p)
                                runSuper' (k a) (Pure ())
                  Just a -> k (\ r -> r <* ap) a

{-
        go' (A.Pure a)        k = k id a
    go' (A.Command g c)   k = go' g $ \ cs -> k (cs . Strong.Command c)
    go' (A.Procedure g p) k = go' g $ \ cs r -> do
        a <- f $ cs $ Strong.Procedure p
        k id (r a)
-}
-}
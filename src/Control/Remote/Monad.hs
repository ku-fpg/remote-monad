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

-- It would make more sense directly interpreate the A.Remote.
--runWeakWP :: (Monad m) => (Strong.Packet c p ~> m) -> (Remote c p ~> m)
--runWeakWP f g = runWeakSP (Strong.toPacket f) g

--runMonad :: (Monad m) => (Packet.Remote c p ~> Local st m) -> (Remote c p ~> Local st m)
--runMonad f (Appl g)   = f g
--runMonad f (Bind g k) = f g >>= runMonad f . k

{-
runMonad :: (Monad m) => (forall a . A.Remote c p a -> st -> m (a,st)) -> (forall a. Remote c p a -> st -> m (a,st))
runMonad f (Appl g)   st0 = f g st0
runMonad f (Bind g k) st0 = f g st0 >>= \ (a,st1) -> runMonad f (k a) st1
-}

runStrong :: forall m c p a f . (Monad m) => (Strong c p ~> m) -> (Remote c p ~> m)
runStrong = undefined

--runStrongApplicative :: forall m c p a f . (Applicative.ApplicativeSend f, Monad m) => (f c p ~> m) -> (Remote c p ~> m)
--runStrongApplicative = undefined

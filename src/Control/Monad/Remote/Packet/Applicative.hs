{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module:      Control.Monad.Remote.Packet.Applicative
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Monad.Remote.Packet.Applicative where

import qualified Control.Monad.Remote.Packet.Strong as Strong
import qualified Control.Monad.Remote.Packet.Weak as Weak
import Control.Natural

-- An Applicative Packet, that can encode both commands and procedures, bundled together.
-- terminated by an optional 'Procedure'.

data Packet c p a where
   Command   :: Packet c p b -> c -> Packet c p b
   Procedure :: Packet c p (a -> b) -> p a -> Packet c p b
   Pure      :: a -> Packet c p a 

instance Functor (Packet c p) where
  fmap f (Command g c)   = Command (fmap f g) c
  fmap f (Procedure g p) = Procedure (fmap (f .) g) p
  fmap f (Pure a)        = Pure (f a)

instance Applicative (Packet c p) where
  pure a = Pure a
  (Pure f) <*> m = fmap f m
  (Command g c)   <*> (Pure a)        = Command (fmap (\ f -> f a) g) c
  (Procedure g p) <*> (Pure a)        = Procedure (fmap (\ f a1 -> f a1 a) g) p
  m <*> (Command g2 c2)               = Command  (m           <*> g2) c2
  m <*> (Procedure g2 p2)             = Procedure (fmap (.) m <*> g2) p2

class SendApplicative f where
  sendApplicative :: (Monad m) => (f c p ~> m) -> (Packet c p ~> m)


instance SendApplicative Weak.Packet where
  sendApplicative = runWeakApplicative

runWeakApplicative :: forall m c p . (Applicative m) => (Weak.Packet c p ~> m) -> (Packet c p ~> m)
runWeakApplicative f (Command   g c) = runWeakApplicative f g <* f (Weak.Command c)
runWeakApplicative f (Procedure g p) = runWeakApplicative f g <*> f (Weak.Procedure p)
runWeakApplicative f (Pure        a) = pure a

instance SendApplicative Strong.Packet where
  sendApplicative = runStrongApplicative

instance SendApplicative Packet where
  sendApplicative = id


-- promote a Strong packet transport, into an Applicative packet transport.
-- Note this unbundles the Applicative packet, but does provide the Applicative API.
runStrongApplicative :: forall m c p . (Monad m) => (Strong.Packet c p ~> m) -> (Packet c p ~> m)
runStrongApplicative f p = go p $ \ cs a -> do
    f $ cs $ Strong.Pure ()
    return a
  where
    go :: forall a r k b . Packet c p a -> ((forall b . Strong.Packet c p b -> Strong.Packet c p b) -> a -> m b) -> m b
    go (Pure a)        k = k id a
    go (Command g c)   k = go g $ \ cs -> k (cs . Strong.Command c)
    go (Procedure g p) k = go g $ \ cs r -> do
        a <- f $ cs $ Strong.Procedure p
        k id (r a)


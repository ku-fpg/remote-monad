{-# LANGUAGE GADTs #-}

{-|
Module:      Control.Monad.Remote.Packet.Applicative
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Monad.Remote.Packet.Applicative where

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


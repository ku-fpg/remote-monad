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

module Control.Remote.Monad.Types 
  ( RemoteMonad(..)
  , RemoteApplicative(..)
  ) where


import           Control.Remote.Monad.Packet.Applicative
import           Control.Natural

data RemoteMonad c p a where
   Appl        :: RemoteApplicative c p a -> RemoteMonad c p a
   Bind        :: RemoteApplicative c p a -> (a -> RemoteMonad c p b) -> RemoteMonad c p b
  
instance Functor (RemoteMonad c p) where
  fmap f m = pure f <*> m

instance Applicative (RemoteMonad c p) where
  pure a                = Appl (pure a)
  Appl f   <*> Appl g   = Appl (f <*> g)
  Appl f   <*> Bind m k = Bind (pure (,) <*> f <*> m) (\ (a,b) -> fmap a $ k b)
  Bind m k <*> r        = Bind m (\ a -> k a <*> r)

instance Monad (RemoteMonad c p) where
  return = pure
  Appl m >>= k    = Bind m k
  Bind m k >>= k2 = Bind m (\ a -> k a >>= k2)
  
  m1 >> m2 = m1 *> m2 -- This improves our bundling opportunities

newtype RemoteApplicative c p a = RemoteApplicative (ApplicativePacket c p a)

instance Functor (RemoteApplicative c p) where
  fmap f (RemoteApplicative g) = RemoteApplicative (fmap f g)

instance Applicative (RemoteApplicative c p) where
  pure a = RemoteApplicative (pure a)
  (RemoteApplicative f) <*> (RemoteApplicative g) = RemoteApplicative (f <*> g)

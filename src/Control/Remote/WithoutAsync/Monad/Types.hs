{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module:      Control.Remote.WithoutAsync.Monad.Types
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.WithoutAsync.Monad.Types 
  ( RemoteMonad(..)
  ) where


import            Control.Natural
import            Control.Monad.Catch
import            Control.Applicative
import            Data.Typeable
import            Control.Monad.Trans.Class

import            Control.Remote.WithoutAsync.Applicative.Types

-- | 'RemoteMonad' is our monad that can be executed in a remote location.
data RemoteMonad  (q:: * -> *) a where
   Appl        :: RemoteApplicative q a -> RemoteMonad q a
   Bind        :: RemoteMonad q a -> (a -> RemoteMonad q b) -> RemoteMonad q b
   Ap'         :: RemoteMonad q (a -> b) -> RemoteMonad  q a -> RemoteMonad q b
   Alt'        :: RemoteMonad q a -> RemoteMonad q a -> RemoteMonad q a
   Empty'      :: RemoteMonad q a 
   Throw       :: Exception e => e -> RemoteMonad q a
   Catch       :: Exception e => RemoteMonad q a -> (e -> RemoteMonad q a)-> RemoteMonad q a
  
instance  Functor (RemoteMonad q) where
  fmap f m = pure f <*> m

instance  Applicative (RemoteMonad q) where
  pure a                = Appl (pure a)
  Appl f   <*> Appl g   = Appl (f <*> g)
  f        <*> g        = Ap' f g

instance Monad (RemoteMonad q) where
  return      = pure
  m >>= k     = Bind m k
  Empty' >> m2 = Empty'
  m1 >> m2    = m1 *> m2 -- This improves our bundling opportunities

instance MonadThrow (RemoteMonad q) where
    throwM e = Throw e

instance MonadCatch (RemoteMonad q) where
    catch m f = Catch m f

instance Alternative (RemoteMonad q) where
    empty        = Empty'
    Empty' <|> p = p
    Appl g <|> Appl h = Appl (g <|> h)
    m1 <|> m2    = Alt' m1 m2


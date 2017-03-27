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
data RemoteMonad  (proc:: * -> *) a where
   Appl        :: RemoteApplicative proc a -> RemoteMonad proc a
   Bind        :: RemoteMonad proc a -> (a -> RemoteMonad proc b) -> RemoteMonad proc b
   Ap'         :: RemoteMonad proc (a -> b) -> RemoteMonad  proc a -> RemoteMonad proc b
   Alt'        :: RemoteMonad proc a -> RemoteMonad proc a -> RemoteMonad proc a
   Empty'      :: RemoteMonad proc a 
   Throw       :: Exception e => e -> RemoteMonad proc a
   Catch       :: Exception e => RemoteMonad proc a -> (e -> RemoteMonad proc a)-> RemoteMonad proc a
  
instance  Functor (RemoteMonad proc) where
  fmap f m = pure f <*> m

instance  Applicative (RemoteMonad proc) where
  pure a                = Appl (pure a)
  Appl f   <*> Appl g   = Appl (f <*> g)
  f        <*> g        = Ap' f g

instance Monad (RemoteMonad proc) where
  return      = pure
  m >>= k     = Bind m k
  Empty' >> m2 = Empty'
  m1 >> m2    = m1 *> m2 -- This improves our bundling opportunities

instance MonadThrow (RemoteMonad proc) where
    throwM e = Throw e

instance MonadCatch (RemoteMonad proc) where
    catch m f = Catch m f

instance Alternative (RemoteMonad proc) where
    empty        = Empty'
    Empty' <|> p = p
    Appl g <|> Appl h = Appl (g <|> h)
    m1 <|> m2    = Alt' m1 m2


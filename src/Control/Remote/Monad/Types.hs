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
  ) where


import            Control.Natural
import            Control.Monad.Catch
import            Control.Applicative
import            Control.Remote.Applicative.Types
import            Control.Remote.Types
import            Control.Monad.Trans.Class

-- | 'RemoteMonad' is our monad that can be executed in a remote location.
data RemoteMonad  (cmd:: *) (proc:: * -> *) a where
   Appl        :: RemoteApplicative cmd proc a -> RemoteMonad cmd proc a
   Bind        :: RemoteMonad cmd proc a -> (a -> RemoteMonad cmd proc b) -> RemoteMonad cmd proc b
   Ap'         :: RemoteMonad cmd proc (a -> b) -> RemoteMonad cmd proc a -> RemoteMonad cmd proc b
   Alt'        :: RemoteMonad cmd proc a -> RemoteMonad cmd proc a -> RemoteMonad cmd proc a
   Empty'      :: RemoteMonad cmd proc a 
   Throw       :: Exception e => e -> RemoteMonad cmd proc a
   Catch       :: Exception e => RemoteMonad cmd proc a -> (e -> RemoteMonad cmd proc a)-> RemoteMonad cmd proc a
   If          :: RemoteMonad cmd proc Bool -> RemoteMonad cmd proc a ->
                  RemoteMonad cmd proc a -> RemoteMonad cmd proc a
instance  Functor (RemoteMonad cmd proc) where
  fmap f m = pure f <*> m

instance  Applicative (RemoteMonad cmd proc) where
  pure a                = Appl (pure a)
  Appl f   <*> Appl g   = Appl (f <*> g)
  f        <*> g        = Ap' f g

instance Monad (RemoteMonad cmd proc) where
  return       = pure
  m >>= k      = Bind m k
  Empty' >> m2 = Empty'
  m1 >> m2     = m1 *> m2 -- This improves our bundling opportunities

instance MonadThrow (RemoteMonad cmd proc) where
    throwM e = Throw e

instance MonadCatch (RemoteMonad cmd proc) where
    catch m f = Catch m f

instance Alternative (RemoteMonad cmd proc) where
    empty        = Empty'
    Empty' <|> p = p
    Appl g <|> Appl h = Appl (g <|> h)
    m1 <|> m2    = Alt' m1 m2


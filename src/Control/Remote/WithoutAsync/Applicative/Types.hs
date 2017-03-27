{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module:      Control.Remote.WithoutAsync.Applicative.Types
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.WithoutAsync.Applicative.Types 
  ( RemoteApplicative(..)
  ) where


import            Control.Natural
import            Control.Monad.Catch
import            Control.Applicative
import            Data.Typeable
import            Control.Monad.Trans.Class

-- | 'RemoteApplicative' is our applicative that can be executed in a remote location.
data RemoteApplicative (proc:: * -> *) a where 
   Procedure :: proc a                   -> RemoteApplicative proc a
   Alt       :: RemoteApplicative proc a 
             -> RemoteApplicative proc a -> RemoteApplicative proc a
   Ap        :: RemoteApplicative proc (a -> b) 
             -> RemoteApplicative proc a -> RemoteApplicative proc b
   Pure      :: a                        -> RemoteApplicative proc a  
   Empty     ::                             RemoteApplicative proc a
  
instance Functor (RemoteApplicative proc) where
  fmap f g = pure f <*> g

instance Applicative (RemoteApplicative proc) where   -- may need m to be restricted to Monad here
  pure a = Pure a
  (<*>) = Ap

instance Alternative (RemoteApplicative proc) where
   empty       = Empty
   Empty <|> p = p
   m1 <|> m2   = Alt m1 m2

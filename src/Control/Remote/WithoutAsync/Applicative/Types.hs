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
data RemoteApplicative (q :: * -> *) a where 
   Query :: q a                   -> RemoteApplicative q a
   Alt   :: RemoteApplicative q a 
         -> RemoteApplicative q a -> RemoteApplicative q a
   Ap    :: RemoteApplicative q (a -> b) 
         -> RemoteApplicative q a -> RemoteApplicative q b
   Pure  :: a                     -> RemoteApplicative q a  
   Empty ::                          RemoteApplicative q a
  
instance Functor (RemoteApplicative q) where
  fmap f g = pure f <*> g

instance Applicative (RemoteApplicative q) where   -- may need m to be restricted to Monad here
  pure a = Pure a
  (<*>) = Ap

instance Alternative (RemoteApplicative q) where
   empty       = Empty
   Empty <|> q = q
   m1 <|> m2   = Alt m1 m2

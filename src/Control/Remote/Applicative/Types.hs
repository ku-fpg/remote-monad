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

module Control.Remote.Applicative.Types 
  ( RemoteApplicative(..)
  , Wrapper(..)
  ) where


import            Control.Natural
import            Control.Monad.Catch
import            Control.Applicative
import            Data.Typeable
import            Control.Monad.Trans.Class

-- | 'RemoteApplicative' is our applicative that can be executed in a remote location.
data RemoteApplicative (cmd:: *) (proc:: * -> *) a where 
   Command   :: cmd   -> RemoteApplicative cmd proc () 
   Procedure :: proc a -> RemoteApplicative cmd proc a
   Alt       :: RemoteApplicative cmd proc a -> RemoteApplicative cmd proc a -> RemoteApplicative cmd proc a
   Ap        :: RemoteApplicative cmd proc (a -> b) -> RemoteApplicative cmd proc a -> RemoteApplicative cmd proc b
   Pure      :: a   -> RemoteApplicative cmd proc a  
   Empty     :: RemoteApplicative cmd proc a
  
instance Functor (RemoteApplicative cmd proc) where
  fmap f g = pure f <*> g

instance Applicative (RemoteApplicative cmd proc) where   -- may need m to be restricted to Monad here
  pure a = Pure a
  (<*>) = Ap

instance Alternative (RemoteApplicative cmd proc) where
   empty       = Empty
   Empty <|> p = p
   m1 <|> m2   = Alt m1 m2
   
                                                  
data Wrapper f a where
    Value :: f a -> Wrapper f a
    Throw' :: f () -> Wrapper f a

instance Applicative f => Functor (Wrapper f) where
    fmap f g = (pure f)<*> g
    
instance Applicative f => Applicative (Wrapper f) where
    pure a = Value $ pure a
    (Value f) <*> (Value g) = Value (f <*> g)
    (Throw' f) <*> g = Throw' f 
    (Value f)  <*> (Throw' g) = Throw' (f *> g)
    
instance Applicative f => Alternative (Wrapper f) where
     empty = Throw' (pure ()) 
     (Throw' g) <|> (Value h) = Value (g *> h)
     (Throw' g) <|> (Throw' h) = Throw' (g *> h)
     (Value g)  <|> _ = Value g

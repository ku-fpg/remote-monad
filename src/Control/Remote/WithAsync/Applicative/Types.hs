{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module:      Control.Remote.WithAsync.Applicative.Types
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.WithAsync.Applicative.Types 
  ( RemoteApplicative(..)
  , CP(..)
  ) where


import            Control.Natural
import            Control.Monad.Catch
import            Control.Applicative
import            Data.Typeable
import            Control.Monad.Trans.Class
import            Control.Remote.WithAsync.Util

data CP (c :: *) (p :: * -> *) a where
  Cmd   :: c   -> CP c p ()
  Proc  :: p a -> CP c p a

-- | 'RemoteApplicative' is our applicative that can be executed in a remote location.
data RemoteApplicative (cp :: * -> (* -> *) -> * -> *) a where 
   Procedure :: cp c p a -> RemoteApplicative cp a
   Alt       :: RemoteApplicative cp a -> RemoteApplicative cp a -> RemoteApplicative cp a
   Ap        :: RemoteApplicative cp (a -> b) -> RemoteApplicative cp a -> RemoteApplicative cp b
   Pure      :: a   -> RemoteApplicative cp a  
   Empty     :: RemoteApplicative cp a
  
instance Functor (RemoteApplicative p) where
  fmap f g = pure f <*> g

instance Applicative (RemoteApplicative p) where
  pure a = Pure a
  (<*>) = Ap

instance Alternative (RemoteApplicative p) where
   empty       = Empty
   Empty <|> p = p
   m1 <|> m2   = Alt m1 m2

instance Result (CP c p) where
    result (Cmd a)  = return ()
    result (Proc p) = fail "unknown result"

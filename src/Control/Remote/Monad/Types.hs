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
  ( RemoteLocalMonad(..)
  , RemoteLocalApplicative(..)
  , RemoteMonadException(..)
  ) where


import            Control.Natural
import            Control.Monad.Catch
import            Control.Applicative
import            Data.Typeable
import            Control.Monad.Trans.Class

-- | 'RemoteMonad' is our monad that can be executed in a remote location.
data RemoteLocalMonad (loc:: * -> *) (cmd:: *) (proc:: * -> *) a where
   Appl        :: RemoteLocalApplicative loc cmd proc a -> RemoteLocalMonad loc cmd proc a
   Bind        :: RemoteLocalMonad loc cmd proc a -> (a -> RemoteLocalMonad loc cmd proc b) -> RemoteLocalMonad loc cmd proc b
   Ap'         :: RemoteLocalMonad loc cmd proc (a -> b) -> RemoteLocalMonad loc cmd proc a -> RemoteLocalMonad loc cmd proc b
   Alt'         :: RemoteLocalMonad loc cmd proc a -> RemoteLocalMonad loc cmd proc a -> RemoteLocalMonad loc cmd proc a
   Empty'       :: RemoteLocalMonad loc cmd proc a 
   Throw       :: Exception e => e -> RemoteLocalMonad loc cmd proc a
   Catch       :: Exception e => RemoteLocalMonad loc cmd proc a -> (e -> RemoteLocalMonad loc cmd proc a)-> RemoteLocalMonad loc cmd proc a
  
instance  Functor (RemoteLocalMonad loc cmd proc) where
  fmap f m = pure f <*> m

instance  Applicative (RemoteLocalMonad loc cmd proc) where
  pure a                = Appl (pure a)
  Appl f   <*> Appl g   = Appl (f <*> g)
  f        <*> g        = Ap' f g

instance Monad (RemoteLocalMonad loc cmd proc) where
  return      = pure
  m >>= k     = Bind m k
  Empty' >> m2 = Empty'
  m1 >> m2    = m1 *> m2 -- This improves our bundling opportunities

instance MonadThrow (RemoteLocalMonad loc cmd proc) where
    throwM e = Throw e

instance MonadCatch (RemoteLocalMonad loc cmd proc) where
    catch m f = Catch m f


instance Alternative (RemoteLocalMonad loc cmd proc) where
    empty        = Empty'
    Empty' <|> p = p
    Appl g <|> Appl h = Appl (g <|> h)
    m1 <|> m2    = Alt' m1 m2

-- | 'RemoteApplicative' is our applicative that can be executed in a remote location.
data RemoteLocalApplicative (loc:: * -> *) (cmd:: *) (proc:: * -> *) a where 
   Command   :: cmd   -> RemoteLocalApplicative loc cmd proc () 
   Procedure :: proc a -> RemoteLocalApplicative loc cmd proc a
   Local     :: loc a -> RemoteLocalApplicative loc cmd proc a
   Alt       :: RemoteLocalApplicative loc cmd proc a -> RemoteLocalApplicative loc cmd proc a -> RemoteLocalApplicative loc cmd proc a
   Ap        :: RemoteLocalApplicative loc cmd proc (a -> b) -> RemoteLocalApplicative loc cmd proc a -> RemoteLocalApplicative loc cmd proc b
   Pure      :: a   -> RemoteLocalApplicative loc cmd proc a  
   Empty     :: RemoteLocalApplicative loc cmd proc a
  
instance Functor (RemoteLocalApplicative loc cmd proc) where
  fmap f g = pure f <*> g

instance Applicative (RemoteLocalApplicative loc cmd proc) where   -- may need m to be restricted to Monad here
  pure a = Pure a
  (<*>) = Ap

instance Alternative (RemoteLocalApplicative loc cmd proc) where
   empty       = Empty
   Empty <|> p = p
   m1 <|> m2   = Alt m1 m2
   
data RemoteMonadException = RemoteEmptyException
   deriving (Show, Typeable)                             
                                                         
instance Exception RemoteMonadException                 
                                                         
{-                                                         
instance Binary RemoteMonadException where              
    put (RemoteEmptyException s) = do put (219:: Word8) 
                                                         
    get = do i <-get                                     
             case i :: Word8 of                          
               219 -> return $ RemoteEmptyException 
-}

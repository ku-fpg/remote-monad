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
  , RemoteMonadException(..)
  ) where


import            Control.Natural
import            Control.Monad.Catch
import            Control.Applicative
import            Data.Typeable

-- | 'RemoteMonad' is our monad that can be executed in a remote location.
data RemoteMonad c p a where
   Appl        :: RemoteApplicative c p a -> RemoteMonad c p a
   Bind        :: RemoteMonad c p a -> (a -> RemoteMonad c p b) -> RemoteMonad c p b
   Ap'         :: RemoteMonad c p (a -> b) -> RemoteMonad c p a -> RemoteMonad c p b
   Alt         :: RemoteMonad c p a -> RemoteMonad c p a -> RemoteMonad c p a
   Empty       :: RemoteMonad c p a 
   Throw       :: Exception e => e -> RemoteMonad c p a
   Catch       :: Exception e => RemoteMonad c p a -> (e -> RemoteMonad c p a)-> RemoteMonad c p a
  
instance Functor (RemoteMonad c p) where
  fmap f m = pure f <*> m

instance Applicative (RemoteMonad c p) where
  pure a                = Appl (pure a)
  Appl f   <*> Appl g   = Appl (f <*> g)
  f        <*> g        = Ap' f g

{-
  Appl f   *> Appl g   = Appl (f *> g)
  Appl f   *> Bind m k = Bind (Appl f *> m) k
  Bind m k *> r        = Bind m (\ a -> k a *> r)

  Appl f   <* Appl g   = Appl (f <* g)

  Appl f   <* Bind m k = Bind (pure (,) <*> Appl f <*> m) (\ (a,b) -> pure a <* k b)
  Bind m k <* r        = Bind m (\ a -> k a <* r)
-}

instance Monad (RemoteMonad c p) where
  return = pure
  m >>= k    = Bind m k
  Empty >> m2 = Empty
  m1 >> m2 = m1 *> m2 -- This improves our bundling opportunities

instance MonadThrow (RemoteMonad c p) where
    throwM e = Throw e

instance MonadCatch (RemoteMonad c p) where
    catch m f = Catch m f


instance Alternative (RemoteMonad c p) where
    empty = Empty
    Empty <|> p = p
    m1 <|> m2 = Alt m1 m2

-- | 'RemoteApplicative' is our applicative that can be executed in a remote location.
data RemoteApplicative c p a where 
   Command   :: c   -> RemoteApplicative c p () 
   Procedure :: p a -> RemoteApplicative c p a
   Ap        :: RemoteApplicative c p (a -> b) -> RemoteApplicative c p a -> RemoteApplicative c p b
   Pure      :: a                                                         -> RemoteApplicative c p a  
  
instance Functor (RemoteApplicative c p) where
  fmap f g = pure f <*> g

instance Applicative (RemoteApplicative c p) where
  pure a = Pure a
  (<*>) = Ap

data RemoteMonadException = RemoteEmptyException
   deriving (Show, Typeable)                             
                                                         
instance Exception RemoteMonadException                 
                                                         
{-                                                         
instance Binary RemoteBinaryException where              
    put (RemoteBinaryException s) = do put (220:: Word8) 
                                       put s             
                                                         
    get = do i <-get                                     
             case i :: Word8 of                          
               220 -> do s <- get                        
                         return $ RemoteBinaryException s
-}

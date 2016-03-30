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
  ( RemoteT(..)
  , RemoteApplicativeT(..)
  , RemoteMonadException(..)
  ) where


import            Control.Natural
import            Control.Monad.Catch
import            Control.Applicative
import            Data.Typeable
import            Control.Monad.Trans.Class
-- | 'RemoteMonad' is our monad that can be executed in a remote location.
data RemoteT c  p m a where
   Appl        :: RemoteApplicativeT c p m a -> RemoteT c p m a
   Bind        :: RemoteT c p m a -> (a -> RemoteT c p m b) -> RemoteT c p m b
   Ap'         :: RemoteT c p m (a -> b) -> RemoteT c p m a -> RemoteT c p m b
   Alt         :: RemoteT c p m a -> RemoteT c p m a -> RemoteT c p m a
   Empty       :: RemoteT c p m a 
   Throw       :: Exception e => e -> RemoteT c p m a
   Catch       :: Exception e => RemoteT c p m a -> (e -> RemoteT c p m a)-> RemoteT c p m a
  
instance (Functor m, Monad m) => Functor (RemoteT c p m) where
  fmap f m = pure f <*> m

instance (Functor m, Monad m) => Applicative (RemoteT c p m) where
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

instance (Monad m) => Monad (RemoteT c p m) where
  return = pure
  m >>= k    = Bind m k
  Empty >> m2 = Empty
  m1 >> m2 = m1 *> m2 -- This improves our bundling opportunities

instance (Monad m) => MonadThrow (RemoteT c p m) where
    throwM e = Throw e

instance (Monad m) => MonadCatch (RemoteT c p m) where
    catch m f = Catch m f


instance (Monad m) => Alternative (RemoteT c p m) where
    empty = Empty
    Empty <|> p = p
    m1 <|> m2 = Alt m1 m2

instance MonadTrans (RemoteT c p) where
   lift m = Appl $ Local m


-- | 'RemoteApplicative' is our applicative that can be executed in a remote location.
data RemoteApplicativeT c p m a where 
   Command   :: c   -> RemoteApplicativeT c p m () 
   Procedure :: p a -> RemoteApplicativeT c p m a
   Local     :: m a -> RemoteApplicativeT c p m a
   Ap        :: RemoteApplicativeT c p m (a -> b) -> RemoteApplicativeT c p m a -> RemoteApplicativeT c p m b
   Pure      :: a   -> RemoteApplicativeT c p m a  
  
instance (Functor m)=>Functor (RemoteApplicativeT c p m) where
  fmap f g = pure f <*> g

instance (Functor m)=>Applicative (RemoteApplicativeT c p m) where   -- may need m to be restricted to Monad here
  pure a = Pure a
  (<*>) = Ap

instance MonadTrans (RemoteApplicativeT c p) where
    lift m = Local m

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

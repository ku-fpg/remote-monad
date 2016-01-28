{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module:      Control.Remote.Monad.Packet.Weak
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.Monad 
  ( -- * The remote monad
    RemoteMonad
    -- * The primitive lift functions
  , command
  , procedure
    -- * The run functions
  , runMonad
  , runWeakMonad
  , runStrongMonad
  , runApplicativeMonad
  , runMonadSkeleton
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

import qualified Control.Remote.Applicative as A
import           Control.Remote.Monad.Packet.Applicative as A
import           Control.Remote.Monad.Packet.Weak as Weak
import           Control.Remote.Monad.Packet.Strong as Strong

import Control.Natural

data RemoteMonad c p a where
   Appl        :: RemoteApplicative c p a -> RemoteMonad c p a
   Bind        :: RemoteApplicative c p a -> (a -> RemoteMonad c p b) -> RemoteMonad c p b
  
instance Functor (RemoteMonad c p) where
  fmap f m = pure f <*> m

instance Applicative (RemoteMonad c p) where
  pure a                = Appl (pure a)
  Appl f   <*> Appl g   = Appl (f <*> g)
  Appl f   <*> Bind m k = Bind (pure (,) <*> f <*> m) (\ (a,b) -> fmap a $ k b)
  Bind m k <*> r        = Bind m (\ a -> k a <*> r)

instance Monad (RemoteMonad c p) where
  return = pure
  Appl m >>= k    = Bind m k
  Bind m k >>= k2 = Bind m (\ a -> k a >>= k2)
  
  m1 >> m2 = m1 *> m2 -- This improves our bundling opportunities

-- | promote a command into the remote monad
command :: c -> RemoteMonad c p ()
command = Appl . A.command

-- | promote a procedure into the remote monad
procedure :: p a -> RemoteMonad c p a
procedure = Appl . A.procedure 

class MonadPacket f where
  -- | This overloaded function chooses the best bundling strategy
  --   based on the type of the handler your provide.
  runMonad :: (Monad m) => (f c p ~> m) -> (RemoteMonad c p ~> m)

instance MonadPacket Weak where
  runMonad = runWeakMonad

instance MonadPacket Strong where
  runMonad = runStrongMonad

instance MonadPacket A.RemoteApplicative where
  runMonad = runApplicativeMonad


-- | This is a remote monad combinator, that takes an implementation
--   of a remote applicative, splits the monad into applicatives
--   without any merge stragegy, and uses the remote applicative.
--   Every '>>=' will generate a call to the 'RemoteApplicative'
--   handler; as well as one terminating call.
--   Using 'runBindeeMonad' with a 'runWeakApplicative' gives the weakest remote monad.
runMonadSkeleton :: (Monad m) => (RemoteApplicative c p ~> m) -> (RemoteMonad c p ~> m)
runMonadSkeleton f (Appl g)   = f g
runMonadSkeleton f (Bind g k) = f g >>= runMonadSkeleton f . k
 
-- | This is the classic weak remote monad, or technically the
--   weak remote applicative weak remote monad.
runWeakMonad :: (Monad m) => (Weak c p ~> m) -> (RemoteMonad c p ~> m)
runWeakMonad f = runMonadSkeleton (runWeakApplicative f)

-- | This is the classic strong remote monad. It bundles
--   packets (of type 'Strong') as large as possible,
--   including over some monadic binds.
runStrongMonad :: forall m c p . (Monad m) => (Strong c p ~> m) -> (RemoteMonad c p ~> m)
runStrongMonad f p = do
    (r,HStrong h) <- runStateT (go2 p) (HStrong id)
    f $ h $ Strong.Done
    return r
  where
    go2 :: forall a . RemoteMonad c p a -> StateT (HStrong c p) m a
    go2 (Appl app)   = go app
    go2 (Bind app k) = go app >>= \ a -> go2 (k a)

    go :: forall a . RemoteApplicative c p a -> StateT (HStrong c p) m a
    go (A.Pure a)        = return a
    go (A.Command g c)   = do
        r <- go g
        modify (\ (HStrong cs) -> HStrong (cs . Strong.Command c))
        return r
    go (A.Procedure g p) = do
        r1 <- go g
        HStrong cs <- get 
        put (HStrong id)
        r2 <- lift $ f $ cs $ Strong.Procedure $ p
        return $ r1 r2

-- | The is the strong applicative strong remote monad. It bundles
--   packets (of type 'RemoteApplicative') as large as possible, 
--   including over some monadic binds.
runApplicativeMonad :: forall m c p . (Monad m) => (A.RemoteApplicative c p ~> m) -> (RemoteMonad c p ~> m)
runApplicativeMonad f p = do
    (r,h) <- runStateT (go2 p) (pure ())
    f $ h
    return r
  where
    go2 :: forall a . RemoteMonad c p a -> StateT (A.RemoteApplicative c p ()) m a
    go2 (Appl app)   = go app
    go2 (Bind app k) = go app >>= \ a -> go2 (k a)

    go :: forall a . A.RemoteApplicative c p a -> StateT (A.RemoteApplicative c p ()) m a
    go ap = case A.superCommand ap of
                Nothing -> do
                  ap' <- get
                  put (pure ())
                  lift $ f $ (ap' *>  ap)
                Just a -> do
                  modify (\ ap' -> ap' <* ap)
                  return a


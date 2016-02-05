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
  , RunMonad(runMonad)
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
import           Control.Remote.Monad.Types

import Control.Natural


-- | promote a command into the remote monad
command :: c -> RemoteMonad c p ()
command = Appl . A.command

-- | promote a procedure into the remote monad
procedure :: p a -> RemoteMonad c p a
procedure = Appl . A.procedure 

-- 'RunMonad' is the overloading for choosing the appropriate bundling strategy for a monad.
class RunMonad f where
  -- | This overloaded function chooses the appropriate bundling strategy
  --   based on the type of the handler your provide.
  runMonad :: (Monad m) => (f c p ~> m) -> (RemoteMonad c p ~> m)

instance RunMonad WeakPacket where
  runMonad = runWeakMonad

instance RunMonad StrongPacket where
  runMonad = runStrongMonad

instance RunMonad ApplicativePacket where
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
runWeakMonad :: (Monad m) => (WeakPacket c p ~> m) -> (RemoteMonad c p ~> m)
runWeakMonad f = runMonadSkeleton (A.runWeakApplicative f)

-- | This is the classic strong remote monad. It bundles
--   packets (of type 'StrongPacket') as large as possible,
--   including over some monadic binds.
runStrongMonad :: forall m c p . (Monad m) => (StrongPacket c p ~> m) -> (RemoteMonad c p ~> m)
runStrongMonad f p = do
    (r,HStrongPacket h) <- runStateT (go2 p) (HStrongPacket id)
    f $ h $ Strong.Done
    return r
  where
    go2 :: forall a . RemoteMonad c p a -> StateT (HStrongPacket c p) m a
    go2 (Appl app)   = go' app
    go2 (Bind app k) = go' app >>= \ a -> go2 (k a)

    go' :: forall a . RemoteApplicative c p a -> StateT (HStrongPacket c p) m a
    go' (RemoteApplicative m) = go m

    go :: forall a . ApplicativePacket c p a -> StateT (HStrongPacket c p) m a
    go (A.Pure a)        = return a
    go (A.Command g c)   = do
        r <- go g
        modify (\ (HStrongPacket cs) -> HStrongPacket (cs . Strong.Command c))
        return r
    go (A.Procedure g p) = do
        r1 <- go g
        HStrongPacket cs <- get 
        put (HStrongPacket id)
        r2 <- lift $ f $ cs $ Strong.Procedure $ p
        return $ r1 r2

-- | The is the strong applicative strong remote monad. It bundles
--   packets (of type 'RemoteApplicative') as large as possible, 
--   including over some monadic binds.
runApplicativeMonad :: forall m c p . (Monad m) => (A.ApplicativePacket c p ~> m) -> (RemoteMonad c p ~> m)
runApplicativeMonad f p = do
    (r,h) <- runStateT (go2 p) (pure ())
    f $ h
    return r
  where
    go2 :: forall a . RemoteMonad c p a -> StateT (A.ApplicativePacket c p ()) m a
    go2 (Appl app)   = go' app
    go2 (Bind app k) = go' app >>= \ a -> go2 (k a)

    go' :: forall a . RemoteApplicative c p a -> StateT (A.ApplicativePacket c p ()) m a
    go' (RemoteApplicative m) = go m

    go :: forall a . ApplicativePacket c p a -> StateT (A.ApplicativePacket c p ()) m a
    go ap = case A.superCommand ap of
                Nothing -> do
                  ap' <- get
                  put (pure ())
                  lift $ f $ (ap' *>  ap)
                Just a -> do
                  modify (\ ap' -> ap' <* ap)
                  return a


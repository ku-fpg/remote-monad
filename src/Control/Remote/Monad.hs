{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
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
    RemoteT
  , RemoteMonad
  , RemoteMonadException(..)
    -- * The primitive lift functions
  , command
  , procedure
  , loop
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
import           Control.Remote.Monad.Types as T
import           Control.Applicative

import Control.Natural
import Control.Monad.Catch
import Control.Monad.Trans.Maybe

type RemoteMonad c p = RemoteT c p IO

-- | promote a command into the remote monad
command :: c -> RemoteT c p m ()
command = Appl . A.command

-- | promote a procedure into the remote monad
procedure :: p a -> RemoteT c p m a
procedure = Appl . A.procedure 

loop :: forall a c p m . (Monad m) => (a-> Bool) -> RemoteT c p m a -> RemoteT c p m a
loop f m = do  res <- m
               if f res then
                 loop f m
                else
                  return res 

-- | 'RunMonad' is the overloading for choosing the appropriate bundling strategy for a monad.
class RunMonad f where
  -- | This overloaded function chooses the appropriate bundling strategy
  --   based on the type of the handler your provide.
  runMonad :: (MonadCatch m) => (f c p :~> m) -> (RemoteT c p m :~> m)

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
runMonadSkeleton :: (MonadCatch m) => (RemoteApplicativeT c p m :~> m) -> (RemoteT c p m :~> m)
runMonadSkeleton f = nat $ \ case 
  Appl g   -> run f g
  Bind g k -> (runMonadSkeleton f # g) >>= \ a -> runMonadSkeleton f # (k a)
  Ap' g h  -> (runMonadSkeleton f # g) <*> (runMonadSkeleton f # h)
  Alt m1 m2 -> (runMonadSkeleton f # m1) 
                  `catch`(\ e-> case e :: RemoteMonadException of
                            RemoteEmptyException -> runMonadSkeleton f # m2
                            _                    -> throwM e
                         )
  Empty     -> throwM RemoteEmptyException
  Throw e   -> throwM e 
  Catch m h -> catch (runMonadSkeleton f # m)  ((runMonadSkeleton f #) . h)

-- | This is the classic weak remote monad, or technically the
--   weak remote applicative weak remote monad.
runWeakMonad :: (MonadCatch m) => (WeakPacket c p :~> m) -> (RemoteT c p m :~> m)
runWeakMonad = runMonadSkeleton . A.runWeakApplicative

-- | This is the classic strong remote monad. It bundles
--   packets (of type 'StrongPacket') as large as possible,
--   including over some monadic binds.
runStrongMonad :: forall m c p . (MonadCatch m) => (StrongPacket c p :~> m) -> (RemoteT c p m:~> m)
runStrongMonad (Nat f) = nat $ \ p -> do
    (r,HStrongPacket h) <- runStateT (runMaybeT (go2 p)) (HStrongPacket id)
    f $ h $ Strong.Done
    case r of 
       Nothing -> throwM RemoteEmptyException
       Just v  -> return v 
  where
    go2 :: forall a . RemoteT c p m a -> MaybeT (StateT (HStrongPacket c p) m) a
    go2 (Appl app)   = lift $ go app
    go2 (Bind app k) = go2 app >>= \ a -> go2 (k a)
    go2 (Ap' g h)    = go2 g <*> go2 h
    go2 (Alt m1 m2)  = go2 m1  <|> go2 m2
    go2 Empty        = empty
    go2 (Throw e)    = lift $ do 
        HStrongPacket cs <-  get
        put (HStrongPacket id) 
        () <- lift $ f $ cs Strong.Done
        throwM e 
    go2 (Catch m h) = catch (go2 m) (go2 . h)

    go :: forall a . T.RemoteApplicativeT c p m a -> StateT (HStrongPacket c p) m a
    go (T.Pure a)      = return a
    go (T.Command c)   = do
        modify (\ (HStrongPacket cs) -> HStrongPacket (cs . Strong.Command c))
        return ()
    go (T.Procedure p) = do
        HStrongPacket cs <- get
        put (HStrongPacket id)
        r2 <- lift $ f $ cs $ Strong.Procedure $ p
        return $ r2
    go (T.Ap g h) = go g <*> go h
    
-- | The is the strong applicative strong remote monad. It bundles
--   packets (of type 'RemoteApplicative') as large as possible, 
--   including over some monadic binds.
runApplicativeMonad :: forall m c p . (MonadCatch m) => (A.ApplicativePacket c p :~> m) -> (RemoteT c p m :~> m)
runApplicativeMonad (Nat f) = nat $ \ p -> do
    (r,h) <-  runStateT (runMaybeT (go2 p)) (pure ()) 
    f $ pk $ h -- should we stub out the call with only 'Pure'?
    case r of
      Nothing -> throwM RemoteEmptyException
      Just v -> return v
  where
    go2 :: forall a . RemoteT c p m a -> MaybeT (StateT (T.RemoteApplicativeT c p m ()) m) a
    go2 (Appl app)   = lift $ go app
    go2 (Bind app k) = go2 app >>= \ a -> go2 (k a)
    go2 (Ap' g h)    = go2 g <*> go2 h
    go2 (Alt m1 m2)  = go2 m1 <|> go2 m2
    go2 Empty        = empty
    go2 (Throw e)    = lift $ do
        ap' <- get
        put (pure ())
        ()<-lift $ f $ (pk ap')
        throwM e
    go2 (Catch m h) = catch (go2 m) (go2 . h)

         

    go :: forall a .  T.RemoteApplicativeT c p m a -> StateT (T.RemoteApplicativeT c p m ()) m a
    go ap = case superApplicative ap of
                Nothing -> do
                  ap' <- get
                  put (pure ())
                  lift $ f $ (pk (ap' *> ap))
                Just a -> do
                  modify (\ ap' -> ap' <* ap)
                  return a

    superApplicative :: T.RemoteApplicativeT c p m a -> Maybe a
    superApplicative (T.Pure a)      = pure a
    superApplicative (T.Command   c) = Just ()
    superApplicative (T.Procedure p) = Nothing
    superApplicative (T.Ap g h)      = superApplicative g <*> superApplicative h


    -- It all comes down to this. Converting quickly between T.RemoteApplicative and ApplicativePacket.
    
    pk :: T.RemoteApplicativeT c p m a -> ApplicativePacket c p a
    pk (T.Pure a)      = A.Pure a
    pk (T.Command   c) = A.Command c
    pk (T.Procedure p) = A.Procedure p
    pk (T.Ap g h)      = A.Zip ($) (pk g) (pk h)

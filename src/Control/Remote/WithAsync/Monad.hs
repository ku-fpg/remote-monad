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

module Control.Remote.WithAsync.Monad
  ( -- * The remote monad
    RemoteMonad
  , RemoteMonadException(..)
    -- * The primitive lift functions
  , command
  , procedure
  , loop
    -- * The run functions
  , RunMonad(runMonad)
  , runWeakMonad
--  , runStrongMonad
  , runApplicativeMonad
  , runAlternativeMonad
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

import qualified Control.Remote.WithAsync.Applicative as A
import           Control.Remote.WithAsync.Packet.Applicative as A
import qualified Control.Remote.WithAsync.Packet.Alternative as Alt
import           Control.Remote.WithAsync.Packet.Weak as Weak
import           Control.Remote.WithAsync.Packet.Strong as Strong
import           Control.Remote.WithAsync.Monad.Types as T
import           Control.Remote.WithAsync.Applicative.Types as AT
import           Control.Remote.WithAsync.Util

import           Control.Applicative
import           Control.Natural
import           Control.Monad.Catch
import           Control.Monad.Trans.Maybe


-- | promote a command into the remote monad
command :: c -> RemoteMonad CP ()
command = Appl . A.command

-- | promote a procedure into the remote monad
procedure :: p a -> RemoteMonad CP a
procedure = Appl . A.procedure

loop :: forall a c p l . (a-> Bool) -> RemoteMonad p a -> RemoteMonad p a
loop f m = do  res <- m
               if f res then
                 loop f m
                else
                  return res

-- | 'RunMonad' is the overloading for choosing the appropriate bundling strategy for a monad.
class RunMonad f where
  -- | This overloaded function chooses the appropriate bundling strategy
  --   based on the type of the handler your provide.
  runMonad :: (MonadCatch m) => (f cp :~> m) -> (RemoteMonad cp :~> m)

instance RunMonad WeakPacket where
  runMonad = runWeakMonad
{-
instance RunMonad StrongPacket where
  runMonad = runStrongMonad
-}
instance RunMonad ApplicativePacket where
  runMonad = runApplicativeMonad

instance RunMonad Alt.AlternativePacket where
  runMonad = runAlternativeMonad

-- | This is a remote monad combinator, that takes an implementation
--   of a remote applicative, splits the monad into applicatives
--   without any merge stragegy, and uses the remote applicative.
--   Every '>>=' will generate a call to the 'RemoteApplicative'
--   handler; as well as one terminating call.
--   Using 'runBindeeMonad' with a 'runWeakApplicative' gives the weakest remote monad.
runMonadSkeleton :: (MonadCatch m) => (RemoteApplicative CP :~> m) -> (RemoteMonad CP :~> m)
runMonadSkeleton f = wrapNT $ \ case
  Appl g   -> unwrapNT f g
  Bind g k -> (runMonadSkeleton f # g) >>= \ a -> runMonadSkeleton f # (k a)
  Ap' g h  -> (runMonadSkeleton f # g) <*> (runMonadSkeleton f # h)
  Alt' m1 m2 -> (runMonadSkeleton f # m1)
                  `catch`(\ e-> case e :: RemoteMonadException of
                            RemoteEmptyException -> runMonadSkeleton f # m2
                         )
  Empty'    -> throwM RemoteEmptyException
  Throw e   -> throwM e
  Catch m h -> catch (runMonadSkeleton f # m)  ((runMonadSkeleton f #) . h)

-- | This is the classic weak remote monad, or technically the
--   weak remote applicative weak remote monad.
runWeakMonad :: (MonadCatch m) => (WeakPacket CP :~> m) -> (RemoteMonad CP :~> m)
runWeakMonad = runMonadSkeleton . A.runWeakApplicative

{-
-- | This is the classic strong remote monad. It bundles
--   packets (of type 'StrongPacket') as large as possible,
--   including over some monadic binds.
runStrongMonad :: forall m c p . (MonadCatch m) => (StrongPacket CP :~> m) -> (RemoteMonad CP :~> m)
runStrongMonad (NT rf) = wrapNT $ \ p -> do
    (r,HStrongPacket h) <- runStateT (runMaybeT (go2 p)) (HStrongPacket id)
    rf $ h $ Strong.Done
    case r of
       Nothing -> throwM RemoteEmptyException
       Just v  -> return v
  where
    go2 :: forall a . RemoteMonad CP a -> MaybeT (StateT (HStrongPacket CP) m) a
    go2 (Appl app)   = go app
    go2 (Bind app k) = go2 app >>= \ a -> go2 (k a)
    go2 (Ap' g h)    = go2 g <*> go2 h
    go2 (Alt' m1 m2) = go2 m1  <|> go2 m2
    go2 Empty'       = empty
    go2 (Throw e)    = lift $ do
        HStrongPacket cs <-  get
        put (HStrongPacket id)
        () <- lift $ rf $ cs Strong.Done
        throwM e
    go2 (Catch m h) = catch (go2 m) (go2 . h)

    go :: forall a . RemoteApplicative CP a -> MaybeT (StateT (HStrongPacket CP) m) a
    go (AT.Pure a)      = return a
    go (AT.Procedure c@(Cmd _))   = lift $ do
        modify (\ (HStrongPacket cs) -> HStrongPacket (cs . (Strong.Procedure $ c)))
        return ()
    go (AT.Procedure p) = lift $ do
        HStrongPacket cs <- get
        put (HStrongPacket id)
        r2 <- lift $ rf $ cs $ Strong.Procedure $ p
        return $ r2
    go (AT.Ap g h) = go g <*> go h
    go (AT.Alt g h) = go g <|> go h
-}
-- | The is the strong applicative strong remote monad. It bundles
--   packets (of type 'RemoteApplicative') as large as possible,
--   including over some monadic binds.
runApplicativeMonad :: forall m . (MonadCatch m) => (A.ApplicativePacket CP :~> m) -> (RemoteMonad CP :~> m)
runApplicativeMonad (NT rf) = wrapNT $ \ p -> do
    (r,h) <-  runStateT (runMaybeT (go2 p)) (pure ())
    case  pk h of -- should we stub out the call with only 'Pure'?
      Pure' a ->  return a
      Pkt f b ->  do res <- rf $ b
                     return $ f res
    case r of
      Nothing -> throwM RemoteEmptyException
      Just v -> return v
  where
    go2 :: forall a . RemoteMonad CP a -> MaybeT (StateT (RemoteApplicative CP ()) m) a
    go2 (Appl app)   = lift $ unwrap $ go app
    go2 (Bind app k) = go2 app >>= \ a -> go2 (k a)
    go2 (Ap' g h)    = go2 g <*> go2 h
    go2 (Alt' m1 m2) = go2 m1 <|> go2 m2
    go2 Empty'       = empty
    go2 (Throw e)    = lift $ do
        ()<-discharge id
        throwM e
    go2 (Catch m h) = catch (go2 m) (go2 . h)

    go :: forall a . AT.RemoteApplicative CP a -> Wrapper (RemoteApplicative CP) a
    go (AT.Empty) = empty
    go (AT.Pure a) = pure a
    go c@(AT.Procedure (Cmd _)) = Value c
    go p@(AT.Procedure (Proc _)) = Value p
    go (AT.Ap g h)      = (go g) <*> (go h)
    go (AT.Alt g h)     = (go g) <|> (go h)

    -- g is a function that will take the current state as input
    discharge :: forall a f . Applicative f => (f () ->RemoteApplicative CP a )-> StateT (f ()) m a
    discharge g = do
                 ap' <- get
                 put (pure ()) -- clear state
                 case pk $ g ap' of
                    Pure' a -> return a
                    Pkt f pkt -> do
                                    res <- lift $ rf pkt
                                    return $ f res
    -- Given a wrapped applicative discharge via local monad
    unwrap :: forall a . Wrapper(RemoteApplicative CP) a -> StateT (RemoteApplicative CP ()) m a
    unwrap (Value ap) = case superApplicative ap of
                            Nothing ->do
                                      discharge $ \ap' -> (ap' *> ap)
                            Just a  ->do
                                       modify (\ap' -> ap' <* ap)
                                       return a

    unwrap (Throw' ap) = do
                         discharge $ \ap' -> (ap' <* ap)
                         throwM RemoteEmptyException

    -- Do we know the answer? Nothing  =  we need to get it
    superApplicative :: RemoteApplicative CP a -> Maybe a
    superApplicative (AT.Pure a)      = pure a
    superApplicative (AT.Procedure (Cmd _)) = Just ()
    superApplicative (AT.Procedure p) = Nothing
    superApplicative (AT.Ap g h)      =  (superApplicative g) <*> (superApplicative h)
    superApplicative (AT.Alt g h)     = Nothing
    superApplicative (AT.Empty)       = Nothing

    -- Either A or a Packet to return A
    pk :: RemoteApplicative CP a -> X CP a
    pk (AT.Pure a)      = Pure' a
    pk (AT.Procedure  c@(Cmd _)) = Pkt id $ A.Procedure c
    pk (AT.Procedure p) = Pkt id $ A.Procedure p
    pk (AT.Ap g h)      = case (pk g, pk h) of
                           (Pure' a, Pure' b)   -> Pure' (a b)
                           (Pure' a, Pkt f b)   -> Pkt (\b' -> a (f b')) b
                           (Pkt f a, Pure' b)   -> Pkt (\a' -> f a' b) a
                           (Pkt f a, Pkt g b)   -> Pkt id $ A.Zip (\ a' b' -> f a' (g b')) a b
data X cp a where
   Pure' :: a -> X cp a
   Pkt  :: (a -> b) -> ApplicativePacket cp a -> X cp b

runAlternativeMonad :: forall m . (MonadCatch m) => (Alt.AlternativePacket CP :~> m) -> (RemoteMonad CP :~> m)
runAlternativeMonad (NT rf) = wrapNT $ \ p -> do
   (r,h) <-  runStateT (runMaybeT (go2 p)) (pure ())
   () <- rf $ pk h
   case r of
      Nothing -> throwM RemoteEmptyException
      Just v -> return v

   where
    go2 :: forall a . RemoteMonad CP a -> MaybeT (StateT (RemoteApplicative CP ()) m) a
    go2 (Appl app)   = lift $ go app
    go2 (Bind app k) = go2 app >>= \ a -> go2 (k a)
    go2 (Ap' g h)    = go2 g <*> go2 h
    go2 (Alt' m1 m2) = go2 m1 <|> go2 m2
    go2 Empty'       = empty
    go2 (Throw e)    = lift $ do
        ()<- discharge id
        throwM e
    go2 (Catch m h) = catch (go2 m) (go2 . h)

    go :: RemoteApplicative CP a -> StateT (RemoteApplicative CP ()) m a
    go ap = case superApplicative ap of
               Nothing -> do
                           discharge $ \ ap' -> ap' *> ap
               Just a  -> do
                             modify (\ap' -> ap' <* ap)
                             return a

    pk :: forall a . RemoteApplicative CP a -> Alt.AlternativePacket CP a
    pk (AT.Empty)       = empty
    pk (AT.Pure a)      = pure a
    pk (AT.Procedure c@(Cmd _))   = (Alt.Procedure c)
    pk (AT.Procedure p) = (Alt.Procedure p)
    pk (AT.Ap g h)      = (pk g) <*> (pk h)
    pk (AT.Alt g h)     = (pk g) <|> (pk h)

    -- g is a function that will take the current state as input
    discharge :: forall a f . Applicative f => (f () ->RemoteApplicative CP a )-> StateT (f ()) m a
    discharge g = do
                 ap' <- get
                 put (pure ()) -- clear state
                 lift $ rf $ pk $ g ap'

    superApplicative :: RemoteApplicative CP a -> Maybe a
    superApplicative (AT.Empty)       = Nothing
    superApplicative (AT.Pure a)      = pure a
    superApplicative (AT.Procedure (Cmd _))   = pure ()
    superApplicative (AT.Procedure p) = Nothing
    superApplicative (AT.Ap g h)      = (superApplicative g) <*> (superApplicative h)
    superApplicative (AT.Alt g h)      = (superApplicative g) <|> (superApplicative h)


{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE KindSignatures      #-}

{-|
Module:      Control.Remote.Monad.Packet.Weak
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.WithoutAsync.Monad
  ( -- * The remote monad
    RemoteMonad
  , RemoteMonadException(..)
    -- * The primitive lift functions
  , query
  , loop
    -- * The run functions
  , RunMonad(runMonad)
  , runWeakMonad
  , runApplicativeMonad
  , runAlternativeMonad
  , runQueryMonad
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

import qualified Control.Remote.WithoutAsync.Applicative as A
import           Control.Remote.WithoutAsync.Packet.Applicative as A
import qualified Control.Remote.WithoutAsync.Packet.Alternative as Alt
import           Control.Remote.WithoutAsync.Packet.Query as Q
import           Control.Remote.WithoutAsync.Packet.Weak as Weak
import           Control.Remote.WithoutAsync.Monad.Types as T
import           Control.Remote.WithoutAsync.Applicative.Types as T
import           Control.Remote.WithoutAsync.Util

import           Control.Applicative
import           Control.Natural
import           Control.Monad.Catch
import           Control.Monad.Trans.Maybe


-- | promote a procedure into the remote monad
query :: q a -> RemoteMonad q a
query = Appl . A.procedure

loop :: forall a q l . (a-> Bool) -> RemoteMonad q a -> RemoteMonad q a
loop f m = do  res <- m
               if f res then
                 loop f m
                else
                  return res

data X (f :: (* -> *) -> * -> *) q a where
   Pure' :: a -> X f q a
   Pkt  :: (a -> b) -> f q a -> X f q b


-- | 'RunMonad' is the overloading for choosing the appropriate bundling strategy for a monad.
class RunMonad f where
  -- | This overloaded function chooses the appropriate bundling strategy
  --   based on the type of the handler your provide.
  runMonad :: (MonadCatch m) => (f q :~> m) -> (RemoteMonad q :~> m)

instance RunMonad WeakPacket where
  runMonad = runWeakMonad

instance RunMonad ApplicativePacket where
  runMonad = runApplicativeMonad

instance RunMonad QueryPacket where
  runMonad = runQueryMonad

instance RunMonad Alt.AlternativePacket where
  runMonad = runAlternativeMonad

-- | This is a remote monad combinator, that takes an implementation
--   of a remote applicative, splits the monad into applicatives
--   without any merge stragegy, and uses the remote applicative.
--   Every '>>=' will generate a call to the 'RemoteApplicative'
--   handler; as well as one terminating call.
--   Using 'runBindeeMonad' with a 'runWeakApplicative' gives the weakest remote monad.
runMonadSkeleton :: (MonadCatch m) => (RemoteApplicative q :~> m) -> (RemoteMonad q :~> m)
runMonadSkeleton f = wrapNT $ \ case
  Appl g     -> unwrapNT f g
  Bind g k   -> (runMonadSkeleton f # g) >>= \ a -> runMonadSkeleton f # (k a)
  Ap' g h    -> (runMonadSkeleton f # g) <*> (runMonadSkeleton f # h)
  Alt' m1 m2 -> (runMonadSkeleton f # m1)
                  `catch`(\ e-> case e :: RemoteMonadException of
                            RemoteEmptyException -> runMonadSkeleton f # m2
                         )
  Empty'    -> throwM RemoteEmptyException
  Throw e   -> throwM e
  Catch m h -> catch (runMonadSkeleton f # m)  ((runMonadSkeleton f #) . h)

-- | This is the classic weak remote monad, or technically the
--   weak remote applicative weak remote monad.
runWeakMonad :: (MonadCatch m) => (WeakPacket q :~> m) -> (RemoteMonad q :~> m)
runWeakMonad = runMonadSkeleton . A.runWeakApplicative


type PreProcessor q = RemoteMonad q :~> RemoteMonad q 

runApplicative :: forall f m q . (MonadCatch m) => (f q :~> m)
               -> (RemoteApplicative q :~> X f q) -> PreProcessor q -> (RemoteMonad q :~> m)
runApplicative (NT rf) (NT pk) (NT reWrite) = wrapNT $ \ q -> do 
    (r,h) <-  runStateT (runMaybeT (go2 (reWrite q))) (pure ())
    case  pk h of -- should we stub out the call with only 'Pure'?
      Pure' a ->  return a
      Pkt f b ->  do res <- rf $ b
                     return $ f res
    case r of
      Nothing -> throwM RemoteEmptyException
      Just v -> return v
  where
    go2 :: forall a . RemoteMonad q a -> MaybeT (StateT (RemoteApplicative q ()) m) a
    go2 (Appl app)   = lift $ unwrap $ go app
    go2 (Bind app k) = go2 app >>= \ a -> go2 (k a)
    go2 (Ap' g h)    = go2 g <*> go2 h
    go2 (Alt' m1 m2) = go2 m1 <|> go2 m2
    go2 Empty'       = empty
    go2 (Throw e)    = lift $ do
        ()<-discharge id
        throwM e
    go2 (Catch m h) = catch (go2 m) (go2 . h)

    go :: forall a . T.RemoteApplicative q a -> Wrapper (RemoteApplicative q) a
    go (T.Empty)   = empty
    go (T.Pure a)  = pure a
    go (T.Query q) = Value (T.Query q)
    go (T.Ap g h)  = (go g) <*> (go h)
    go (T.Alt g h) = (go g) <|> (go h)

    -- g is a function that will take the current state as input
    discharge :: forall a f . Applicative f => (f () ->RemoteApplicative q a )-> StateT (f ()) m a
    discharge g = do
                 ap' <- get
                 put (pure ()) -- clear state
                 case pk $ g ap' of
                    Pure' a -> return a
                    Pkt f pkt -> do
                                    res <- lift $ rf pkt
                                    return $ f res
    -- Given a wrapped applicative discharge via local monad
    unwrap :: forall a . Wrapper(RemoteApplicative q) a -> StateT (RemoteApplicative q ()) m a
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
    superApplicative :: RemoteApplicative q a -> Maybe a
    superApplicative (T.Pure a)    = pure a
    superApplicative (T.Query _q)  = Nothing
    superApplicative (T.Ap g h)    = (superApplicative g) <*> (superApplicative h)
    superApplicative (T.Alt _g _h) = Nothing
    superApplicative (T.Empty)     = Nothing


-- | The is the strong applicative strong remote monad. It bundles
--   packets (of type 'RemoteApplicative') as large as possible,
--   including over some monadic binds.
runApplicativeMonad :: forall m q . (MonadCatch m) => (A.ApplicativePacket q :~> m) -> (RemoteMonad q :~> m)
runApplicativeMonad f = runApplicative f (wrapNT pk) (wrapNT id)
  where
    -- Either A or a Packet to return A
    pk :: RemoteApplicative q a -> X ApplicativePacket q a
    pk (T.Pure a)  = Pure' a
    pk (T.Query q) = Pkt id $ A.Query q
    pk (T.Ap g h)  = case (pk g, pk h) of
                       (Pure' a, Pure' b)   -> Pure' (a b)
                       (Pure' a, Pkt f b)   -> Pkt (\b' -> a (f b')) b
                       (Pkt f a, Pure' b)   -> Pkt (\a' -> f a' b) a
                       (Pkt f a, Pkt g b)   -> Pkt id $ A.Zip (\ a' b' -> f a' (g b')) a b

-- | The is the strong applicative strong remote monad. It bundles
--   packets (of type 'RemoteApplicative') as large as possible,
--   including over some monadic binds.
runQueryMonad :: forall m q . (MonadCatch m) => (Q.QueryPacket q :~> m) -> (RemoteMonad q :~> m)
runQueryMonad f = runApplicative f (wrapNT pk) (wrapNT helper)
  where 
    -- Either A or a Packet to return A
    pk :: RemoteApplicative q a -> X QueryPacket q a
    pk (T.Pure a)  = Pure' a
    pk (T.Query p) = Pkt id $ QueryPacket $ A.Query p
    pk (T.Ap g h)  = case (pk g, pk h) of
                       (Pure' a, Pure' b)   -> Pure' (a b)
                       (Pure' a, Pkt f b)   -> Pkt (\b' -> a (f b')) b
                       (Pkt f a, Pure' b)   -> Pkt (\a' -> f a' b) a
                       (Pkt f (QueryPacket a), Pkt g (QueryPacket b)) -> Pkt id $ QueryPacket $ A.Zip (\ a' b' -> f a' (g b')) a b
                           
    helper:: RemoteMonad q a -> RemoteMonad q a
    helper (Ap' x@(Ap' _ _) y@(Ap' _ _))    = helper x <*> helper y
    helper (Ap' (Bind m1 k1) (Bind m2 k2) ) = liftA2 (,)  (helper m1) (helper m2) >>=
                                                  \(x1,x2) ->helper ( k1 x1) <*> helper (k2 x2)
    helper (Ap' (Bind m1 k1) app)           = liftA2 (,) (helper m1) (helper app) >>=
                                                  \(x1,x2) -> helper (k1 x1) <*> (pure x2)
    helper (Ap' (Ap' app (Bind m1 k1))   (Bind m2 k2))  =
      liftA3 (,,) (helper app) (helper m1) (helper  m2) >>=
          \(x1,x2,x3) -> (pure x1 <*> k1 x2) <*> helper (k2 x3)
    helper (Bind m k) =  (helper m) >>= \ x -> helper (k x)
    helper x = x

runAlternativeMonad :: forall m q . (MonadCatch m) => (Alt.AlternativePacket q :~> m) -> (RemoteMonad q :~> m)
runAlternativeMonad (NT rf) = wrapNT $ \ q -> do
   (r,h) <-  runStateT (runMaybeT (go2 q)) (pure ())
   () <- rf $ pk h
   case r of
      Nothing -> throwM RemoteEmptyException
      Just v -> return v

   where
    go2 :: forall a . RemoteMonad q a -> MaybeT (StateT (RemoteApplicative  q ()) m) a
    go2 (Appl app)   = lift $ go app
    go2 (Bind app k) = go2 app >>= \ a -> go2 (k a)
    go2 (Ap' g h)    = go2 g <*> go2 h
    go2 (Alt' m1 m2) = go2 m1 <|> go2 m2
    go2 Empty'       = empty
    go2 (Throw e)    = lift $ do
        ()<- discharge id
        throwM e
    go2 (Catch m h) = catch (go2 m) (go2 . h)

    go :: RemoteApplicative q a -> StateT (RemoteApplicative q ()) m a
    go ap = case superApplicative ap of
               Nothing -> do
                           discharge $ \ ap' -> ap' *> ap
               Just a  -> do
                             modify (\ap' -> ap' <* ap)
                             return a

    pk :: forall a . RemoteApplicative q a -> Alt.AlternativePacket q a
    pk (T.Empty)   = empty
    pk (T.Pure a)  = pure a
    pk (T.Query q) = (Alt.Query q)
    pk (T.Ap g h)  = (pk g) <*> (pk h)
    pk (T.Alt g h) = (pk g) <|> (pk h)

    -- g is a function that will take the current state as input
    discharge :: forall a f . Applicative f => (f () ->RemoteApplicative q a )-> StateT (f ()) m a
    discharge g = do
                 ap' <- get
                 put (pure ()) -- clear state
                 lift $ rf $ pk $ g ap'

    superApplicative :: RemoteApplicative q a -> Maybe a
    superApplicative (T.Empty)    = Nothing
    superApplicative (T.Pure a)   = pure a
    superApplicative (T.Query _q) = Nothing
    superApplicative (T.Ap g h)   = (superApplicative g) <*> (superApplicative h)
    superApplicative (T.Alt g h)  = (superApplicative g) <|> (superApplicative h)

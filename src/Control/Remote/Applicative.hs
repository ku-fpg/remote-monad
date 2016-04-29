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

module Control.Remote.Applicative 
  ( -- * The remote applicative
    RemoteLocalApplicative
  , RemoteApplicative
    -- * The primitive lift functions
  , command
  , procedure
  , local
    -- * The run functions
  , RunApplicative(runLocalApplicative)
  , runApplicative
  , runWeakApplicative
  , runStrongApplicative
  , runApplicativeApplicative
--  , runAlternativeApplicative
  , debugApplicative
  ) where


import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Monad.Identity
import Control.Category ((>>>))

import           Control.Remote.Monad.Packet.Applicative as A
import           Control.Remote.Monad.Packet.Alternative as Alt
import qualified Control.Remote.Monad.Packet.Strong as Strong
import           Control.Remote.Monad.Packet.Strong (StrongPacket, HStrongPacket(..))
import qualified Control.Remote.Monad.Packet.Weak as Weak
import           Control.Remote.Monad.Packet.Weak (WeakPacket)
import           Control.Remote.Monad.Types as T
import           Control.Natural
import           Control.Applicative
import           Control.Monad.Catch
import           Control.Monad.Trans.Maybe

type RemoteApplicative = RemoteLocalApplicative Identity 

-- | promote a command into the applicative
command :: c -> RemoteLocalApplicative l c p ()
command c = T.Command c

-- | promote a command into the applicative
procedure :: p a -> RemoteLocalApplicative l c p a
procedure p = T.Procedure p

local :: l a -> RemoteLocalApplicative l c p a
local l = T.Local l

-- | 'RunApplicative' is the overloading for choosing the appropriate bundling strategy for applicative.
class RunApplicative f where
  -- | This overloaded function chooses the appropriate bundling strategy
  --   based on the type of the handler your provide.
  runLocalApplicative :: (MonadThrow m) => (l :~> m)->(f c p :~> m) -> (RemoteLocalApplicative l c p:~> m)

instance RunApplicative WeakPacket where
  runLocalApplicative = runWeakApplicative

instance RunApplicative StrongPacket where
  runLocalApplicative = runStrongApplicative

instance RunApplicative ApplicativePacket where
  runLocalApplicative = runApplicativeApplicative

runApplicative :: (RunApplicative f, MonadThrow m) => (f c p :~> m) -> (RemoteApplicative c p :~> m)
runApplicative = runLocalApplicative $ nat (return . runIdentity)

-- | The weak remote applicative, that sends commands and procedures piecemeal.
runWeakApplicative :: forall m c p l . (MonadThrow m) => (l :~> m) -> (WeakPacket c p :~> m) -> (RemoteLocalApplicative l c p :~> m)
runWeakApplicative (Nat lf) (Nat rf) = nat $ go  
  where
    go :: forall a . RemoteLocalApplicative l c p a ->  m a
    go p = do r <- runMaybeT (go2 p) 
              case r of  
                Nothing -> throwM RemoteEmptyException
                Just a  -> return a
  
    go2 :: forall a . RemoteLocalApplicative l c p a -> MaybeT m a
    go2 (T.Command   c)  = lift $ rf (Weak.Command c)
    go2 (T.Procedure p)  = lift $ rf (Weak.Procedure p)
    go2 (T.Ap g h)      = go2 g <*> go2 h
    go2 (T.Pure      a) = pure a
    go2 (T.Local     m) = lift $ lf m
    go2 T.Empty         = empty
    go2 (T.Alt g h)     = (go2 g <|> go2 h) 
                          
-- | The strong remote applicative, that bundles together commands.
runStrongApplicative :: forall m c p l . (MonadThrow m) => (l :~> m) -> (StrongPacket c p :~> m) -> (RemoteLocalApplicative l c p :~> m)
runStrongApplicative (Nat lf) (Nat rf) = nat $ \ p -> do
    (r,HStrongPacket h) <- runStateT (runMaybeT (go p)) (HStrongPacket id)
    rf $ h $ Strong.Done
    case r of
      Just a -> return a
      Nothing -> throwM RemoteEmptyException
  where
    go :: forall a . T.RemoteLocalApplicative l c p a -> MaybeT (StateT (HStrongPacket c p) m) a
    go (T.Pure a)      = return a
    go (T.Command c)   = lift $ do
        modify (\ (HStrongPacket cs) -> HStrongPacket (cs . Strong.Command c))
        return ()
    go (T.Procedure p) = lift$ do
        HStrongPacket cs <- get
        put (HStrongPacket id)
        r2 <- lift $ rf $ cs $ Strong.Procedure $ p
        return $ r2
    go (T.Ap g h)      = go g <*> go h
    go (T.Local m)     = lift $ lift $ lf m
    go (T.Alt g h)     = go g <|> go h
    go (T.Empty )      = empty



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



-- | The applicative remote applicative, that is the identity function.
runApplicativeApplicative :: forall m c p l . (MonadThrow m) => (l :~> m) -> (ApplicativePacket c p :~> m) -> (RemoteLocalApplicative l c p :~> m)
runApplicativeApplicative (Nat lf) (Nat rf) = nat (go4 . go3)
  where
    go3 :: forall a . T.RemoteLocalApplicative l c p a -> Wrapper (ApplicativePacket c p) a
    go3 (T.Empty)       = empty
    go3 (T.Pure a)      = pure a
    go3 (T.Command c)   = Value (A.Command c)
    go3 (T.Procedure p) = Value (A.Procedure p)
    go3 (T.Ap g h)      = (go3 g) <*> (go3 h)
    go3 (T.Alt g h)     = (go3 g) <|> (go3 h)

    go4 :: forall a . Wrapper (ApplicativePacket c p) a -> m a
    go4 (Value pkt)  = rf pkt
    go4 (Throw' pkt) = do () <- rf pkt  
                          throwM RemoteEmptyException   

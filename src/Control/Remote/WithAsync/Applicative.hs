{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module:      Control.Remote.WithAsync.Applicative
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.WithAsync.Applicative
  ( -- * The remote applicative
    RemoteApplicative
    -- * The primitive lift functions
  , command
  , procedure
    -- * The run functions
--  , RunApplicative(runApplicative)
  , runWeakApplicative
--  , runStrongApplicative
  , runApplicativeApplicative
  , runAlternativeApplicative
  ) where


import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Monad.Identity
import Control.Category ((>>>))

import           Control.Remote.WithAsync.Packet.Applicative as A
import           Control.Remote.WithAsync.Packet.Alternative as Alt
import qualified Control.Remote.WithAsync.Packet.Strong as Strong
import           Control.Remote.WithAsync.Packet.Strong (StrongPacket, HStrongPacket(..))
import qualified Control.Remote.WithAsync.Packet.Weak as Weak
import           Control.Remote.WithAsync.Packet.Weak (WeakPacket)
import           Control.Remote.WithAsync.Applicative.Types as T
import           Control.Remote.WithAsync.Util as T
import           Control.Natural
import           Control.Applicative
import           Control.Monad.Catch
import           Control.Monad.Trans.Maybe


-- | promote a command into the applicative
command :: c -> RemoteApplicative CP ()
command c = T.Procedure (Cmd c)

-- | promote a command into the applicative
procedure :: p a -> RemoteApplicative CP a
procedure p = T.Procedure (Proc p)


-- | 'RunApplicative' is the overloading for choosing the appropriate bundling strategy for applicative.
class RunApplicative f where

  -- | This overloaded function chooses the appropriate bundling strategy
  --   based on the type of the handler your provide.
  runApplicative :: (MonadThrow m) => (f prim :~> m) -> (RemoteApplicative prim :~> m)


instance RunApplicative WeakPacket where
  runApplicative = runWeakApplicative
{-
instance RunApplicative StrongPacket where
  runApplicative = runStrongApplicative
-}
instance RunApplicative ApplicativePacket where
  runApplicative = runApplicativeApplicative

instance RunApplicative AlternativePacket where
  runApplicative = runAlternativeApplicative

-- | The weak remote applicative, that sends commands and procedures piecemeal.
runWeakApplicative :: forall m prim . (MonadThrow m) => (WeakPacket prim :~> m) -> (RemoteApplicative prim :~> m)
runWeakApplicative (NT rf) = wrapNT $ go
  where
    go :: forall a . RemoteApplicative prim a ->  m a
    go p = do r <- runMaybeT (go2 p)
              case r of
                Nothing -> throwM RemoteEmptyException
                Just a  -> return a

    go2 :: forall a . RemoteApplicative prim a -> MaybeT m a
    go2 (T.Procedure prim)  = lift $ rf (Weak.Procedure prim)
    go2 (T.Ap g h)      = go2 g <*> go2 h
    go2 (T.Pure      a) = pure a
    go2 T.Empty         = empty
    go2 (T.Alt g h)     = (go2 g <|> go2 h)
{-
-- | The strong remote applicative, that bundles together commands.
runStrongApplicative :: forall m p . (MonadThrow m) => (StrongPacket CP :~> m) -> (RemoteApplicative CP :~> m)
runStrongApplicative (NT rf) = wrapNT $ \ p -> do
    (r,HStrongPacket h) <- runStateT (runMaybeT (go p)) (HStrongPacket id)
    rf $ h $ Strong.Done
    case r of
      Just a -> return a
      Nothing -> throwM RemoteEmptyException
  where
    go :: forall a . RemoteApplicative CP a -> MaybeT (StateT (HStrongPacket CP) m) a
    go (T.Pure a)      = return a
    go (T.Procedure c@(Cmd _))   = lift $ do
 --       modify (\ (HStrongPacket cs) -> HStrongPacket (cs . (Strong.Procedure c)))
        return ()
    go (T.Procedure p) = lift$ do
        HStrongPacket cs <- get
        put (HStrongPacket id)
        r2 <- lift $ rf $ cs $ Strong.Procedure $ p
        return $ r2
    go (T.Ap g h)      = go g <*> go h
    go (T.Alt g h)     = go g <|> go h
    go (T.Empty )      = empty
-}

-- | The applicative remote applicative, that is the identity function.
runApplicativeApplicative :: forall m prim . (MonadThrow m) => (ApplicativePacket prim :~> m) -> (RemoteApplicative prim :~> m)
runApplicativeApplicative (NT rf) = wrapNT (go4 . go3)
  where
    go3 :: forall a . RemoteApplicative prim a -> Wrapper (ApplicativePacket prim) a
    go3 (T.Empty)       = empty   --uses Throw'
    go3 (T.Pure a)      = pure a
--    go3 (T.Procedure c@(Cmd _))   = Value (A.Procedure c)
    go3 (T.Procedure p) = Value (A.Procedure p)
    go3 (T.Ap g h)      = (go3 g) <*> (go3 h)
    go3 (T.Alt g h)     = (go3 g) <|> (go3 h)

    go4 :: forall a . Wrapper (ApplicativePacket prim) a -> m a
    go4 (Value pkt)  = rf pkt
    go4 (Throw' pkt) = do () <- rf pkt
                          throwM RemoteEmptyException



runAlternativeApplicative :: forall m prim . (MonadThrow m) => (AlternativePacket prim :~> m) -> (RemoteApplicative prim :~> m)
runAlternativeApplicative (NT rf) = wrapNT $ \p ->  rf $ go p
   where
      go :: forall a . RemoteApplicative prim a -> AlternativePacket prim a
      go (T.Empty)               = Alt.Empty
      go (T.Pure a)              = pure a
--      go (T.Procedure c@(Cmd _)) = Alt.Procedure c
      go (T.Procedure p)         = Alt.Procedure p
      go (T.Ap g h)              = (go g) <*> (go h)
      go (T.Alt g h)             = (go g) <|> (go h)


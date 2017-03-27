{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module:      Control.Remote.WithoutAsync.Applicative
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.WithoutAsync.Applicative
  ( -- * The remote applicative
    RemoteApplicative
    -- * The primitive lift functions
  , procedure
    -- * The run functions
  , RunApplicative(runApplicative)
  , runWeakApplicative
  , runApplicativeApplicative
  , runAlternativeApplicative
  ) where


import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Monad.Identity
import Control.Category ((>>>))

import           Control.Remote.WithoutAsync.Packet.Applicative as A
import           Control.Remote.WithoutAsync.Packet.Alternative as Alt
import qualified Control.Remote.WithoutAsync.Packet.Weak as Weak
import           Control.Remote.WithoutAsync.Packet.Weak (WeakPacket)
import           Control.Remote.WithoutAsync.Applicative.Types as T
import           Control.Remote.WithoutAsync.Util (Wrapper(..), RemoteMonadException(..))
import           Control.Natural
import           Control.Applicative
import           Control.Monad.Catch
import           Control.Monad.Trans.Maybe

-- | promote a command into the applicative
procedure :: p a -> RemoteApplicative  p a
procedure p = T.Procedure p


-- | 'RunApplicative' is the overloading for choosing the appropriate bundling strategy for applicative.
class RunApplicative f where
  -- | This overloaded function chooses the appropriate bundling strategy
  --   based on the type of the handler your provide.
  runApplicative :: (MonadThrow m) => (f p :~> m) -> (RemoteApplicative p:~> m)

instance RunApplicative WeakPacket where
  runApplicative = runWeakApplicative

instance RunApplicative ApplicativePacket where
  runApplicative = runApplicativeApplicative

instance RunApplicative AlternativePacket where
  runApplicative = runAlternativeApplicative

-- | The weak remote applicative, that sends commands and procedures piecemeal.
runWeakApplicative :: forall m p . (MonadThrow m) => (WeakPacket p :~> m) -> (RemoteApplicative p :~> m)
runWeakApplicative (NT rf) = wrapNT $ go
  where
    go :: forall a . RemoteApplicative p a ->  m a
    go p = do r <- runMaybeT (go2 p)
              case r of
                Nothing -> throwM RemoteEmptyException
                Just a  -> return a

    go2 :: forall a . RemoteApplicative p a -> MaybeT m a
    go2 (T.Procedure p)  = lift $ rf (Weak.Procedure p)
    go2 (T.Ap g h)      = go2 g <*> go2 h
    go2 (T.Pure      a) = pure a
    go2 T.Empty         = empty
    go2 (T.Alt g h)     = (go2 g <|> go2 h)


-- | The applicative remote applicative, that is the identity function.
runApplicativeApplicative :: forall m p . (MonadThrow m) => (ApplicativePacket p :~> m) -> (RemoteApplicative p :~> m)
runApplicativeApplicative (NT rf) = wrapNT (go4 . go3)
  where
    go3 :: forall a . RemoteApplicative p a -> Wrapper (ApplicativePacket p) a
    go3 (T.Empty)       = empty   --uses Throw'
    go3 (T.Pure a)      = pure a
    go3 (T.Procedure p) = Value (A.Procedure p)
    go3 (T.Ap g h)      = (go3 g) <*> (go3 h)
    go3 (T.Alt g h)     = (go3 g) <|> (go3 h)

    go4 :: forall a . Wrapper (ApplicativePacket p) a -> m a
    go4 (Value pkt)  = rf pkt
    go4 (Throw' pkt) = do () <- rf pkt
                          throwM RemoteEmptyException



runAlternativeApplicative :: forall m p . (MonadThrow m) => (AlternativePacket p :~> m) -> (RemoteApplicative p :~> m)
runAlternativeApplicative (NT rf) = wrapNT $ \p ->  rf $ go p
   where
      go :: forall a . RemoteApplicative p a -> AlternativePacket p a
      go (T.Empty)       = Alt.Empty
      go (T.Pure a)      = pure a
      go (T.Procedure p) = Alt.Procedure p
      go (T.Ap g h)      = (go g) <*> (go h)
      go (T.Alt g h)     = (go g) <|> (go h)

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
    RemoteApplicative
    -- * The primitive lift functions
  , command
  , procedure
    -- * The run functions
  , RunApplicative(runApplicative)
  , runWeakApplicative
  , runStrongApplicative
  , runApplicativeApplicative
  , runAlternativeApplicative
  ) where


import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

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

-- | promote a command into the applicative
command :: c -> RemoteApplicative c p ()
command c = T.Command c

-- | promote a command into the applicative
procedure :: p a -> RemoteApplicative c p a
procedure p = T.Procedure p

-- | 'RunApplicative' is the overloading for choosing the appropriate bundling strategy for applicative.
class RunApplicative f where
  -- | This overloaded function chooses the appropriate bundling strategy
  --   based on the type of the handler your provide.
  runApplicative :: (MonadCatch m) => (f c p :~> m) -> (RemoteApplicative c p :~> m)

instance RunApplicative WeakPacket where
  runApplicative = runWeakApplicative

instance RunApplicative StrongPacket where
  runApplicative = runStrongApplicative

instance RunApplicative ApplicativePacket where
  runApplicative = runApplicativeApplicative

-- | The weak remote applicative, that sends commands and procedures piecemeal.
runWeakApplicative :: forall m c p . (Applicative m, MonadCatch m) => (WeakPacket c p :~> m) -> (RemoteApplicative c p :~> m)
runWeakApplicative (Nat f) = nat go 
  where
    go :: forall a . RemoteApplicative c p a -> m a
    go (T.Command   c) = f (Weak.Command c)
    go (T.Procedure p) = f (Weak.Procedure p)
    go (T.Ap f g)      = go f <*> go g
    go (T.Pure      a) = pure a
    go T.Empty         = throwM RemoteEmptyException
    go (T.Alt g h)     = do r <- runMaybeT (go3 g <|> go3 h) 
                            case r of  
                              Nothing -> throwM RemoteEmptyException
                              Just a  -> return a
                          
    go3 :: forall a . RemoteApplicative c p a -> (MaybeT m) a
    go3 (T.Command c)      = lift $ f (Weak.Command c)
    go3 (T.Procedure p)    = lift $ f (Weak.Procedure p)
    go3 (T.Ap f g)         = go3 f <*> go3 g
    go3 (T.Pure a)         = pure a
    go3 (T.Alt g h)        = go3 g <|> go3 h
    go3 T.Empty            = empty

-- | The strong remote applicative, that bundles together commands.
runStrongApplicative :: forall m c p . (MonadCatch m) => (StrongPacket c p :~> m) -> (RemoteApplicative c p :~> m)
runStrongApplicative (Nat f) = nat $ \ p -> do
    (r,HStrongPacket h) <- runStateT (runMaybeT (go p)) (HStrongPacket id)
    f $ h $ Strong.Done
    case r of
      Just a -> return a
      Nothing -> throwM RemoteEmptyException
  where
    go :: forall a . T.RemoteApplicative c p a -> MaybeT (StateT (HStrongPacket c p) m) a
    go (T.Pure a)      = return a
    go (T.Command c)   = lift $ do
        modify (\ (HStrongPacket cs) -> HStrongPacket (cs . Strong.Command c))
        return ()
    go (T.Procedure p) = lift$ do
        HStrongPacket cs <- get
        put (HStrongPacket id)
        r2 <- lift $ f $ cs $ Strong.Procedure $ p
        return $ r2
    go (T.Ap f g)      = go f <*> go g 
    go (T.Alt g h)     = go g <|> go h
    go (T.Empty )      = empty


-- | The applicative remote applicative, that is the identity function.
runApplicativeApplicative :: (ApplicativePacket c p :~> m) -> (RemoteApplicative c p :~> m)
runApplicativeApplicative (Nat f) = nat $ f . go
  where
    go :: T.RemoteApplicative c p a -> ApplicativePacket c p a
    go (T.Pure a)      = A.Pure a
    go (T.Command   c) = A.Command  c
    go (T.Procedure p) = A.Procedure p
    go (T.Ap g h)      = A.Zip ($) (go g) (go h)
--    go (T.Alt g h)     = go g <|> go h
--    go (T.Empty)       = empty

runAlternativeApplicative :: (AlternativePacket c p :~> m) -> (RemoteApplicative c p :~> m)
runAlternativeApplicative (Nat f) = nat $ f . go
  where
    go :: T.RemoteApplicative c p a -> AlternativePacket c p a
    go (T.Pure a)      = Alt.Pure a
    go (T.Command   c) = Alt.Command  c
    go (T.Procedure p) = Alt.Procedure p
    go (T.Ap g h)      = Alt.Zip ($) (go g) (go h)
    go (T.Alt g h)     = Alt.Alt (go g)  (go h)
    go (T.Empty)       = empty

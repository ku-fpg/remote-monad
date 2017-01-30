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
  , runIfApplicative
  ) where


import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import           Control.Monad.Identity
import           Control.Category ((>>>))
import           Control.Natural
import           Control.Applicative
import           Control.Monad.Catch
import           Control.Monad.Trans.Maybe


import           Control.Remote.Packet.Applicative as AP
import           Control.Remote.Packet.Alternative as Alt
import           Control.Remote.Packet.If as I
import qualified Control.Remote.Packet.Strong as Strong
import           Control.Remote.Packet.Strong (StrongPacket, HStrongPacket(..))
import qualified Control.Remote.Packet.Weak as Weak
import           Control.Remote.Packet.Weak (WeakPacket)
import           Control.Remote.Applicative.Types as A
import           Control.Remote.Types


-- | promote a command into the applicative
command :: c -> RemoteApplicative c p ()
command c = A.Command c

-- | promote a command into the applicative
procedure :: p a -> RemoteApplicative  c p a
procedure p = A.Procedure p


-- | 'RunApplicative' is the overloading for choosing the appropriate bundling strategy for applicative.
class RunApplicative f where
  -- | This overloaded function chooses the appropriate bundling strategy
  --   based on the type of the handler your provide.
  runApplicative :: (MonadThrow m) => (f c p :~> m) -> (RemoteApplicative c p:~> m)

instance RunApplicative WeakPacket where
  runApplicative = runWeakApplicative

instance RunApplicative StrongPacket where
  runApplicative = runStrongApplicative

instance RunApplicative ApplicativePacket where
  runApplicative = runApplicativeApplicative

instance RunApplicative AlternativePacket where
  runApplicative = runAlternativeApplicative

instance RunApplicative IfPacket where
  runApplicative = runIfApplicative
  
-- | The weak remote applicative, that sends commands and procedures piecemeal.
runWeakApplicative :: forall m c p . (MonadThrow m) => (WeakPacket c p :~> m) -> (RemoteApplicative c p :~> m)
runWeakApplicative (NT rf) = wrapNT $ go
  where
    go :: forall a . RemoteApplicative c p a ->  m a
    go p = do r <- runMaybeT (go2 p)
              case r of
                Nothing -> throwM RemoteEmptyException
                Just a  -> return a

    go2 :: forall a . RemoteApplicative c p a -> MaybeT m a
    go2 (A.Command   c)  = lift $ rf (Weak.Command c)
    go2 (A.Procedure p)  = lift $ rf (Weak.Procedure p)
    go2 (A.Ap g h)      = go2 g <*> go2 h
    go2 (A.Pure      a) = pure a
    go2 A.Empty         = empty
    go2 (A.Alt g h)     = (go2 g <|> go2 h)
    go2 (A.If c a b)    = do
                        c' <- go2 c
                        if c' then
                          go2 a
                         else
                          go2 b

-- | The strong remote applicative, that bundles together commands.
runStrongApplicative :: forall m c p . (MonadThrow m) => (StrongPacket c p :~> m) -> (RemoteApplicative c p :~> m)
runStrongApplicative (NT rf) = wrapNT $ \ p -> do
    (r,HStrongPacket h) <- runStateT (runMaybeT (go p)) (HStrongPacket id)
    rf $ h $ Strong.Done
    case r of
      Just a -> return a
      Nothing -> throwM RemoteEmptyException
  where
    go :: forall a . RemoteApplicative c p a -> MaybeT (StateT (HStrongPacket c p) m) a
    go (A.Pure a)      = return a
    go (A.Command c)   = lift $ do
        modify (\ (HStrongPacket cs) -> HStrongPacket (cs . Strong.Command c))
        return ()
    go (A.Procedure p) = lift$ do
        HStrongPacket cs <- get
        put (HStrongPacket id)
        r2 <- lift $ rf $ cs $ Strong.Procedure $ p
        return $ r2
    go (A.Ap g h)      = go g <*> go h
    go (A.Alt g h)     = go g <|> go h
    go (A.Empty )      = empty
    go (A.If c a b)    = do
        c' <- go c
        if c' then
          go a
        else
          go b

-- | The applicative remote applicative, that is the identity function.
runApplicativeApplicative :: forall m c p . (MonadThrow m) => (ApplicativePacket c p :~> m) -> (RemoteApplicative c p :~> m)
runApplicativeApplicative (NT rf) = wrapNT (go4 . go3)
  where
    go3 :: forall a . RemoteApplicative c p a -> Wrapper (ApplicativePacket c p) a
    go3 (A.Empty)       = empty   --uses Throw'
    go3 (A.Pure a)      = pure a
    go3 (A.Command c)   = Value (AP.Command c)
    go3 (A.Procedure p) = Value (AP.Procedure p)
    go3 (A.Ap g h)      = (go3 g) <*> (go3 h)
    go3 (A.Alt g h)     = (go3 g) <|> (go3 h)
--    go3 (A.If c a b)    = (Value (\c' -> if c' then go3 a else go3 b)) <*> (go3 c )
    
    go4 :: forall a . Wrapper (ApplicativePacket c p) a -> m a
    go4 (Value pkt)  = rf pkt
    go4 (Throw' pkt) = do () <- rf pkt
                          throwM RemoteEmptyException


runAlternativeApplicative :: forall m c p . (MonadThrow m) => (AlternativePacket c p :~> m) -> (RemoteApplicative c p :~> m)
runAlternativeApplicative (NT rf) = wrapNT $ \p ->  rf $ go p
   where
      go :: forall a . RemoteApplicative c p a -> AlternativePacket c p a
      go (A.Empty)       = Alt.Empty
      go (A.Pure a)      = pure a
      go (A.Command c)   = Alt.Command c
      go (A.Procedure p) = Alt.Procedure p
      go (A.Ap g h)      = (go g) <*> (go h)
      go (A.Alt g h)     = (go g) <|> (go h)
 --     go (A.If c a b)    =  (\ c' -> if c' then go a else go b ) <$> (rf $ go c)

runIfApplicative :: forall m c p . (MonadThrow m) => (IfPacket c p :~> m) -> (RemoteApplicative c p :~> m)
runIfApplicative (NT rf) = wrapNT $ \p ->  rf $ go p
   where
      go :: forall a . RemoteApplicative c p a -> IfPacket c p a
      go (A.Empty)       = I.Empty
      go (A.Pure a)      = pure a
      go (A.Command c)   = I.Command c
      go (A.Procedure p) = I.Procedure p
      go (A.Ap g h)      = (go g) <*> (go h)
      go (A.Alt g h)     = (go g) <|> (go h)
      go (A.If c a b)    = I.If (go c) (go a) (go b)
 


{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module:      Control.Remote.Monad.Packet.Applicative
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.Monad.Packet.Applicative
  ( -- * The remote applicative
    RemoteApplicative(..)
    -- * The run functions
  , ApplicativePacket(runApplicative)
  , runWeakApplicative
  , runStrongApplicative
  , runApplicativeApplicative
    -- * Utility
  , superCommand
  ) where


import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

import qualified Control.Remote.Monad.Packet.Strong as Strong
import           Control.Remote.Monad.Packet.Strong (Strong, HStrong(..))
import qualified Control.Remote.Monad.Packet.Weak as Weak
import           Control.Remote.Monad.Packet.Weak (Weak)
import Control.Natural

-- | A Remote Applicative, that can encode both commands and procedures, bundled together.

data RemoteApplicative (c :: *) (p :: * -> *) (a :: *) where
   Command   :: RemoteApplicative c p b        -> c   -> RemoteApplicative c p b
   Procedure :: RemoteApplicative c p (a -> b) -> p a -> RemoteApplicative c p b
   Pure      :: a                                     -> RemoteApplicative c p a 

instance Functor (RemoteApplicative c p) where
  fmap f (Command g c)   = Command (fmap f g) c
  fmap f (Procedure g p) = Procedure (fmap (f .) g) p
  fmap f (Pure a)        = Pure (f a)

instance Applicative (RemoteApplicative c p) where
  pure a = Pure a
  (Pure f) <*> m = fmap f m
  (Command g c)   <*> (Pure a)        = Command (fmap (\ f -> f a) g) c
  (Procedure g p) <*> (Pure a)        = Procedure (fmap (\ f a1 -> f a1 a) g) p
  m <*> (Command g2 c2)               = Command  (m           <*> g2) c2
  m <*> (Procedure g2 p2)             = Procedure (fmap (.) m <*> g2) p2

-- | promote a command into the applicative
command :: c -> RemoteApplicative c p ()
command c = Command (pure ()) c

-- | promote a command into the applicative
procedure :: p a -> RemoteApplicative c p a
procedure p = Procedure (pure id) p

class ApplicativePacket f where
  -- | This overloaded function chooses the best bundling strategy
  --   based on the type of the handler your provide.
  runApplicative :: (Monad m) => (f c p ~> m) -> (RemoteApplicative c p ~> m)

instance ApplicativePacket Weak where
  runApplicative = runWeakApplicative

instance ApplicativePacket Strong where
  runApplicative = runStrongApplicative

instance ApplicativePacket RemoteApplicative where
  runApplicative = runApplicativeApplicative

-- | The weak remote applicative, that sends commands and procedures piecemeal.

runWeakApplicative :: forall m c p . (Applicative m) => (Weak c p ~> m) -> (RemoteApplicative c p ~> m)
runWeakApplicative f (Command   g c) = runWeakApplicative f g <*  f (Weak.Command c)
runWeakApplicative f (Procedure g p) = runWeakApplicative f g <*> f (Weak.Procedure p)
runWeakApplicative f (Pure        a) = pure a

-- | The strong remote applicative, that bundles together commands.
runStrongApplicative :: forall m c p . (Monad m) => (Strong c p ~> m) -> (RemoteApplicative c p ~> m)
runStrongApplicative f p = do
    (r,HStrong h) <- runStateT (go p) (HStrong id)
    f $ h $ Strong.Done
    return r
  where
    go :: forall a . RemoteApplicative c p a -> StateT (HStrong c p) m a
    go (Pure a)        = return a
    go (Command g c)   = do
        r <- go g
        modify (\ (HStrong cs) -> HStrong (cs . Strong.Command c))
        return r
    go (Procedure g p) = do
        r1 <- go g
        HStrong cs <- get 
        put (HStrong id)
        r2 <- lift $ f $ cs $ Strong.Procedure $ p
        return $ r1 r2

-- | The applicative remote applicative, that is the identity function.
runApplicativeApplicative :: (RemoteApplicative c p ~> m) -> (RemoteApplicative c p ~> m)
runApplicativeApplicative = id

-- | This simulates a 'RemoteApplicative', to see if it only contains commands, and if so,
-- returns the static result. The commands still need executed. The term super-command
-- is a play on Hughes' super-combinator terminology.

superCommand :: RemoteApplicative c p a -> Maybe a
superCommand (Pure a)        = Just a
superCommand (Command g _)   = superCommand g
superCommand (Procedure _ _) = Nothing

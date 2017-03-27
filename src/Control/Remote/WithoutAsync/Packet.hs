{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-|
Module:      Control.Remote.Monad.Packet
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.WithoutAsync.Packet
  (
    Promote(..)
  , promoteToApplicative
  , promoteToAlternative
  ) where
import qualified Control.Remote.WithoutAsync.Packet.Weak as Weak
import qualified Control.Remote.WithoutAsync.Packet.Applicative as A
import qualified Control.Remote.WithoutAsync.Packet.Alternative as Alt
import           Control.Natural
import           Control.Applicative

class Promote f where
    promote :: (Applicative m) => (Weak.WeakPacket p :~> m) -> (f p :~> m)

instance Promote A.ApplicativePacket where
   promote f =  promoteToApplicative f


-- | promotes a function that can work over WeakPackets to a function that can work over Alternative Packets
promoteToAlternative :: forall p m . (Alternative m) => (Weak.WeakPacket p :~> m) -> (Alt.AlternativePacket p :~> m)
promoteToAlternative (NT f) =  NT $ alternativeFunc
                   where
                        alternativeFunc :: (Alternative m) => (Alt.AlternativePacket p a -> m a)
                        alternativeFunc (Alt.Procedure p) = f (Weak.Procedure p)
                        alternativeFunc (Alt.Zip f1 a b) =  f1 <$> alternativeFunc a <*> alternativeFunc b
                        alternativeFunc (Alt.Alt a b) =  alternativeFunc a <|> alternativeFunc b
                        alternativeFunc (Alt.Pure a) = pure a


-- | promotes a function that can work over WeakPackets to a function that can work over Applicative Packets
promoteToApplicative :: forall p m . (Applicative m) => (Weak.WeakPacket p :~> m) -> (A.ApplicativePacket p :~> m)
promoteToApplicative (NT f) =  NT $ applicativeFunc
                    where
                        applicativeFunc :: (Applicative m) => (A.ApplicativePacket p a -> m a)
                        applicativeFunc (A.Procedure p) = f (Weak.Procedure p)
                        applicativeFunc (A.Zip f1 a b) =  f1 <$> applicativeFunc a <*> applicativeFunc b
                        applicativeFunc (A.Pure a) = pure a


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

{-|
Module:      Control.Remote.WithAsync.Monad.Packet.Weak
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.WithAsync.Packet.Weak where

import Control.Remote.WithAsync.Packet.Transport 

data WeakPacket (cp :: * -> (* -> *) -> * -> *) (a :: *) where
   Procedure :: cp c p a -> WeakPacket cp a

--deriving instance (Show (cp c p a)) => Show (WeakPacket cp a)

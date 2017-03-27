{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

{-|
Module:      Control.Remote.WithoutAsync.Monad.Packet.Weak
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.WithoutAsync.Packet.Weak where

import Control.Remote.WithoutAsync.Packet.Transport 

-- | A Weak Packet, that can encode a command or a procedure.

data WeakPacket (p :: * -> *) (a :: *) where
   Procedure :: p a -> WeakPacket p a

deriving instance Show (p a) => Show (WeakPacket p a)

instance Read (Transport p) => Read (Transport (WeakPacket p)) where
  readsPrec d = readParen (d > 10) $ \ r0 ->
        [ (Transport $ Procedure p,r2)
        | ("Procedure",r1) <- lex r0
        , (Transport p,r2) <- readsPrec 11 r1
        ] 

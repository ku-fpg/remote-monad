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

data WeakPacket (q :: * -> *) (a :: *) where
   Query :: q a -> WeakPacket q a

deriving instance Show (q a) => Show (WeakPacket q a)

instance Read (Transport q) => Read (Transport (WeakPacket q)) where
  readsPrec d = readParen (d > 10) $ \ r0 ->
        [ (Transport $ Query q,r2)
        | ("Query",r1) <- lex r0
        , (Transport q,r2) <- readsPrec 11 r1
        ] 

{-# LANGUAGE GADTs #-}

{-|
Module:      Control.Monad.Remote.Packet.Weak
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Monad.Remote.Packet.Weak where

-- A Weak Packet, that can encode a command, or a procedure.

data Packet c p a where
   Command   :: c -> Packet c p ()
   Procedure :: p a -> Packet c p a


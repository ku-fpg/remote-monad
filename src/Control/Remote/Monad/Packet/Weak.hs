{-# LANGUAGE GADTs #-}

{-|
Module:      Control.Remote.Monad.Packet.Weak
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.Monad.Packet.Weak where

-- A Weak Packet, that can encode a command, or a procedure.

data Weak c p a where
   Command   :: c -> Weak c p ()
   Procedure :: p a -> Weak c p a


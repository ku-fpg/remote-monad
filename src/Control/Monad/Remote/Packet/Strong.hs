{-# LANGUAGE GADTs #-}

{-|
Module:      Control.Monad.Remote.Packet.Strong
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Monad.Remote.Packet.Strong where

-- A Strong Packet, that can encode a list of commands, terminated by an optional procedure.

data Packet c p a where
   Command   :: c -> Packet c p b -> Packet c p b
   Procedure :: p a               -> Packet c p a
   Pure      :: a                 -> Packet c p a

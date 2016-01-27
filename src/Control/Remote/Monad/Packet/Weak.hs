{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

{-|
Module:      Control.Remote.Monad.Packet.Weak
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.Monad.Packet.Weak where

import Control.Remote.Monad.Packet.Transport 

-- | A Weak Packet, that can encode a command or a procedure.

data Weak (c :: *) (p :: * -> *) (a :: *) where
   Command   :: c   -> Weak c p ()
   Procedure :: p a -> Weak c p a

deriving instance (Show c, Show (p a)) => Show (Weak c p a)

instance Read (Transport (Weak c p)) where
  readsPrec _ r0 = []
{-
        [ (Transport $ Command c p,r2)
        | (c,r1)      <- reads r0
        , (";",r2)    <- lex r1
        , (Transport p,r2) <- reads r2
        ] ++
        [ (Transport $ Procedure_ q, r1)
        | (Transport q,r1) <- reads r0
        ] 
 -}

{-# LANGUAGE GADTs #-}

{-|
Module:      Control.Monad.Remote.Packet.Weak
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Monad.Remote.Local where


newtype Local st m a = Local (st -> m (st,a))

runLocal :: Local st m a -> st -> m (st,a)
runLocal (Local m) = m

instance Functor (Local st m) where
instance Applicative (Local st m) where
instance Monad (Local st m) where    
  

--type Level upper lower m = (lower ~> m) -> (upper ~> m)

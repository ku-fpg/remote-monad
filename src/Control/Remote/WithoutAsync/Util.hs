{-# LANGUAGE GADTs #-}

{-|
Module:      Control.Remote.WithoutAsync.Util
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Justin Dawson (jdawson@ku.edu)
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.WithoutAsync.Util
  ( Wrapper(..)
  , RemoteMonadException(..)
  ) where

import Control.Applicative
import Control.Monad.Catch
import Data.Typeable

value :: f a -> Wrapper f a
value  = Value 
                                                  
data Wrapper f a where
    Value :: f a -> Wrapper f a
    Throw' :: f () -> Wrapper f a

instance Applicative f => Functor (Wrapper f) where
    fmap f g = (pure f)<*> g
    
instance Applicative f => Applicative (Wrapper f) where
    pure a = Value $ pure a
    (Value f) <*> (Value g) = Value (f <*> g)
    (Throw' f) <*> g = Throw' f 
    (Value f)  <*> (Throw' g) = Throw' (f *> g)
    
instance Applicative f => Alternative (Wrapper f) where
     empty = Throw' (pure ()) 
     (Throw' g) <|> (Value h) = Value (g *> h)
     (Throw' g) <|> (Throw' h) = Throw' (g *> h)
     (Value g)  <|> _ = Value g


data RemoteMonadException = RemoteEmptyException
   deriving (Show, Typeable)                             
                                                         
instance Exception RemoteMonadException                 

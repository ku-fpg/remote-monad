{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
 
module Main where

import Control.Natural (nat, (#))
import Control.Remote.Monad
import Control.Remote.Monad.Packet.Weak

data Command :: * where
   Say :: String -> Command

data Procedure :: * -> * where
   Temperature :: Procedure Int

say :: String -> RemoteMonad Command Procedure ()
say s = command (Say s)

temperature :: RemoteMonad Command Procedure Int
temperature = procedure Temperature


runWP ::  WeakPacket Command Procedure a -> IO a 
runWP (Command (Say s))  = print s  
runWP (Procedure Temperature) = return 42

send :: RemoteMonad Command Procedure a -> IO a
send m = runMonad (nat runWP) # m

main :: IO ()
main = send $ do 
         say "Howdy doodly do"     
         say "How about a muffin?" 
         t <- temperature          
         say (show t ++ "F")       


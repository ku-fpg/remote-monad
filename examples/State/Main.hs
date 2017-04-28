{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent
import           Control.Natural
import           Data.ByteString.Lazy (toStrict)
import           Data.Text
import           Data.Text.Lazy (fromStrict)
import           Data.Text.Encoding (decodeUtf8)
import           Data.IORef
import           Network.HTTP.Client
import           Web.Scotty

import           Control.Remote.Monad
import           Control.Remote.Packet.Weak



------------------------------------------------------------
  -- Client Side
------------------------------------------------------------

data MyState a where
  Put :: Text -> MyState ()
  Get ::       MyState FontColor

instance KnownResult MyState where
  knownResult (Put _) = Just ()
  knownResult Get     = Nothing


myGet:: RemoteMonad MyState FontColor
myGet = primitive Get

myPut :: FontColor -> RemoteMonad MyState ()
myPut st = primitive (Put st)

transmissionFunction :: Manager -> (WeakPacket MyState :~> IO)
transmissionFunction mgr = wrapNT transmit
   where
     transmit :: WeakPacket MyState a -> IO a
     transmit (Primitive Get) = do
                                  req <- parseRequest "GET http://localhost:3000/data"
                                  res <- httpLbs req mgr
                                  let bs =  responseBody res
                                  return $ decodeUtf8 $ toStrict bs

     transmit (Primitive (Put x)) = do
                req <- parseRequest $ "PUT http://localhost:3000/data/" ++ unpack x
                _   <- httpLbs req mgr
                return ()





send :: RemoteMonad MyState a -> IO a
send rm = do
         mgr <- newManager defaultManagerSettings
         (unwrapNT $ runMonad (transmissionFunction mgr)) rm

------------------------------------------------------------
  --Server Side
------------------------------------------------------------
data Shape = Circle | Square | Diamond
  deriving (Show)

type FontColor = Text

data StateObject = StateObject { fontColor :: FontColor, remoteOnlyBrushSize :: Int, remoteOnlyBrushType :: Shape}
  deriving (Show)


testStateObject :: StateObject
testStateObject = StateObject "Blue" 3 Circle


routes :: IORef StateObject -> ScottyM ()
routes ref = do

  get "/data" $ do
    val <- liftAndCatchIO $ readIORef ref
    liftAndCatchIO $ putStrLn $ "Get state: " ++ show val
    text $ fromStrict $ fontColor val

  put "/data/:val" $ do
    font <- param "val"
    liftAndCatchIO $ putStrLn $ "Put state: " ++ show font
    liftAndCatchIO $ do
         s <- readIORef ref
         writeIORef ref (s {fontColor=font})


------------------------------------------------------------
--Main Method
------------------------------------------------------------
main :: IO ()
main = do
  ref <- newIORef testStateObject
  -- setup server
  _ <- forkIO $ scotty 3000 (routes ref)
  a <- send $ do
                  before <- myGet
                  myPut "Yellow"
                  after <- myGet
                  return (before,after)
  print a
  return ()

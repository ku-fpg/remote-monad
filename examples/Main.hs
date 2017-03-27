{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Natural
import Control.Remote.WithAsync.Monad
import Control.Remote.WithAsync.Packet.Weak as WP
import Control.Remote.WithAsync.Packet.Strong as SP
import Control.Remote.WithAsync.Packet.Applicative as AP
import Control.Remote.WithAsync.Packet.Alternative as Alt
import Control.Applicative
import Control.Monad.Catch
import Control.Exception hiding (catch)

data Command :: * where
   Say :: String -> Command

data Procedure :: * -> * where
   Temperature :: Procedure Int

say :: String -> RemoteMonad Command Procedure ()
say s = command (Say s)

temperature :: RemoteMonad Command Procedure Int
temperature = procedure Temperature


--Server Side Functions
---------------------------------------------------------
runWP ::  WeakPacket Command Procedure a -> IO a
runWP (WP.Command (Say s))       = print s
runWP (WP.Procedure Temperature) = do
                                 putStrLn "Temp Call"
                                 return 42
---------------------------------------------------------
runSP :: StrongPacket Command Procedure a -> IO a
runSP (SP.Command (Say s) cs) =do
                                print s
                                runSP cs
runSP (SP.Procedure (Temperature)) = do
                                putStrLn "Temp Call"
                                return 42
runSP Done = do --print "End Packet Done"
                return ()
---------------------------------------------------------
runAP :: ApplicativePacket Command Procedure a -> IO a
runAP (AP.Command (Say s)) = print s
runAP (AP.Procedure Temperature) =do
                                    putStrLn "Temp Call"
                                    return 42
runAP (AP.Zip f g h) = do
             f <$> runAP g <*> runAP h
runAP (AP.Pure a) = do
                       putStrLn "Pure"
                       return a
---------------------------------------------------------
runAlt :: AlternativePacket Command Procedure a -> IO a
runAlt (Alt.Command (Say s)) = print s
runAlt (Alt.Procedure Temperature) = do
                                  putStrLn "Temp Call"
                                  return 42
runAlt (Alt.Zip f g h) = f <$> runAlt g <*> runAlt h
runAlt (Alt.Pure a)    = return a
runAlt (Alt g h)       = do
           putStrLn "Alternative"
           a <-(runAlt g) <|> (runAlt h)
           putStrLn "End Alternative"
           return a
runAlt (Alt.Empty) = empty
-----------------------------------------------------------
sendWeak :: RemoteMonad Command Procedure a -> IO a
sendWeak = unwrapNT $ runMonad $ wrapNT (\pkt -> do putStrLn "-----"; runWP pkt)

sendStrong :: RemoteMonad Command Procedure a -> IO a
sendStrong = unwrapNT $ runMonad $ wrapNT (\pkt -> do putStrLn "-----"; runSP pkt)

sendApp :: RemoteMonad Command Procedure a -> IO a
sendApp = unwrapNT $ runMonad $ wrapNT (\pkt -> do putStrLn "-----"; runAP pkt)

sendAlt :: RemoteMonad Command Procedure a -> IO a
sendAlt = unwrapNT $ runMonad $ wrapNT (\pkt -> do putStrLn "-----"; runAlt pkt)
---------------------------------------------------------

main :: IO ()
main = do

        putStrLn "WeakSend\n"
        runTest $ wrapNT sendWeak
        putStrLn "\nStrongSend\n"
        runTest $ wrapNT sendStrong

        putStrLn "\nAppSend\n"
        runTest $ wrapNT sendApp

        putStrLn "\nAltSend\n"
        runTest $ wrapNT sendAlt

--Run Test Suite
runTest :: (RemoteMonad Command Procedure :~> IO)-> IO()
runTest (NT f) = do
               f test
               f testBind
               f testApp
               f testAlt
               (f  testAltException)
                 `catch` (\e -> case e ::RemoteMonadException of
                                       RemoteEmptyException -> putStrLn "Empty Exception Thrown"
                         )
               (f $ testThrowM)
                 `catch` (\e -> case e :: ArithException of
                                  DivideByZero -> putStrLn "Should have sent \"hi\", then given this exception"
                                  _            -> throw e
                         )

               (f $ testThrowM2)
                  `catch` (\e -> case e :: ArithException of
                                  Underflow -> putStrLn "Should have sent temp, then given this exception"
                                  _            -> throw e
                          )

               f testCatch
               f testCatch2

-- Original test case
test :: RemoteMonad Command Procedure ()
test = do
         say "Howdy doodly do"
         say "How about a muffin?"
         t <- temperature
         say (show t ++ "F")

-- Test bind
testBind :: RemoteMonad Command Procedure ()
testBind = say "one" >> say "two" >> temperature >>= say . ("Temperature: " ++) .show

-- test alt
testAlt :: RemoteMonad Command Procedure ()
testAlt = do
    say "three" <|> say "ERROR"
    _ <- say "test1" >> say "test2" >> say "test3" >> temperature <|> temperature
    (say "test1" >> say "test2" >> empty >> say "test3") <|> say "fail"
    say "four" <|> empty
    empty       <|> say "five"
    say "six" >> empty <|> say "seven"
    r <- (do temperature >>= \a -> if a /= 42 then return a else (say "HA" >> empty)) <|> pure 32
    say "Should be 32:"
    say (show r)


--test alt withe empty on both sides
testAltException :: RemoteMonad Command Procedure ()
testAltException = do
    say "finished tests, now testing exception thrown"
    (do say "eight"; _ <- empty; say "shouldn't See me")  --expected exception
       <|> (do say "nine"; _ <- empty; say "AHAHA")


--test throw
testThrowM :: RemoteMonad Command Procedure ()
testThrowM = say "hi" >> throwM DivideByZero

-- test throw in an AP
testThrowM2 :: RemoteMonad Command Procedure ()
testThrowM2 = do
    r <- (+) <$> temperature <*> throwM Underflow
    say (show r)


--test catch random throwM
testCatch :: RemoteMonad Command Procedure ()
testCatch = do (say "going to throw" >> throwM DivideByZero)
                   `catch` (\e -> case e :: ArithException of
                                    DivideByZero -> say "Divided by Zero"
                                    _            -> say "Oops!"
                           )
-- test catching Empty exception
testCatch2 :: RemoteMonad Command Procedure ()
testCatch2 =do
     r <- (do temperature >>= \a -> if a /= 42 then return a else (say "HA" >> empty)) <|> pure 32
        `catch` (\e -> case e :: RemoteMonadException of
                          RemoteEmptyException -> do
                                                  say "Caught Exception in Send"
                                                  temperature
                )
     say (show r)

testApp :: RemoteMonad Command Procedure ()
testApp = do
          r<- add <$> temperature<*>temperature <*> temperature
          say (show r)
     where
            add :: Int -> Int -> Int -> Int
            add x y z= x + y + z


{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators  #-}

module Main where

import           Control.Applicative
import           Control.Exception                 hiding (catch)
import           Control.Monad.Catch
import           Control.Natural
import           Control.Remote.Monad
import           Control.Remote.Packet.Alternative as Alt
import           Control.Remote.Packet.Applicative as AP
import           Control.Remote.Packet.Query       as Q
import           Control.Remote.Packet.Weak        as WP

data Query :: * -> * where
   Temperature ::           Query Int
   Say         :: String -> Query ()

instance KnownResult Query where
  knownResult _ = Nothing

say :: String -> RemoteMonad Query ()
say s = primitive (Say s)

temperature :: RemoteMonad Query Int
temperature = primitive Temperature


--Server Side Functions
---------------------------------------------------------
runWP ::  WeakPacket Query a -> IO a
runWP (WP.Primitive (Say s))     = print s
runWP (WP.Primitive Temperature) = do
                                 putStrLn "Temp Call"
                                 return 42
----------------------------------------------------------
runAP :: ApplicativePacket Query a -> IO a
runAP (AP.Primitive (Say s))     = print s
runAP (AP.Primitive Temperature) = do
                                  putStrLn "Temp Call"
                                  return 42
runAP (AP.Zip f g h)         = f <$> runAP g <*> runAP h
runAP (AP.Pure a)            = do
                                   putStrLn "Pure"
                                   return a
---------------------------------------------------------
runQ :: QueryPacket Query a -> IO a
runQ (Q.Primitive (Say s))     = print s
runQ (Q.Primitive Temperature) = do
                                    putStrLn "Temp Call"
                                    return 42
runQ (Q.Zip f g h)             = f <$> runQ g <*> runQ h
runQ (Q.Pure a)                = do
                                    putStrLn "Pure"
                                    return a
---------------------------------------------------------
runAlt :: AlternativePacket Query a -> IO a
runAlt (Alt.Primitive (Say s))     = print s
runAlt (Alt.Primitive Temperature) = do
                                  putStrLn "Temp Call"
                                  return 42
runAlt (Alt.Zip f g h) = f <$> runAlt g <*> runAlt h
runAlt (Alt.Pure a)    = return a
runAlt (Alt g h)       = do
           putStrLn "Alternative"
           a <- runAlt g <|> runAlt h
           putStrLn "End Alternative"
           return a
runAlt  Alt.Empty = empty
-----------------------------------------------------------
sendWeak :: RemoteMonad Query a -> IO a
sendWeak = unwrapNT $ runMonad $ wrapNT (\pkt -> do putStrLn "-----"; runWP pkt)

sendApp  :: RemoteMonad Query a -> IO a
sendApp  = unwrapNT $ runMonad $ wrapNT (\pkt -> do putStrLn "-----"; runAP pkt)

sendQ    :: RemoteMonad Query a -> IO a
sendQ    = unwrapNT $ runMonad $ wrapNT (\pkt -> do putStrLn "-----"; runQ pkt)

sendAlt  :: RemoteMonad Query a -> IO a
sendAlt  = unwrapNT $ runMonad $ wrapNT (\pkt -> do putStrLn "-----"; runAlt pkt)
---------------------------------------------------------

main :: IO ()
main = do

        putStrLn "WeakSend\n"
        runTest $ wrapNT sendWeak

        putStrLn "\nAppSend\n"
        runTest $ wrapNT sendApp

        putStrLn "\nQuerySend\n"
        runTest $ wrapNT sendQ

        putStrLn "\nAltSend\n"
        runTest $ wrapNT sendAlt

--Run Test Suite
runTest :: (RemoteMonad Query :~> IO)-> IO()
runTest (NT f) = do
               f test
               f testBind
               f testApp
               f testAlt
               (f  testAltException)
                 `catch` (\e -> case e ::RemoteMonadException of
                                       RemoteEmptyException -> putStrLn "Empty Exception Thrown"
                         )
               (f testThrowM)
                 `catch` (\e -> case e :: ArithException of
                                  DivideByZero -> putStrLn "Should have sent \"hi\", then given this exception"
                                  _            -> throw e
                         )

               (f testThrowM2)
                  `catch` (\e -> case e :: ArithException of
                                  Underflow -> putStrLn "Should have sent temp, then given this exception"
                                  _            -> throw e
                          )

               f testCatch
               f testCatch2

-- Original test case
test :: RemoteMonad Query ()
test = do
         say "Howdy doodly do"
         say "How about a muffin?"
         t <- temperature
         say (show t ++ "F")

-- Test bind
testBind :: RemoteMonad Query ()
testBind = say "one" >> say "two" >> temperature >>= say . ("Temperature: " ++) .show

-- test alt
testAlt :: RemoteMonad Query ()
testAlt = do
    say "three" <|> say "ERROR"
    _ <- say "test1" >> say "test2" >> say "test3" >> temperature <|> temperature
    (say "test1" >> say "test2" >> empty >> say "test3") <|> say "fail"
    say "four" <|> empty
    empty       <|> say "five"
    say "six" >> empty <|> say "seven"
    r <- (do temperature >>= \a -> if a /= 42 then return a else say "HA" >> empty) <|> pure 32
    say "Should be 32:"
    say (show r)


--test alt withe empty on both sides
testAltException :: RemoteMonad Query ()
testAltException = do
    say "finished tests, now testing exception thrown"
    (do say "eight"; _ <- empty; say "shouldn't See me")  --expected exception
       <|> (do say "nine"; _ <- empty; say "AHAHA")


--test throw
testThrowM :: RemoteMonad Query ()
testThrowM = say "hi" >> throwM DivideByZero

-- test throw in an AP
testThrowM2 :: RemoteMonad Query ()
testThrowM2 = do
    r <- (+) <$> temperature <*> throwM Underflow
    say (show r)


--test catch random throwM
testCatch :: RemoteMonad Query ()
testCatch = (say "going to throw" >> throwM DivideByZero)
                   `catch` (\e -> case e :: ArithException of
                                    DivideByZero -> say "Divided by Zero"
                                    _            -> say "Oops!"
                           )
-- test catching Empty exception
testCatch2 :: RemoteMonad Query ()
testCatch2 =do
     r <- (temperature >>= \a -> if a /= 42 then return a else say "HA" >> empty) <|> pure 32
        `catch` (\e -> case e :: RemoteMonadException of
                          RemoteEmptyException -> do
                                                  say "Caught Exception in Send"
                                                  temperature
                )
     say (show r)

testApp :: RemoteMonad Query ()
testApp = do
          r<- add <$> temperature<*>temperature <*> temperature
          say (show r)
     where
            add :: Int -> Int -> Int -> Int
            add x y z= x + y + z


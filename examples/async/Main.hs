{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators  #-}

module Main where
import           Control.Applicative
import           Control.Exception                 hiding (catch)
import           Control.Monad                     (replicateM)
import           Control.Monad.Catch
import           Control.Natural
import           System.Random


import           Control.Remote.Monad              as T
import qualified Control.Remote.Packet.Weak        as WP
--import qualified Control.Remote.Packet.Strong as SP
import qualified Control.Remote.Packet.Alternative as Alt
import qualified Control.Remote.Packet.Applicative as AP


data MyProc :: * -> * where
  Say              :: String -> MyProc ()
  Temperature      :: MyProc Int
  CommentOnWeather :: MyProc Int


instance KnownResult MyProc where
  knownResult (Say _s)           = pure ()
  knownResult (Temperature)      = Nothing
  knownResult (CommentOnWeather) = Nothing

say :: String -> RemoteMonad MyProc ()
say s = primitive (Say s)

temperature :: RemoteMonad MyProc Int
temperature = primitive Temperature

commentOnWeather :: RemoteMonad MyProc Int
commentOnWeather = primitive CommentOnWeather

--Server Side Functions
---------------------------------------------------------
eval :: MyProc a -> IO a
eval (Say s) = print s
eval Temperature = do
                       putStrLn "Temp Call"
                       getStdRandom $ randomR ((-20),120)
eval CommentOnWeather = do
                           do t <- eval Temperature
                              eval $ Say $ getPhrase t
                              return t
  where
    getPhrase :: Int -> String
    getPhrase t = if t < 32 then
                     "It's freezing out here!"
                   else if t < 70 then
                     "It's getting kind of chilly!"
                   else if t < 80 then
                     "What a beautiful day we are having!"
                   else if t < 100 then
                     "It's time to hit the pool!"
                   else
                     "It's too hot to go outside!"

---------------------------------------------------------
runWP ::  WP.WeakPacket MyProc a -> IO a
runWP (WP.Primitive x )       = eval x

---------------------------------------------------------
--runSP :: StrongPacket MyProc a -> IO a
--runSP (SP.Primitive (Say s) cs) =do
--                                print s
--                                runSP cs
--runSP (SP.Primitive Temperature) = do
--                                putStrLn "Temp Call"
--                                return 42
--runSP Done = return ()
-----------------------------------------------------------
runAP :: AP.ApplicativePacket MyProc a -> IO a
runAP (AP.Primitive x ) = eval x
runAP (AP.Zip f g h) = f <$> runAP g <*> runAP h
runAP (AP.Pure a) = do
                       putStrLn "Pure"
                       return a
---------------------------------------------------------
runAlt :: Alt.AlternativePacket MyProc a -> IO a
runAlt (Alt.Primitive x) = eval x
runAlt (Alt.Zip f g h) = f <$> runAlt g <*> runAlt h
runAlt (Alt.Pure a)    = return a
runAlt (Alt.Alt g h)       = do
           putStrLn "Alternative"
           a <- runAlt g <|> runAlt h
           putStrLn "End Alternative"
           return a
runAlt  Alt.Empty      = empty
-----------------------------------------------------------
sendWeak :: RemoteMonad MyProc a -> IO a
sendWeak = unwrapNT $ runMonad $ wrapNT (\pkt -> do putStrLn "-----"; runWP pkt)

-- sendStrong :: RemoteMonad MyProc a -> IO a
-- sendStrong = unwrapNT $ runMonad $ wrapNT (\pkt -> do putStrLn "-----"; runSP pkt)

sendApp :: RemoteMonad MyProc a -> IO a
sendApp = unwrapNT $ runMonad $ wrapNT (\pkt -> do putStrLn "-----"; runAP pkt)

sendAlt :: RemoteMonad MyProc a -> IO a
sendAlt = unwrapNT $ runMonad $ wrapNT (\pkt -> do putStrLn "-----"; runAlt pkt)
---------------------------------------------------------

main :: IO ()
main = do

        putStrLn "WeakSend\n"
        runTest $ wrapNT sendWeak

        -- putStrLn "\nStrongSend\n"
        -- runTest $ wrapNT sendStrong

        putStrLn "\nAppSend\n"
        runTest $ wrapNT sendApp

        putStrLn "\nAltSend\n"
        runTest $ wrapNT sendAlt

--Run Test Suite
runTest :: (RemoteMonad MyProc :~> IO)-> IO()
runTest (NT f) = do
               f test
               f testBind
               f testApp
               f testAlt
               f testAltException
                 `catch` (\e -> case e ::RemoteMonadException of
                                       RemoteEmptyException -> putStrLn "Empty Exception Thrown"
                         )
               f testThrowM
                 `catch` (\e -> case e :: ArithException of
                                  DivideByZero -> putStrLn "Should have sent \"hi\", then given this exception"
                                  _            -> throw e
                         )

               f testThrowM2
                  `catch` (\e -> case e :: ArithException of
                                  Underflow -> putStrLn "Should have sent temp, then given this exception"
                                  _            -> throw e
                          )

               f testCatch
               f testCatch2
               f testMapSay
               f testMapTemp

-- Original test case
test :: RemoteMonad MyProc ()
test = do
         say "Howdy doodly do"
         say "How about a muffin?"
         io $ putStrLn "This is a local IO call!"
         t <- temperature
         say (show t ++ " F")
         t2 <- commentOnWeather
         say (show t2 ++ " F")

-- Test bind
testBind :: RemoteMonad MyProc ()
testBind = say "one" >> say "two" >> temperature >>= say . ("Temperature: " ++) .show

testApp :: RemoteMonad MyProc ()
testApp = do
          r <- add <$> temperature <*> temperature <*> temperature
          say ("We added 3 temperatures: " ++ show r)
     where
            add :: Int -> Int -> Int -> Int
            add x y z= x + y + z

-- test alt
testAlt :: RemoteMonad MyProc ()
testAlt = do
    say "three" <|> say "ERROR"
    _ <- say "test1" >> say "test2" >> say "test3" >> temperature <|> temperature
    (say "test1" >> say "test2" >> empty >> say "test3") <|> say "fail"
    say "four" <|> empty
    empty       <|> say "five"
    say "six" >> empty <|> say "seven"
    r <- (temperature >>= \a -> if a > 120 then return a else say "HA" >> empty) <|> pure 32
    say "Should be 32:"
    say (show r)


--test alt withe empty on both sides
testAltException :: RemoteMonad MyProc ()
testAltException = do
    say "finished tests, now testing exception thrown"
    (do say "eight"; _ <- empty; say "shouldn't See me")  --expected exception
       <|> (do say "nine"; _ <- empty; say "AHAHA")


--test throw
testThrowM :: RemoteMonad MyProc ()
testThrowM = say "hi" >> throwM DivideByZero

-- test throw in an AP
testThrowM2 :: RemoteMonad MyProc ()
testThrowM2 = do
    r <- (+) <$> temperature <*> throwM Underflow
    say (show r)


--test catch random throwM
testCatch :: RemoteMonad MyProc ()
testCatch = (say "going to throw" >> throwM DivideByZero)
                   `catch` (\e -> case e :: ArithException of
                                    DivideByZero -> say "Divided by Zero"
                                    _            -> say "Oops!"
                           )
-- test catching Empty exception
testCatch2 :: RemoteMonad MyProc ()
testCatch2 =do
     r <- (temperature >>= \a -> if a > 120 then return a else say "HA" >> empty) <|> pure 32
        `catch` (\e -> case e :: RemoteMonadException of
                          RemoteEmptyException -> do
                                                  say "Caught Exception in Send"
                                                  temperature
                )
     say (show r)

testMapSay :: RemoteMonad MyProc ()
testMapSay = do
  sequence_ $ map say ["These","could","all", "be", "together"]

testMapTemp :: RemoteMonad MyProc ()
testMapTemp = do
  ts <- replicateM 4 temperature
  sequence_ $ map (say . show) ts

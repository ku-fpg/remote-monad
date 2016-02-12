{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE BangPatterns   #-}

import Criterion.Main
import Data.List (foldl')

import           Control.Natural (nat,run,(:~>),(#))

import qualified Control.Remote.Monad as M
import           Control.Remote.Monad.Packet.Applicative as AP
import qualified Control.Remote.Monad.Packet.Weak as WP
import qualified Control.Remote.Monad.Packet.Strong as SP
import qualified Control.Remote.Applicative as A

import System.Environment
import Data.IORef
import Data.Maybe (fromJust)

import Debug.Trace


fib :: Int -> Int
fib n
  | n <= 0    = 1
  | otherwise = fib (n-1) + fib (n-2)



defArgs :: [String]
defArgs = ["--csv","bench.csv","--output","bench.html"]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> withArgs defArgs main2
    other -> main2
  
main2 :: IO ()  
main2 = do
  stack <- newIORef []

  let bindCounts :: [Integer]
      bindCounts = take 11 [0,100..] -- take 4 $ iterate (*2) 100

  putStr "bindCounts = "
  print bindCounts

  defaultMain
    [ bgroup "left associated monadic binds"
        [ bench (show bindCount) $ whnfIO $ (\x -> run (M.runMonad (nat $ runWP stack)) $ testLeftM x) bindCount
        | bindCount <- bindCounts
        ]
    , bgroup "right associated monadic binds"
        [ bench (show bindCount) $ whnfIO $ (\x -> run (M.runMonad (nat $ runWP stack)) $ testRightM x) bindCount
        | bindCount <- bindCounts
        ]
    , bgroup "balanced monadic binds"
        [ bench (show bindCount) $ whnfIO $ (\x -> run (M.runMonad (nat $ runWP stack)) $ testBalancedM x) bindCount
        | bindCount <- bindCounts
        ]
    , bgroup "left associated >>"
        [ bench (show bindCount) $ whnfIO $ (\x -> run (M.runMonad (nat $ runWP stack)) $ testLeftM_ x) bindCount
        | bindCount <- bindCounts
        ]
    , bgroup "right associated >>"
        [ bench (show bindCount) $ whnfIO $ (\x -> run (M.runMonad (nat $ runWP stack)) $ testRightM_ x) bindCount
        | bindCount <- bindCounts
        ]
    , bgroup "left associated ma >>= ignoreArg mb"
        [ bench (show bindCount) $ whnfIO $ (\x -> run (M.runMonad (nat $ runWP stack)) $ testLeftM_ignore x) bindCount
        | bindCount <- bindCounts
        ]
    , bgroup "right associated ma >>= ignoreArg mb"
        [ bench (show bindCount) $ whnfIO $ (\x -> run (M.runMonad (nat $ runWP stack)) $ testRightM_ignore x) bindCount
        | bindCount <- bindCounts
        ]
    ]

push :: Integer -> M.RemoteMonad C P ()
push = M.command . Push

maybePush :: Maybe Integer -> M.RemoteMonad C P ()
maybePush (Just n) = M.command $ Push n
maybePush _        = pure ()

pop :: M.RemoteMonad C P (Maybe Integer)
pop = M.procedure Pop

ignoreArg :: a -> b -> a
ignoreArg x _ = x

testLeftM :: Integer -> M.RemoteMonad C P Integer
testLeftM !count
  | count == 0 = pure count
  | otherwise  = (((testLeftM (count-1) >>= ignoreArg (push (count-1))) >>= ignoreArg pop) >>= maybePush) >>= ignoreArg (fmap fromJust pop)

testRightM :: Integer -> M.RemoteMonad C P Integer
testRightM !count
  | count == 0 = pure count
  | otherwise  = push (count-1) >>= ignoreArg (pop >>= (\mi -> maybePush mi >>= ignoreArg (pop >>= testRightM . fromJust)))

testBalancedM :: Integer -> M.RemoteMonad C P Integer
testBalancedM !count
  | count <= 1 = pure count
  | otherwise  = ((testBalancedM halfCount >>= ignoreArg (push (count - halfCount))) >>= ignoreArg pop) >>= (\mi -> maybePush mi >> (pop >>= (testBalancedM . fromJust)))
  where
    halfCount = count `div` 2

testLeftM_ :: Integer -> M.RemoteMonad C P ()
testLeftM_ !count
  | count == 0 = pure ()
  | otherwise  = (((testLeftM_ (count-1) >> push (count-1)) >> pop) >> push (count-1)) >> pop >> pure ()

testRightM_ :: Integer -> M.RemoteMonad C P ()
testRightM_ !count
  | count == 0 = pure ()
  | otherwise  = push (count-1) >> (pop >> (push (count-1)) >> (pop >> testRightM_ (count-1)))


testLeftM_ignore :: Integer -> M.RemoteMonad C P ()
testLeftM_ignore !count
  | count == 0 = pure ()
  | otherwise  = (((testLeftM_ignore (count-1) >>= ignoreArg (push (count-1)) >>= ignoreArg pop)) >>= ignoreArg (push (count-1)) ) >>= ignoreArg pop >>= ignoreArg (pure ())

testRightM_ignore :: Integer -> M.RemoteMonad C P ()
testRightM_ignore !count
  | count == 0 = pure ()
  | otherwise  = push (count-1) >>= ignoreArg (pop >>= ignoreArg (push (count-1)) >>= ignoreArg (pop >>= ignoreArg (testRightM_ignore (count-1))))

----------------------------------------------------------------
-- Basic stack machine, with its interpreter

data C :: * where
  Push :: Integer -> C

data P :: * -> * where
  Pop :: P (Maybe Integer)

-- Basic evaluator

runWP :: IORef [Integer] -> WP.WeakPacket C P a -> IO a
runWP ref (WP.Command (Push a)) = do
    stack <- readIORef ref
    writeIORef ref (a : stack)
    return ()
runWP ref (WP.Procedure (Pop)) = do
    stack <- readIORef ref
    case stack of
      [] -> return Nothing
      (x:xs) -> do
          writeIORef ref xs
          return (Just x)

runSP :: IORef [Integer] -> SP.StrongPacket C P a -> IO a
runSP ref (SP.Command   c pk) = runWP ref (WP.Command c) >> runSP ref pk
runSP ref (SP.Procedure p)    = runWP ref (WP.Procedure p)
runSP ref SP.Done             = pure ()

runAppP :: IORef [Integer] -> ApplicativePacket C P a -> IO a
runAppP ref (AP.Command   g c) = runAppP ref g <*  runWP ref (WP.Command c)
runAppP ref (AP.Procedure g p) = runAppP ref g <*> runWP ref (WP.Procedure p)
runAppP ref (AP.Pure a)        = pure a

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

import Data.IORef
import Data.Maybe (fromJust)


fib :: Int -> Int
fib n
  | n <= 0    = 1
  | otherwise = fib (n-1) + fib (n-2)



main :: IO ()
main = do
  stack <- newIORef []

  let bindCount :: Integer
      bindCount = 1000

  putStr "bindCount = "
  print bindCount

  defaultMain
    [ bgroup "left associated monadic binds"
        [ bench (show bindCount) $ whnf (\x -> run (M.runMonad (nat $ runWP stack)) $ testLeftM x) bindCount
        ]
    , bgroup "right associated monadic binds"
        [ bench (show bindCount) $ whnf (\x -> run (M.runMonad (nat $ runWP stack)) $ testRightM x) bindCount
        ]

    , bgroup "balanced monadic binds"
        [ bench (show bindCount) $ whnf (\x -> run (M.runMonad (nat $ runWP stack)) $ testBalancedM x) bindCount
        ]

    , bgroup "left associated >>"
        [ bench (show bindCount) $ whnf (\x -> run (M.runMonad (nat $ runWP stack)) $ testLeftM_ x) bindCount
        ]
    , bgroup "right associated >>"
        [ bench (show bindCount) $ whnf (\x -> run (M.runMonad (nat $ runWP stack)) $ testRightM_ x) bindCount
        ]

    , bgroup "left associated ma >>= ignoreArg mb"
        [ bench (show bindCount) $ whnf (\x -> run (M.runMonad (nat $ runWP stack)) $ testLeftM_ignore x) bindCount
        ]
    , bgroup "right associated ma >>= ignoreArg mb"
        [ bench (show bindCount) $ whnf (\x -> run (M.runMonad (nat $ runWP stack)) $ testRightM_ignore x) bindCount
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
  | otherwise  = ((testBalancedM halfCount >>= ignoreArg (push halfCount)) >>= ignoreArg pop) >>= (\mi -> maybePush mi >> (pop >>= (testBalancedM . fromJust)))
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
  | otherwise  = (((testLeftM_ (count-1) >>= ignoreArg (push (count-1)) >>= ignoreArg pop)) >>= ignoreArg (push (count-1)) ) >>= ignoreArg pop >>= ignoreArg (pure ())

testRightM_ignore :: Integer -> M.RemoteMonad C P ()
testRightM_ignore !count
  | count == 0 = pure ()
  | otherwise  = push (count-1) >>= ignoreArg (pop >>= ignoreArg (push (count-1)) >>= ignoreArg (pop >>= ignoreArg (testRightM_ (count-1))))

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


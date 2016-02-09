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
      bindCount = 1000000000000000^2

  putStr "bindCount = "
  print bindCount

  defaultMain
    [ bgroup "left associated monadic binds"
        [ bench (show bindCount) $ whnf (\x -> run (M.runMonad (nat $ runWP stack)) $ testLeftM x) bindCount
        ]
    , bgroup "right associated monadic binds"
        [ bench (show bindCount) $ whnf (\x -> run (M.runMonad (nat $ runWP stack)) $ testRightM x) bindCount
        ]
    ]

push :: Integer -> M.RemoteMonad C P ()
push = M.command . Push

maybePush :: Maybe Integer -> M.RemoteMonad C P ()
maybePush (Just n) = M.command $ Push n
maybePush _        = pure ()

pop :: M.RemoteMonad C P (Maybe Integer)
pop = M.procedure Pop

testLeftM :: Integer -> M.RemoteMonad C P Integer
testLeftM !count
  | count == 0 = pure count
  | otherwise  = (((push (count-1) >>= const pop) >>= maybePush) >>= const pop) >>= (testLeftM . fromJust)

testRightM :: Integer -> M.RemoteMonad C P Integer
testRightM !count
  | count == 0 = pure count
  | otherwise  = push (count-1) >>= const (pop >>= (\mi -> maybePush mi >>= const (pop >>= testRightM . fromJust)))

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


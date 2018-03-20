{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -O0         #-}

import           Criterion.Main
import           Data.List                         (foldl')

import           Control.Natural                   ((:~>), unwrapNT, wrapNT,
                                                    ( # ))

import qualified Control.Remote.Applicative        as A
import qualified Control.Remote.Monad              as M
import           Control.Remote.Packet.Applicative as AP
import qualified Control.Remote.Packet.Strong      as SP
import qualified Control.Remote.Packet.Weak        as WP

import           Data.IORef
import           Data.Maybe                        (fromJust)
import           System.Environment

import           Control.Monad

import           Debug.Trace


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
    []    -> withArgs defArgs main2
    other -> main2

main2 :: IO ()
main2 = do
  stack <- newIORef []

  let bindCounts :: [Integer]
      bindCounts = take 6 [0,200..] -- take 4 $ iterate (*2) 100

  putStr "bindCounts = "
  print bindCounts

  defaultMain
    [ bgroup packetType
        [ bgroup "left associated monadic binds"
            [ bench (show bindCount) $ whnfIO $ (\x -> unwrapNT sender $ testLeftM x) bindCount
            | bindCount <- bindCounts
            ]
        , bgroup "right associated monadic binds"
            [ bench (show bindCount) $ whnfIO $ (\x -> unwrapNT sender $ testRightM x) bindCount
            | bindCount <- bindCounts
            ]
        , bgroup "<*> with >>="
            [ bench (show bindCount) $ whnfIO $ (\x -> unwrapNT sender $ testRightBindAndApp x) bindCount
            | bindCount <- bindCounts
            ]
        , bgroup "balanced monadic binds"
            [ bench (show bindCount) $ whnfIO $ (\x -> unwrapNT sender $ testBalancedM x) bindCount
            | bindCount <- bindCounts
            ]
        , bgroup "left associated >>"
            [ bench (show bindCount) $ whnfIO $ (\x -> unwrapNT sender $ testLeftM_ x) bindCount
            | bindCount <- bindCounts
            ]
        , bgroup "right associated >>"
            [ bench (show bindCount) $ whnfIO $ (\x -> unwrapNT sender $ testRightM_ x) bindCount
            | bindCount <- bindCounts
            ]
{-
        , bgroup "left associated ma >>= ignoreArg mb"
            [ bench (show bindCount) $ whnfIO $ (\x -> unwrapNT sender $ testLeftM_ignore x) bindCount
            | bindCount <- bindCounts
            ]
        , bgroup "right associated ma >>= ignoreArg mb"
            [ bench (show bindCount) $ whnfIO $ (\x -> unwrapNT sender $ testRightM_ignore x) bindCount
            | bindCount <- bindCounts
            ]
-}
        ]
    | (packetType,sender) <-
           [("weak",   M.runMonad (wrapNT $ runWP   stack))
           ,("strong", M.runMonad (wrapNT $ runSP   stack))
           ,("app",    M.runMonad (wrapNT $ runAppP stack))
           ]
    ]

push :: Integer -> M.RemoteMonad P ()
push = M.primitive . Push

maybePush :: Maybe Integer -> M.RemoteMonad P ()
maybePush (Just n) = M.primitive $ Push n
maybePush _        = pure ()

pop :: M.RemoteMonad P (Maybe Integer)
pop = M.primitive Pop

ignoreArg :: a -> b -> a
ignoreArg x _ = x

testLeftM :: Integer -> M.RemoteMonad P Integer
testLeftM !count
  | count == 0 = pure count
  | otherwise  = (((testLeftM (count-1) >>= ignoreArg (push (count-1))) >>= ignoreArg pop) >>= maybePush) >>= ignoreArg (fmap fromJust pop)

testRightM :: Integer -> M.RemoteMonad P Integer
testRightM !count
  | count == 0 = pure count
  | otherwise  = push (count-1) >>= ignoreArg (pop >>= (\mi -> maybePush mi >>= ignoreArg (pop >>= testRightM . fromJust)))

testBalancedM :: Integer -> M.RemoteMonad P Integer
testBalancedM !count
  | count <= 1 = pure count
  | otherwise  = ((testBalancedM halfCount >>= ignoreArg (push (count - halfCount))) >>= ignoreArg pop) >>= (\mi -> maybePush mi >> (pop >>= (testBalancedM . fromJust)))
  where
    halfCount = count `div` 2

testLeftM_ :: Integer -> M.RemoteMonad P ()
testLeftM_ !count
  | count == 0 = pure ()
  | otherwise  = (((testLeftM_ (count-1) >> push (count-1)) >> pop) >> push (count-1)) >> pop >> pure ()

testRightM_ :: Integer -> M.RemoteMonad P ()
testRightM_ !count
  | count == 0 = pure ()
  | otherwise  = push (count-1) >> (pop >> (push (count-1)) >> (pop >> testRightM_ (count-1)))


testLeftM_ignore :: Integer -> M.RemoteMonad P ()
testLeftM_ignore !count
  | count == 0 = pure ()
  | otherwise  = (((testLeftM_ignore (count-1) >>= ignoreArg (push (count-1)) >>= ignoreArg pop)) >>= ignoreArg (push (count-1)) ) >>= ignoreArg pop >>= ignoreArg (pure ())

testRightM_ignore :: Integer -> M.RemoteMonad P ()
testRightM_ignore !count
  | count == 0 = pure ()
  | otherwise  = push (count-1) >>= ignoreArg (pop >>= ignoreArg (push (count-1)) >>= ignoreArg (pop >>= ignoreArg (testRightM_ignore (count-1))))

testRightBindAndApp :: Integer -> M.RemoteMonad P Integer
testRightBindAndApp !count
  | count == 0 = pure count
  | otherwise  =
      pure pred <*> (push (count-1) >>= ignoreArg pop >>= testRightBindAndApp . fromJust)

----------------------------------------------------------------
-- Basic stack machine, with its interpreter

data P :: * -> * where
  Push :: Integer -> P ()
  Pop  :: P (Maybe Integer)

instance A.KnownResult P where
  knownResult (Push a) = Just ()
  knownResult Pop      = Nothing


-- Basic evaluator

runWP :: IORef [Integer] -> WP.WeakPacket P a -> IO a
runWP ref (WP.Primitive (Push a)) = do
    stack <- readIORef ref
    writeIORef ref (a : stack)
    return ()
runWP ref (WP.Primitive (Pop)) = do
    stack <- readIORef ref
    case stack of
      [] -> return Nothing
      (x:xs) -> do
          writeIORef ref xs
          return (Just x)

runSP :: IORef [Integer] -> SP.StrongPacket P a -> IO a
runSP ref (SP.Command   c pk) = runWP ref (WP.Primitive c) >> runSP ref pk
runSP ref (SP.Procedure p)    = runWP ref (WP.Primitive p)
runSP ref SP.Done             = pure ()

runAppP :: IORef [Integer] -> ApplicativePacket P a -> IO a
runAppP ref (AP.Primitive p) = runWP ref (WP.Primitive p)
runAppP ref (AP.Pure a)      = pure a
runAppP ref (AP.Zip f g h)   = f <$> runAppP ref g <*> runAppP ref h

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}


{-|
Module:      Main
Copyright:   (C) 2015 The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Experimental

@QuickCheck@ properties for natural transformations.
-}
module Main (main) where

import Data.Foldable (toList)
import Data.Sequence (Seq, fromList)

import qualified Control.Remote.Monad as M
import qualified Control.Remote.Monad.Packet.Weak as WP

import Test.QuickCheck 
import Test.QuickCheck.Instances ()
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck.Poly (A)
import Test.QuickCheck.Monadic
import Test.QuickCheck.Gen.Unsafe (promote)

import Data.IORef

main :: IO ()
main = defaultMain testProperties

testProperties :: TestTree
testProperties = testGroup "QuickCheck properties"
    [ 
    ]
   

----------------------------------------------------------------
-- Basic stack machine, with its interpreter

data Stack :: * -> * where
  Push :: A -> Stack ()
  Pop :: Stack (Maybe A)

data C :: * where
  CommandPush :: A -> C

data P :: * -> * where
  ProcedurePop :: P (Maybe A)

-- Basic evaluator

runWP :: IORef [String] -> IORef [A] -> WP.Weak C P a -> IO a
runWP tr ref (WP.Command (CommandPush a)) = do
    putStrLn "\n"
    stack <- readIORef ref
    writeIORef ref (a : stack)
    modifyIORef tr (("push " ++ show a) :)
    return ()
runWP tr ref (WP.Procedure (ProcedurePop)) = do
    modifyIORef tr (("pop") :)
    putStrLn "\n"
    stack <- readIORef ref
    case stack of
      [] -> return Nothing
      (x:xs) -> do
          writeIORef ref xs
          modifyIORef tr ((show x) :)
          putStrLn "\n"
          return (Just x)


----------------------------------------------------------------
-- The different ways of running remote monads.

data RemoteMonad = RemoteMonad String (forall a . IORef [String] -> IORef [A] -> M.Remote C P a -> IO a)

instance Show RemoteMonad where
  show (RemoteMonad msg _) = "Remote Monad: " ++ msg
  
instance Arbitrary RemoteMonad where
  arbitrary = elements 
    [ runWeakMonadWeakPacket
    ]
  
runWeakMonadWeakPacket :: RemoteMonad
runWeakMonadWeakPacket = RemoteMonad "WeakMonadWeakPacket" $ \ tr ref -> M.runWeakMonad (runWP tr ref)

----------------------------------------------------------------

data DeviceM = Device (IORef [String]) (IORef [A]) (forall a . M.Remote C P a -> IO a)

sendM :: DeviceM -> M.Remote C P a -> IO a
sendM (Device _ _ f) = f

newDevice :: [A] 
          -> RemoteMonad
          -> IO DeviceM
newDevice xs (RemoteMonad _ f) = do
  tr <- newIORef []
  ref <- newIORef xs
  return $ Device tr ref $ f tr ref

readDevice :: DeviceM -> IO [A]
readDevice (Device _ ref _) = readIORef ref

cmpDevices :: DeviceM -> DeviceM -> IO Bool
cmpDevices d1 d2 = (==) <$> readDevice d1 <*> readDevice d2

-- returns backwards, but is for cmp or debugging anyway
traceDevice :: DeviceM -> IO [String]
traceDevice (Device tr _ _) = readIORef tr 

----------------------------------------------------------------

newtype Remote a = Remote (M.Remote C P a)

instance Show (Remote a) where
  show _ = "<REMOTE>"

instance Arbitrary (Remote A) where
  arbitrary = sized $ \ n -> Remote <$> arbitraryRemoteMonadA n

----------------------------------------------------------------

data RemoteBind :: * -> * where
  RemoteBind :: M.Remote C P a -> (a -> M.Remote C P b) -> RemoteBind b

----------------------------------------------------------------

arbitraryRemoteMonad' :: [Gen (M.Remote C P a)] -> Int -> Gen (M.Remote C P a)
arbitraryRemoteMonad' base 0 = oneof base 
arbitraryRemoteMonad' base n = frequency 
  [ (1 , oneof base)
  , (1 , do RemoteBind m k <- arbitraryBind (arbitraryRemoteMonad' base) n
            return (m >>= k)
    )
  ]

arbitraryRemoteMonadUnit :: Int -> Gen (M.Remote C P ())
arbitraryRemoteMonadUnit = arbitraryRemoteMonad'
  [ return (return ())
  , M.command . CommandPush <$> arbitrary
  ]

arbitraryRemoteMonadMaybeA :: Int -> Gen (M.Remote C P (Maybe A))
arbitraryRemoteMonadMaybeA = arbitraryRemoteMonad'
  [ return <$> arbitrary
  , return $ M.procedure ProcedurePop
  ]

arbitraryRemoteMonadA :: Int -> Gen (M.Remote C P A)
arbitraryRemoteMonadA = arbitraryRemoteMonad'
  [ return <$> arbitrary
  , do f :: () -> A <- arbitrary
       fmap f <$> arbitraryRemoteMonadUnit 0
  , do f :: Maybe A -> A <- arbitrary
       fmap f <$> arbitraryRemoteMonadMaybeA 0
  ]


arbitraryBind :: (Int -> Gen (M.Remote C P a)) -> Int -> Gen (RemoteBind a)
arbitraryBind f n = oneof
  [ do m <- arbitraryRemoteMonadUnit (n `div` 2)
       k  <- promote (`coarbitrary` f (n `div` 2))  -- look for a better way of doing this
       return $ RemoteBind m k
  , do m <- arbitraryRemoteMonadMaybeA (n `div` 2)
       k  <- promote (`coarbitrary` f (n `div` 2))  -- look for a better way of doing this
       return $ RemoteBind m k
  , do m <- arbitraryRemoteMonadA (n `div` 2)
       k  <- promote (`coarbitrary` f (n `div` 2))  -- look for a better way of doing this
       return $ RemoteBind m k
  ]

--------------------------------------------------------------------------

-- Check that two remote monad configurations given the same trace and same result
testRunRemoteMonad :: RemoteMonad -> RemoteMonad -> Remote A -> [A] -> Property
testRunRemoteMonad runMe1 runMe2 (Remote m) xs = monadicIO $ do
    dev1 <- run $ newDevice xs runMe2
    r1 <- run $ sendM dev1 m
    tr1 <- run $ traceDevice dev1

    dev2 <- run $ newDevice xs runMe2
    r2 <- run $ sendM dev2 m
    tr2 <- run $ traceDevice dev2

    monitor (collect tr1)
    return (r1 == r2 && tr1 == tr2)
    
-- Test the remote push primitive
prop_push :: RemoteMonad -> [A] -> A -> Property
prop_push runMe xs x = monadicIO $ do
    dev <- run $ newDevice xs runMe
    () <- run $ sendM dev (M.command (CommandPush x))
    ys <- run $ readDevice  dev
    assert (ys == (x : xs))

-- Test the remote pop primitive
prop_pop :: RemoteMonad -> [A] -> Property
prop_pop runMe xs = monadicIO $ do
    dev <- run $ newDevice xs runMe
    r <- run $ sendM dev (M.procedure ProcedurePop)
    ys <- run $ readDevice  dev
    case xs of
      [] -> assert (r == Nothing && ys == [])
      (x':xs') -> assert (r == Just x' && ys == xs')


--runWeakMonadWeakPacket :: Device -> R.Remote C P a -> IO a

--WeakMonadWeakPacket :: R.Remote C P a -> IO a

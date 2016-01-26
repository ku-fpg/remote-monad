{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
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

runWP :: IORef [A] -> WP.Weak C P a -> IO a
runWP ref (WP.Command (CommandPush a)) = do
    print $ "push " ++ show a
    stack <- readIORef ref
    writeIORef ref (a : stack)
    return ()
runWP ref (WP.Procedure (ProcedurePop)) = do
    print ("pop" :: String)
    stack <- readIORef ref
    case stack of
      [] -> return Nothing
      (x:xs) -> do
          writeIORef ref xs
          print $ "popped " ++ show x
          return (Just x)


data DeviceM = Device (IORef [A]) (forall a . M.Remote C P a -> IO a)

sendM :: DeviceM -> M.Remote C P a -> IO a
sendM (Device _ f) = f

newWeakMonadWeakPacket :: [A] -> IO DeviceM
newWeakMonadWeakPacket xs = do
  ref <- newIORef xs
  return $ Device ref $ M.runWeakMonad (runWP ref)

newDevice :: [A] 
          -> (forall a . IORef [A] -> M.Remote C P a -> IO a) 
          -> IO DeviceM
newDevice xs f = do
  ref <- newIORef xs
  return $ Device ref $ f ref

readDevice :: DeviceM -> IO [A]
readDevice (Device ref _) = readIORef ref

cmpDevices :: DeviceM -> DeviceM -> IO Bool
cmpDevices d1 d2 = (==) <$> readDevice d1 <*> readDevice d2


----------------------------------------------------------------

newtype Remote a = Remote (M.Remote C P a)

instance Show (Remote a) where
  show _ = "<REMOTE>"

----------------------------------------------------------------

arbitraryRemoteMonad' :: [Gen (M.Remote C P a)] -> Int -> Gen (M.Remote C P a)
arbitraryRemoteMonad' base 0 = oneof base 
arbitraryRemoteMonad' base n = frequency 
  [ (1 , oneof base)
  , (2 , arbitraryBind (arbitraryRemoteMonad' base) n)
  ]

arbitraryBind :: (Int -> Gen (M.Remote C P a)) -> Int -> Gen (M.Remote C P a)
arbitraryBind f n = oneof
  [ do m <- arbitraryRemoteMonadA (n `div` 2)
       k  <- promote (`coarbitrary` f (n `div` 2))  -- look for a better way of doing this
       return $ m >>= k
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


--------------------------------------------------------------------------


testRunWeakMonad xs = monadicIO $ do
    run $ putStrLn $ show xs
    dev <- run $ newDevice xs (\ ref -> M.runWeakMonad (runWP ref))
    Remote m <- pick (sized $ \ n -> Remote <$> arbitraryRemoteMonadA n)
    r <- run $ sendM dev m 
    return True
    
-- Test the remote push action
prop_push :: [A] -> A -> Property
prop_push xs x = monadicIO $ do
    monitor (collect xs)
    dev <- run $ newDevice xs (\ ref -> M.runWeakMonad (runWP ref) )
    () <- run $ sendM dev (M.command (CommandPush x))
    ys <- run $ readDevice  dev
    assert (ys == (x : xs))

-- Test the remote pop action 
prop_pop :: [A] -> Property
prop_pop xs = monadicIO $ do
    monitor (collect xs)
    dev <- run $ newDevice xs (\ ref -> M.runWeakMonad (runWP ref) )
    r <- run $ sendM dev (M.procedure ProcedurePop)
    ys <- run $ readDevice  dev
    case xs of
      [] -> assert (r == Nothing && ys == [])
      (x':xs') -> assert (r == Just x' && ys == xs')



--runWeakMonadWeakPacket :: Device -> R.Remote C P a -> IO a

--WeakMonadWeakPacket :: R.Remote C P a -> IO a

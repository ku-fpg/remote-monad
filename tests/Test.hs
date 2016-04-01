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

import           Control.Natural (nat,(:~>),(#))
import           Control.Applicative 

import qualified Control.Remote.Monad as M
import           Control.Remote.Monad.Packet.Applicative as AP
import qualified Control.Remote.Monad.Packet.Weak as WP
import qualified Control.Remote.Monad.Packet.Strong as SP
import qualified Control.Remote.Applicative as A

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
testProperties = testGroup "QuickCheck remote monad properties"
    [ testProperty "push works remotely"                  $ prop_push
    , testProperty "pop works remotely"                   $ prop_pop 
    , testProperty "compare two remote monad strategies"  $ testRunRemoteMonad
    , testProperty "send (m >>= k) = send m >>= send . k" $ testRemoteMonadBindLaw
    , testProperty "send (return a) = return a"           $ testRemoteMonadReturnLaw
    , testProperty "local alt with push"                  $ testAlt
    , testProperty "local alt with arbitrary"             $ testAltArbitrary
    ]


----------------------------------------------------------------
-- Basic stack machine, with its interpreter

data C :: * where
  Push :: A -> C

data P :: * -> * where
  Pop :: P (Maybe A)

-- Basic evaluator

runWP :: IORef [String] -> IORef [A] -> WP.WeakPacket C P a -> IO a
runWP tr ref (WP.Command (Push a)) = do
    stack <- readIORef ref
    writeIORef ref (a : stack)
    modifyIORef tr (("push " ++ show a) :)
    return ()
runWP tr ref (WP.Procedure (Pop)) = do
    modifyIORef tr (("pop") :)
    stack <- readIORef ref
    case stack of
      [] -> return Nothing
      (x:xs) -> do
          writeIORef ref xs
          modifyIORef tr ((show x) :)
          return (Just x)


runSP :: IORef [String] -> IORef [A] -> SP.StrongPacket C P a -> IO a
runSP tr ref (SP.Command   c pk) = runWP tr ref (WP.Command c) >> runSP tr ref pk
runSP tr ref (SP.Procedure p)    = runWP tr ref (WP.Procedure p)
runSP tr ref SP.Done             = pure ()

runAppP :: IORef [String] -> IORef [A] -> ApplicativePacket C P a -> IO a
runAppP tr ref (AP.Command   c) = runWP tr ref (WP.Command c)
runAppP tr ref (AP.Procedure p) = runWP tr ref (WP.Procedure p)
runAppP tr ref (AP.Pure a)      = pure a
runAppP tr ref (AP.Zip f g h)   = f <$> runAppP tr ref g <*> runAppP tr ref h

----------------------------------------------------------------
-- The different ways of running remote monads.

data RemoteMonad = RemoteMonad String (forall a . IORef [String] -> IORef [A] -> M.RemoteMonad C P :~> IO)

instance Show RemoteMonad where
  show (RemoteMonad msg _) = "Remote Monad: " ++ msg
  
instance Arbitrary RemoteMonad where
  arbitrary = elements 
    [ runWeakMonadWeakPacket
    , runStrongMonadStrongPacket
    , runApplicativeMonadApplicativePacket
    ]

--- This is a complete enumeration of ways of building remote monads
  
runWeakMonadWeakPacket :: RemoteMonad
runWeakMonadWeakPacket = RemoteMonad "WeakMonadWeakPacket" 
  $ \ tr ref -> M.runMonad (nat $ runWP tr ref)

runStrongMonadStrongPacket :: RemoteMonad
runStrongMonadStrongPacket = RemoteMonad "StrongMonadStrongPacket" 
  $ \ tr ref -> M.runMonad (nat $ runSP tr ref)

runApplicativeMonadApplicativePacket :: RemoteMonad
runApplicativeMonadApplicativePacket = RemoteMonad "ApplicativeMonadApplicativePacket" 
  $ \ tr ref -> M.runMonad (nat $ runAppP tr ref)


----------------------------------------------------------------

data DeviceM = Device (IORef [String]) (IORef [A]) (M.RemoteMonad C P :~> IO)

sendM :: DeviceM -> M.RemoteMonad C P a -> IO a
sendM (Device _ _ f) m = f # m

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

newtype Remote a = Remote (M.RemoteMonad C P a)

instance Show (Remote a) where
  show _ = "<REMOTE>"

instance Arbitrary (Remote A) where
  arbitrary = sized $ \ n -> Remote <$> arbitraryRemoteMonadA n

----------------------------------------------------------------

data RemoteBind :: * -> * where
  RemoteBind :: Arbitrary a => M.RemoteMonad C P a -> (a -> M.RemoteMonad C P b) -> RemoteBind b

instance Show (RemoteBind a) where
  show _ = "<REMOTEBIND>"

----------------------------------------------------------------

arbitraryRemoteMonad' :: (CoArbitrary a, Arbitrary a) => [Gen (M.RemoteMonad C P a)] -> Int -> Gen (M.RemoteMonad C P a)
arbitraryRemoteMonad' base 0 = oneof base 
arbitraryRemoteMonad' base n = frequency 
  [ (1 , oneof base)
  , (1 , do RemoteBind m k <- arbitraryBind (arbitraryRemoteMonad' base) n
            return (m >>= k)
    )
  , (1 , do m1 <- arbitraryRemoteMonadA (n `div` 2)
            m2 <- arbitraryRemoteMonad' base (n `div` 2)
            return (m1 >> m2)
    )
  , (1 , do m1 <- arbitraryRemoteMonadA (n `div` 2)
            m2 <- arbitraryRemoteMonad' base (n `div` 2)
            f  <- arbitrary
            return (fmap f m1 <*> m2)
    )
  , (1 , do m1 <- arbitraryRemoteMonadA (n `div` 2)
            m2 <- arbitraryRemoteMonad' base (n `div` 2)
            return (m1 *> m2)
    )
  , (1 , do m1 <- arbitraryRemoteMonadA (n `div` 2)
            m2 <- arbitraryRemoteMonad' base (n `div` 2)
            return (m2 <* m1) -- reversed, because we want to return m2's result
    )
  ]

arbitraryRemoteMonadUnit :: Int -> Gen (M.RemoteMonad C P ())
arbitraryRemoteMonadUnit = arbitraryRemoteMonad'
  [ return (return ())
  , M.command . Push <$> arbitrary
  ]

arbitraryRemoteMonadMaybeA :: Int -> Gen (M.RemoteMonad C P (Maybe A))
arbitraryRemoteMonadMaybeA = arbitraryRemoteMonad'
  [ return <$> arbitrary
  , return $ M.procedure Pop
  ]

arbitraryRemoteMonadA :: Int -> Gen (M.RemoteMonad C P A)
arbitraryRemoteMonadA = arbitraryRemoteMonad'
  [ return <$> arbitrary
  ]

arbitraryBind :: (Int -> Gen (M.RemoteMonad C P a)) -> Int -> Gen (RemoteBind a)
arbitraryBind f n = oneof
  [ do m <- arbitraryRemoteMonadUnit (n `div` 2)
       k  <- promote (`coarbitrary` f (n `div` 2))  -- look for a better way of doing this
       return $ RemoteBind m k
  , do m <- arbitraryRemoteMonadMaybeA (n `div` 2)
       k  <- promote (`coarbitrary` f (n `div` 2)) 
       return $ RemoteBind m k
  , do m <- arbitraryRemoteMonadA (n `div` 2)
       k  <- promote (`coarbitrary` f (n `div` 2)) 
       return $ RemoteBind m k
  ]

--------------------------------------------------------------------------

-- Test the remote push primitive
prop_push :: RemoteMonad -> [A] -> A -> Property
prop_push runMe xs x = monadicIO $ do
    dev <- run $ newDevice xs runMe
    ()  <- run $ sendM dev (M.command (Push x))
    ys  <- run $ readDevice  dev
    assert (ys == (x : xs))

-- Test the remote pop primitive
prop_pop :: RemoteMonad -> [A] -> Property
prop_pop runMe xs = monadicIO $ do
    dev <- run $ newDevice xs runMe
    r   <- run $ sendM dev (M.procedure Pop)
    ys  <- run $ readDevice  dev
    case xs of
      [] -> assert (r == Nothing && ys == [])
      (x':xs') -> assert (r == Just x' && ys == xs')

-- Check that two remote monad configurations given the same trace and same result
testRunRemoteMonad :: RemoteMonad -> RemoteMonad -> Remote A -> [A] -> Property
testRunRemoteMonad runMe1 runMe2 (Remote m) xs = monadicIO $ do
    dev1 <- run $ newDevice xs runMe1
    r1   <- run $ sendM dev1 m
    tr1  <- run $ traceDevice dev1
    st1  <- run $ readDevice dev1

    dev2 <- run $ newDevice xs runMe2
    r2   <- run $ sendM dev2 m
    tr2  <- run $ traceDevice dev2
    st2  <- run $ readDevice dev2
    
--    monitor $ collect $ (tr1,tr2)
    assert (r1 == r2 && tr1 == tr2 && st1 == st2)
    
-- Check remote monad laws
testRemoteMonadBindLaw :: RemoteMonad -> [A] -> Property
testRemoteMonadBindLaw runMe xs = monadicIO $ do
    RemoteBind m k <- pick (sized $ arbitraryBind arbitraryRemoteMonadA)

    dev1 <- run $ newDevice xs runMe
    a    <- run $ sendM dev1 m
    r1   <- run $ sendM dev1 (k a)
    tr1  <- run $ traceDevice dev1
    st1  <- run $ readDevice dev1

    dev2 <- run $ newDevice xs runMe
    r2   <- run $ sendM dev2 (m >>= k)
    tr2  <- run $ traceDevice dev2
    st2  <- run $ readDevice dev2

--    monitor $ collect $ (runMe, tr1)
    assert (r1 == r2 && tr1 == tr2 && st1 == st2)

-- Check remote monad laws
testRemoteMonadReturnLaw :: RemoteMonad -> [A] -> A -> Property
testRemoteMonadReturnLaw runMe xs x = monadicIO $ do

    dev1 <- run $ newDevice xs runMe
    x'   <- run $ sendM dev1 (return x)
    tr1  <- run $ traceDevice dev1
    st1  <- run $ readDevice dev1

--    monitor $ collect $ (runMe, tr1)
    assert (x == x' && tr1 == [] && st1 == xs)

testAlt :: RemoteMonad -> [A] -> A -> A -> A ->Property
testAlt runMe xs x y z = monadicIO $ do
    let m1 = do M.command (Push x)
                M.command (Push y)
    let m2 = M.command (Push z)
    dev1 <- run $ newDevice xs runMe
    ()   <- run $ sendM dev1 (m1 <|> m2)
    ys   <- run $ readDevice  dev1
    let assert1= (ys == (y:x:xs))
    -----------------------------
    
    let m3 = do M.command (Push x)
                empty
                M.command (Push y)
    let m4 = M.command (Push z)
    dev2 <- run $ newDevice xs runMe
    ()   <- run $ sendM dev2 (m3 <|> m4)
    ys   <- run $ readDevice dev2
    let assert2 = (ys == (z:x:xs))
    -----------------------------

    let m5 = do M.command (Push x)
                M.command (Push y)
                M.command (Push z)
    let m6 = empty 
    dev3 <- run $ newDevice xs runMe
    ()   <- run $ sendM dev3 (m5 <|> m6)
    ys   <- run $ readDevice dev3
    let assert3 = (ys == (z:y:x:xs))

    assert (assert1 && assert2 && assert3)

testAltArbitrary :: RemoteMonad -> [A] -> Property
testAltArbitrary runMe xs = monadicIO $ do
   (Remote m1)   <- pick (arbitrary :: Gen (Remote A))
   (Remote m2)   <- pick (arbitrary :: Gen (Remote A))

   dev1 <- run $ newDevice xs runMe
   dev2 <- run $ newDevice xs runMe
   dev3 <- run $ newDevice xs runMe
   dev4 <- run $ newDevice xs runMe

   r1   <- run $ sendM dev1 (m1)
   r2   <- run $ sendM dev2 (m1 <|> m2)
   r3   <- run $ sendM dev3 (empty <|> m1)
   r4   <- run $ sendM dev4 (m1 <|> empty)

   y1   <- run $ readDevice dev1
   y2   <- run $ readDevice dev2
   y3   <- run $ readDevice dev3
   y4   <- run $ readDevice dev4

   assert (  r1 == r2 
          && r2 == r3 
          && r3 == r4 
          && y1 == y2 
          && y2 == y3
          && y3 == y4
          )

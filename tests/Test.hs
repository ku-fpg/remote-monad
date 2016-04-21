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
    [ testProperty "push works remotely"                        $ prop_pushM
    , testProperty "pop works remotely"                         $ prop_popM 
    , testProperty "compare two remote monad strategies"        $ testRunRemoteMonad
    , testProperty "send (m >>= k) = send m >>= send . k"       $ testRemoteMonadBindLaw
    , testProperty "send (return a) = return a"                 $ testRemoteMonadReturnLaw
    , testProperty "local alt with push"                        $ testAlt
    , testProperty "local alt with arbitrary"                   $ testAltArbitrary
    , testProperty "push works remotely (Applicative)"          $ prop_pushA
    , testProperty "pop works remotely  (Applicative)"          $ prop_popA 
    , testProperty "compare two remote applicative strategies"  $ testRunRemoteApplicative
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
data RemoteApplicative = RemoteApplicative String (forall a . IORef [String] -> IORef [A] -> A.RemoteApplicative C P :~> IO)


instance Show RemoteMonad where
  show (RemoteMonad msg _) = "Remote Monad: " ++ msg
  
instance Show RemoteApplicative where
  show (RemoteApplicative msg _) = "Remote Applicative: " ++ msg

instance Arbitrary RemoteMonad where
  arbitrary = elements 
    [ runWeakMonadWeakPacket
    , runStrongMonadStrongPacket
    , runApplicativeMonadApplicativePacket
    ]

instance Arbitrary RemoteApplicative where
  arbitrary = elements
    [ runApplicativeWeakPacket
    , runApplicativeStrongPacket
    , runApplicativeApplicativePacket
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

-- Ways of building remote applicative
runApplicativeWeakPacket :: RemoteApplicative
runApplicativeWeakPacket = RemoteApplicative "ApplicativeWeakPacket" 
  $ \ tr ref -> A.runApplicative (nat $ runWP tr ref)

runApplicativeStrongPacket :: RemoteApplicative
runApplicativeStrongPacket = RemoteApplicative "ApplicativeStrongPacket" 
  $ \ tr ref -> A.runApplicative (nat $ runSP tr ref)

runApplicativeApplicativePacket :: RemoteApplicative
runApplicativeApplicativePacket = RemoteApplicative "ApplicativeApplicativePacket" 
  $ \ tr ref -> A.runApplicative (nat $ runAppP tr ref)


----------------------------------------------------------------

data DeviceM = DeviceM (IORef [String]) (IORef [A]) (M.RemoteMonad C P :~> IO)
data DeviceA = DeviceA (IORef [String]) (IORef [A]) (A.RemoteApplicative C P :~> IO)

sendM :: DeviceM -> M.RemoteMonad C P a -> IO a
sendM (DeviceM _ _ f) m = f # m

sendA :: DeviceA -> A.RemoteApplicative C P a -> IO a
sendA (DeviceA _ _ f) m = f # m

newDeviceM :: [A] 
          -> RemoteMonad
          -> IO DeviceM
newDeviceM xs (RemoteMonad _ f) = do
  tr <- newIORef []
  ref <- newIORef xs
  return $ DeviceM tr ref $ f tr ref

newDeviceA :: [A] 
          -> RemoteApplicative
          -> IO DeviceA
newDeviceA xs (RemoteApplicative _ f) = do
  tr <- newIORef []
  ref <- newIORef xs
  return $ DeviceA tr ref $ f tr ref

readDeviceM :: DeviceM -> IO [A]
readDeviceM (DeviceM _ ref _) = readIORef ref

readDeviceA :: DeviceA -> IO [A]
readDeviceA (DeviceA _ ref _) = readIORef ref

cmpDevicesM :: DeviceM -> DeviceM -> IO Bool
cmpDevicesM d1 d2 = (==) <$> readDeviceM d1 <*> readDeviceM d2

-- returns backwards, but is for cmp or debugging anyway
traceDeviceM :: DeviceM -> IO [String]
traceDeviceM (DeviceM tr _ _) = readIORef tr 

traceDeviceA :: DeviceA -> IO [String]
traceDeviceA (DeviceA tr _ _) = readIORef tr 
----------------------------------------------------------------

newtype Remote a = Remote (M.RemoteMonad C P a)
newtype RemoteA a = RemoteA (A.RemoteApplicative C P a)

instance Show (Remote a) where
  show _ = "<REMOTE>"

instance Show (RemoteA a) where
  show _ = "<REMOTEA>"

instance Arbitrary (Remote A) where
  arbitrary = sized $ \ n -> Remote <$> arbitraryRemoteMonadA n

instance Arbitrary (RemoteA A) where
  arbitrary = sized $ \ n -> RemoteA <$> arbitraryRemoteApplicativeA n
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

arbitraryRemoteApplicative' :: (CoArbitrary a, Arbitrary a) => [Gen (A.RemoteApplicative C P a)] -> Int -> Gen (A.RemoteApplicative C P a)
arbitraryRemoteApplicative' base 0 = oneof base 
arbitraryRemoteApplicative' base n = frequency 
  [ (1 , oneof base)
  , (1 , do m1 <- arbitraryRemoteApplicativeA (n `div` 2)
            m2 <- arbitraryRemoteApplicative' base (n `div` 2)
            f  <- arbitrary
            return (fmap f m1 <*> m2)
    )
  , (1 , do m1 <- arbitraryRemoteApplicativeA (n `div` 2)
            m2 <- arbitraryRemoteApplicative' base (n `div` 2)
            return (m1 *> m2)
    )
  , (1 , do m1 <- arbitraryRemoteApplicativeA (n `div` 2)
            m2 <- arbitraryRemoteApplicative' base (n `div` 2)
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

arbitraryRemoteApplicativeA :: Int -> Gen (A.RemoteApplicative C P A)
arbitraryRemoteApplicativeA = arbitraryRemoteApplicative'
  [ pure <$> arbitrary
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
prop_pushM :: RemoteMonad -> [A] -> A -> Property
prop_pushM runMe xs x = monadicIO $ do
    dev <- run $ newDeviceM xs runMe
    ()  <- run $ sendM dev (M.command (Push x))
    ys  <- run $ readDeviceM  dev
    assert (ys == (x : xs))

prop_pushA :: RemoteApplicative -> [A] -> A -> Property
prop_pushA runMe xs x = monadicIO $ do
    dev <- run $ newDeviceA xs runMe
    ()  <- run $ sendA dev (A.command (Push x))
    ys  <- run $ readDeviceA  dev
    assert (ys == (x : xs))

-- Test the remote pop primitive
prop_popM :: RemoteMonad -> [A] -> Property
prop_popM runMe xs = monadicIO $ do
    dev <- run $ newDeviceM xs runMe
    r   <- run $ sendM dev (M.procedure Pop)
    ys  <- run $ readDeviceM  dev
    case xs of
      [] -> assert (r == Nothing && ys == [])
      (x':xs') -> assert (r == Just x' && ys == xs')

prop_popA :: RemoteApplicative -> [A] -> Property
prop_popA runMe xs = monadicIO $ do
    dev <- run $ newDeviceA xs runMe
    r   <- run $ sendA dev (A.procedure Pop)
    ys  <- run $ readDeviceA  dev
    case xs of
      [] -> assert (r == Nothing && ys == [])
      (x':xs') -> assert (r == Just x' && ys == xs')

-- Check that two remote monad configurations given the same trace and same result
testRunRemoteMonad :: RemoteMonad -> RemoteMonad -> Remote A -> [A] -> Property
testRunRemoteMonad runMe1 runMe2 (Remote m) xs = monadicIO $ do
    dev1 <- run $ newDeviceM xs runMe1
    r1   <- run $ sendM dev1 m
    tr1  <- run $ traceDeviceM dev1
    st1  <- run $ readDeviceM dev1

    dev2 <- run $ newDeviceM xs runMe2
    r2   <- run $ sendM dev2 m
    tr2  <- run $ traceDeviceM dev2
    st2  <- run $ readDeviceM dev2
    
--    monitor $ collect $ (tr1,tr2)
    assert (r1 == r2 && tr1 == tr2 && st1 == st2)
    
testRunRemoteApplicative :: RemoteApplicative -> RemoteApplicative -> RemoteA A -> [A] -> Property
testRunRemoteApplicative runMe1 runMe2 (RemoteA m) xs = monadicIO $ do
    dev1 <- run $ newDeviceA xs runMe1
    r1   <- run $ sendA dev1 m
    tr1  <- run $ traceDeviceA dev1
    st1  <- run $ readDeviceA dev1

    dev2 <- run $ newDeviceA xs runMe2
    r2   <- run $ sendA dev2 m
    tr2  <- run $ traceDeviceA dev2
    st2  <- run $ readDeviceA dev2
    
--    monitor $ collect $ (tr1,tr2)
    assert (r1 == r2 && tr1 == tr2 && st1 == st2)
    
-- Check remote monad laws
testRemoteMonadBindLaw :: RemoteMonad -> [A] -> Property
testRemoteMonadBindLaw runMe xs = monadicIO $ do
    RemoteBind m k <- pick (sized $ arbitraryBind arbitraryRemoteMonadA)

    dev1 <- run $ newDeviceM xs runMe
    a    <- run $ sendM dev1 m
    r1   <- run $ sendM dev1 (k a)
    tr1  <- run $ traceDeviceM dev1
    st1  <- run $ readDeviceM dev1

    dev2 <- run $ newDeviceM xs runMe
    r2   <- run $ sendM dev2 (m >>= k)
    tr2  <- run $ traceDeviceM dev2
    st2  <- run $ readDeviceM dev2

--    monitor $ collect $ (runMe, tr1)
    assert (r1 == r2 && tr1 == tr2 && st1 == st2)

-- Check remote monad laws
testRemoteMonadReturnLaw :: RemoteMonad -> [A] -> A -> Property
testRemoteMonadReturnLaw runMe xs x = monadicIO $ do

    dev1 <- run $ newDeviceM xs runMe
    x'   <- run $ sendM dev1 (return x)
    tr1  <- run $ traceDeviceM dev1
    st1  <- run $ readDeviceM dev1

--    monitor $ collect $ (runMe, tr1)
    assert (x == x' && tr1 == [] && st1 == xs)

testAlt :: RemoteMonad -> [A] -> A -> A -> A ->Property
testAlt runMe xs x y z = monadicIO $ do
    let m1 = do M.command (Push x)
                M.command (Push y)
    let m2 = M.command (Push z)
    dev1 <- run $ newDeviceM xs runMe
    ()   <- run $ sendM dev1 (m1 <|> m2)
    ys   <- run $ readDeviceM  dev1
    let assert1= (ys == (y:x:xs))
    -----------------------------
    
    let m3 = do M.command (Push x)
                empty
                M.command (Push y)
    let m4 = M.command (Push z)
    dev2 <- run $ newDeviceM xs runMe
    ()   <- run $ sendM dev2 (m3 <|> m4)
    ys   <- run $ readDeviceM dev2
    let assert2 = (ys == (z:x:xs))
    -----------------------------

    let m5 = do M.command (Push x)
                M.command (Push y)
                M.command (Push z)
    let m6 = empty 
    dev3 <- run $ newDeviceM xs runMe
    ()   <- run $ sendM dev3 (m5 <|> m6)
    ys   <- run $ readDeviceM dev3
    let assert3 = (ys == (z:y:x:xs))

    assert (assert1 && assert2 && assert3)

testAltArbitrary :: RemoteMonad -> [A] -> Property
testAltArbitrary runMe xs = monadicIO $ do
   (Remote m1)   <- pick (arbitrary :: Gen (Remote A))
   (Remote m2)   <- pick (arbitrary :: Gen (Remote A))

   dev1 <- run $ newDeviceM xs runMe
   dev2 <- run $ newDeviceM xs runMe
   dev3 <- run $ newDeviceM xs runMe
   dev4 <- run $ newDeviceM xs runMe

   r1   <- run $ sendM dev1 (m1)
   r2   <- run $ sendM dev2 (m1 <|> m2)
   r3   <- run $ sendM dev3 (empty <|> m1)
   r4   <- run $ sendM dev4 (m1 <|> empty)

   y1   <- run $ readDeviceM dev1
   y2   <- run $ readDeviceM dev2
   y3   <- run $ readDeviceM dev3
   y4   <- run $ readDeviceM dev4

   assert (  r1 == r2 
          && r2 == r3 
          && r3 == r4 
          && y1 == y2 
          && y2 == y3
          && y3 == y4
          )

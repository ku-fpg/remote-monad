{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (when)

------------------------------------------------------------------------------------------

data WeakPacket c p a where
   WP_Command   :: c -> WeakPacket c p ()
   WP_Procedure :: p a -> WeakPacket c p a

data StrongPacket c p a where
   SP_Command   :: c -> StrongPacket c p b -> StrongPacket c p b
   SP_Procedure :: p a                     -> StrongPacket c p a
   SP_Pure      :: a                       -> StrongPacket c p a


promoteWeakPacket :: Applicative m => (forall a . WeakPacket c p a -> m a) -> (StrongPacket c p a -> m a)
promoteWeakPacket f (SP_Command c r) = f (WP_Command c) *> promoteWeakPacket f r
promoteWeakPacket f (SP_Procedure p) = f (WP_Procedure p)
promoteWeakPacket f (SP_Pure a)       = pure a

------------------------------------------------------------------------------------------

data Packet c p a where
   Pure      :: a -> Packet c p a 
   Command   :: Packet c p b -> c -> Packet c p b
   Procedure :: Packet c p (a -> b) -> p a -> Packet c p b

instance Functor (Packet c p) where
  fmap f (Pure a)        = Pure (f a)
  fmap f (Command g c)   = Command (fmap f g) c
  fmap f (Procedure g p) = Procedure (fmap (f .) g) p

instance Applicative (Packet c p) where
  pure a = Pure a
  (Pure f) <*> m = fmap f m
  (Command g c)   <*> (Pure a)        = Command (fmap (\ f -> f a) g) c
  (Procedure g p) <*> (Pure a)        = Procedure (fmap (\ f a1 -> f a1 a) g) p
  m <*> (Command g2 c2)               = Command  (m           <*> g2) c2
  m <*> (Procedure g2 p2)             = Procedure (fmap (.) m <*> g2) p2

promoteStrongPacket :: forall m c p a . Monad m => (forall a . StrongPacket c p a -> m a) -> (Packet c p a -> m a)
promoteStrongPacket f p = do
    (cs,a) <- go p
    when (not (null cs)) -- you do not need to check this, if the lower level 
        $ f 
        $ foldr SP_Command (SP_Pure ()) 
        $ cs
    return a
  where
    go :: forall a . Packet c p a -> m ([c],a)
    go (Pure a)        = pure ([],a)
    go (Command g c)   = do
      (cs,r) <- go g
      return $ (cs ++ [c],r)
    go (Procedure g p) = do
      (cs,r1) <- go g
      r2 <- f $ foldr SP_Command (SP_Procedure p) cs
      return $ ([],r1 r2)

--promoteStrongPacket f (Procedure g p) = promoteStrongPacket f g <*> f (
--promoteStrongPacket f (Pure a)       = pure a
------
data WR c p a = WR ((forall a . Packet c p a -> IO a) -> IO a)

instance Functor (WR c p) where fmap f (WR g) = WR $ \ s -> fmap f (g s)
instance Applicative (WR c p) where 
  pure a = WR $ \ s -> pure a
  (WR f) <*> (WR g) = WR $ \ s -> f s <*> g s
instance Monad (WR c p) where 
  return = pure
  (WR f) >>= k = WR $ \ s -> do
                              r <- f s
                              let WR m = k r
                              m s

--command_WR c = WR $ \ s -> s (Command c)
--procedure_WR c = WR $ \ s -> s (Procedure c)

data Remote c p a where
   Bind :: Packet c p a -> (a -> Remote c p b) -> Remote c p b
   Return  :: a -> Remote c p a
  
instance Functor (Remote c p) where
  fmap f m = pure f <*> m

instance Applicative (Remote c p) where
  pure a                = Return a
  Return f <*> Return g = Return (f  g)
  Return f <*> Bind m k = Bind m (\ a -> Return f <*> k a)
  Bind m k <*> r        = Bind m (\ a -> k a <*> r)


instance Monad (Remote c p) where
  return = pure
  Return a >>= k = k a
  Bind m k >>= k2 = Bind m (\ a -> k a >>= k2)

-- After normalization, the choise of weak or strong comes down to interpretation.
runWeak :: forall m c p a . Monad m => (c -> m ()) -> (forall a . p a -> m a) -> Remote c p a -> m a
runWeak runC runP (Return a) = return a
runWeak runC runP (Bind p k) = runWeakA p >>= runWeak runC runP . k
  where
    runWeakA :: forall a . Applicative m => Packet c p a -> m a
    runWeakA (Pure a) = pure a
    runWeakA (Command g c) = runWeakA g <* runC c
    runWeakA (Procedure g p) = runWeakA g <*> runP p

runStrong :: forall m c p a . Monad m => ([c] -> m ()) -> (forall a . [c] -> p a -> m a) -> Remote c p a -> m a
runStrong runC runP m = do
   (cs,a) <- runStrong' m []
   if null cs
   then return a
   else do runC cs
           return a
  where
    runStrong' :: forall a . Remote c p a -> [c] -> m ([c],a)
    runStrong' (Return a) cs0 = return (cs0,a)
    runStrong' (Bind p k) cs0 = do
      (cs1,a) <- runStrongA p cs0
      runStrong' (k a) cs1

    runStrongA :: forall a . Monad m => Packet c p a -> [c] -> m ([c],a)
    runStrongA (Pure a)      cs0 = pure (cs0,a)
    -- A command is *queued* for later sending
    runStrongA (Command g c) cs0 = do
      (cs1,a) <- runStrongA g cs0
      return $ (cs1 ++ [c],a)
    -- A procedure is send *now*
    runStrongA (Procedure g c) cs0 = do 
      (cs1,f) <- runStrongA g cs0
      a <- runP cs1 c
      return $ ([],f a)


runStrongAF :: forall m c p a . Monad m => (forall a . Packet c p a -> m a) -> Remote c p a -> m a
runStrongAF runP (Return a) = return a
runStrongAF runP (Bind p k) = runP p >>= runStrongAF runP . k

runSuper :: forall m c p a . Monad m => (forall a . Packet c p a -> m a) -> Remote c p a -> m a
runSuper runP m = do
    (p,a) <- runSuper' m (Pure ())
    runP p -- final push
    return a
  where
    runSuper' :: forall a b . Remote c p a -> Packet c p () -> m (Packet c p (),a)
    runSuper' (Return a) p0 = return (p0,a)
    runSuper' (Bind p k) p0 = 
       case staticResult p of
          Nothing -> do a <- runP (p0 *> p)
                        runSuper' (k a) (Pure ())
          Just a -> runSuper' (k a) (p0 <* p) -- we've already extracted the a inside the p
      where
        staticResult :: forall a . Packet c p a -> Maybe a
        staticResult (Pure a) = Just a
        staticResult (Command g _) = staticResult g
        staticResult (Procedure _ _) = Nothing


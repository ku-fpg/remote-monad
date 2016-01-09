{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

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



{-
instance Monad (Remote c p) where
  Remote (Pure a) >>= k = k a
  Remote other    >>= k = Send other (\ (a,p) -> k a)
-}

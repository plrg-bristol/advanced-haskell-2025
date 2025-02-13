{-# language InstanceSigs, KindSignatures #-}
module FunctorApplicativeMonad where

import Prelude hiding (Functor(..), Applicative(..))

class Functor f where
  fmap :: (a -> b) -> f a -> f b


instance Functor (Either a) where
  fmap :: (b -> c) -> Either a b -> Either a c
  fmap _ (Left a) = Left a
  fmap f (Right x) = Right (f x)

instance Functor Maybe where
  fmap f Nothing = Nothing
  fmap f (Just x) = Just (f x) -- Nothing

-- >>> id Nothing
-- Nothing
-- >>> fmap id Nothing
-- Nothing

-- >>> fmap id (Just 3)
-- Nothing

(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

-- fmap :: (a -> b) -> f a -> f b

class Functor f => Applicative f where
  liftA2 :: (a -> (b -> c)) -> f a -> f b -> f c
  -- liftA2 f mx my = (pure f <*> mx) <*> my
  liftA2 f mx my = (fmap f mx) <*> my


  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b -- pronounced 'app' or 'tie fighting'

-- fmapp :: Applicative f => (a -> b) -> f a -> f b
-- fmapp f mx = pure f <*> mx

-- it's like function application!
-- littleApp :: (a -> b) -> a -> b
-- littleApp f x = f x

-- f :: a -> (b -> c)

-- >>> zip [1,2,3,4,5] [True, False, True, False]
-- [(1,True),(2,False),(3,True),(4,False)]

instance Applicative Maybe where
  -- liftA2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
  -- liftA2 _ Nothing _ = Nothing
  -- liftA2 _ _ Nothing = Nothing
  -- liftA2 f (Just x) (Just y) = Just (f x y)

  pure :: a -> Maybe a
  pure x = Just x

  (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  Nothing <*> mb      = Nothing
  Just f  <*> Nothing = Nothing
  Just f  <*> Just x  = Just (f x)

-- Monad

-- comp ctx
-- adding more power to functors, breaking up comp into chunks and comping
-- intro side effects

-- class Monad (m :: * -> *) where
--   -- (>>=) :: m a -> (m b -> c) -> m c
--   (>>=) :: m a -> (a -> m b) -> m b
--   fmap' :: f a -> (a ->   b) -> m b
--   join :: m (m a) -> m a

joinList :: [[a]] -> [a]
joinList = concat

bindList :: [a] -> (a -> [b]) -> [b]
bindList = flip concatMap

apM :: Monad f => f (a -> b) -> f a -> f b
apM mf mx = do
  f <- mf
  x <- mx
  return (f x)
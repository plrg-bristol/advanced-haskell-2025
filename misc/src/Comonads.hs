{-# LANGUAGE InstanceSigs #-}

module Comonads where

class Functor m => MonaDD m where
  purEE :: a -> m a
  binDD :: m a -> (a -> m b) -> m b
  -- alternative to bind
  join :: m (m a) -> m a

  -- default defs
  join mm = binDD mm (\ys -> binDD ys (\y -> purEE y))
  binDD m f = join (fmap f m)
    -- f :: (a -> m b)
    -- fmap :: (a -> b) -> m a -> m b
    -- fmap f :: -> m a -> m m b

instance MonaDD [] where
  purEE a = [a]
  -- join xss = [ y | ys <- xss, y <- ys]
  -- join xss = do
  --   ys <- xss
  --   y <- ys
  --   return y
  join xss = binDD xss (\ys -> binDD ys (\y -> pure y))

  -- lists
  -- join = concat
  -- bind = concatMap

-- comonads - dual of monads

class Functor w => Comonad w where
  -- pure :: a -> m a
  -- pureOp :: w a -> a
  extract :: w a -> a
  -- binDD :: m a -> (a -> m b) -> m b
  -- unbind :: w b -> (w b -> a) -> w a
  extend :: (w a -> b) -> w a -> w b
  -- join :: m (m a) -> m a
  -- mitosis :: w a -> w (w a)
  duplicate :: w a -> w (w a)

{-
~cofunctor interlude~
... not to be confused with contravariant functors

It is a functor that works on opposite cats

but we see that functors are their own dual

-}

data Tape a = Tape [a] a [a] deriving Show

instance Functor Tape where
  fmap f (Tape asL a asR) = Tape (fmap f asL) (f a) (fmap f asR)

-- rules for comonads
-- extend extract x     = id x
  -- extract :: w a -> a
  -- extend :: (w a -> b) -> w a -> w b
  -- x :: w a
-- extract . extend f  = f
-- extend f . extend g = extend (f . extend g)

-- Same laws, easier to understand in terms of duplicate
-- extract . duplicate      = id
-- fmap extract . duplicate = id
-- duplicate . duplicate    = fmap duplicate . duplicate

instance Comonad Tape where
  extract :: Tape a -> a
  extract (Tape asL a asR) = a
  -- TODO finish defining
  extend = undefined
  duplicate = undefined


-- TODO intro grid for game of life

-- TODO define a comonad for it

-- TODO use the comonad to play the game

-- ... TODO? Jess makes it parallel
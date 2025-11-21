{-# LANGUAGE InstanceSigs #-}

module Comonads where

import Data.List (unfoldr)

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

  duplicate = extend id
  extend f wa = fmap f (duplicate wa)

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
-- extract . duplicate        = id
-- (fmap extract) . duplicate = id
-- duplicate . duplicate    = fmap duplicate . duplicate

exampleTape :: Tape Int
exampleTape = Tape [3,2,1] 4 [5,6,7]

-- >>> moveLeft exampleTape
-- Just (Tape [2,1] 3 [4,5,6,7])

-- >>> (moveLeft >=> moveLeft >=> moveLeft) exampleTape
-- Just (Tape [] 1 [2,3,4,5,6,7])

moveLeft :: Tape a -> Maybe (Tape a)
moveLeft (Tape [] _ _)     = Nothing
moveLeft (Tape (x:xs) a asR) = Just (Tape xs x (a:asR))

-- >>> moveRight exampleTape
-- Just (Tape [4,3,2,1] 5 [6,7])

-- >>> (moveRight >=> moveRight >=> moveRight) exampleTape
-- Just (Tape [6,5,4,3,2,1] 7 [])

moveRight :: Tape a -> Maybe (Tape a)
moveRight (Tape _ _ [])     = Nothing
moveRight (Tape asL a (x:xs)) = Just (Tape (a:asL) x xs)

-- >>> allRights (Tape [1] 2 [3,4,5])
-- [Tape [2,1] 3 [4,5],Tape [3,2,1] 4 [5],Tape [4,3,2,1] 5 []]

allRights :: Tape a -> [Tape a]
allRights = unfoldr (\t -> fmap dup (moveRight t))
  -- unfoldr :: (b -> Maybe (a, b)) -> b -> [a]

dup :: a -> (a,a)
dup x = (x,x)

-- >>> allLefts (Tape [5,4,3] 6 [])
-- [Tape [4,3] 5 [6],Tape [3] 4 [5,6],Tape [] 3 [4,5,6]]

allLefts  :: Tape a -> [Tape a]
allLefts = unfoldr (\t -> fmap dup (moveLeft t))

instance Comonad Tape where
  extract :: Tape a -> a
  extract (Tape _ a _) = a
  duplicate :: Tape a -> Tape (Tape a)
  duplicate t = Tape (allLefts t) t (allRights t)

-- game of life
-- data Store coord a = Store (coord -> a)

newtype Grid a = Grid ((Int, Int) -> a)

instance Functor Grid where
  fmap :: (a -> b) -> Grid a -> Grid b
  fmap f (Grid lookup) = Grid (\c -> f (lookup c))
  -- fmap f (Grid lookup) = Grid (f . lookup)
  -- a :: (Int, Int) -> a

instance Comonad Grid where
  extract :: Grid a -> a
  extract (Grid f) = f (0,0)

  duplicate :: Grid a -> Grid (Grid a)
  duplicate (Grid f) = Grid $ \(x,y) ->
    Grid $ \(a,b) -> f (a+x,b+y)


type Rule = Grid Bool -> Bool

neighbourhood :: [(Int,Int)]
neighbourhood = [(x,y) | x <- [-1..1], y <- [-1..1], (x,y) /= (0,0)]

gameOfLifeRule :: Rule
gameOfLifeRule g@(Grid lookup) =
  let
    -- alive = lookup (0,0)
    alive = extract g
    neighboursAlive = length $ filter (==True) [ lookup c | c <- neighbourhood]
  in (not alive && neighboursAlive == 3)
    || (alive && (neighboursAlive == 2 || neighboursAlive == 3))
    -- || (alive && (neighboursAlive `elem` [2,3]))
  -- let 

step :: Rule -> Grid Bool -> Grid Bool
-- step :: (Grid Bool -> Bool) -> Grid Bool -> Grid Bool
step = extend

-- ... TODO? Jess makes it parallel

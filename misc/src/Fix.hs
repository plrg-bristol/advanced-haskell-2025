{-# language DeriveFunctor #-}

module Fix where

-- type fix

data Nat = Z | S Nat deriving Show

data Fix f = In (f (Fix f))

data NatF k = FZ | FS k

instance Functor NatF where
    fmap _ FZ = FZ
    fmap f (FS k) = FS (f k)

one :: Fix NatF
one = In (FS (In FZ))

-- recursion not abstracted
toNat :: Fix NatF -> Nat
toNat (In FZ)  = Z
toNat (In (FS k)) = S (toNat k)

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg (In x) = alg (fmap (cata alg) x)

toNat' :: Fix NatF -> Nat
toNat' = cata alg
    where
        alg :: NatF Nat -> Nat
        alg FZ     = Z
        alg (FS n) = S n

toInt :: Fix NatF -> Int
toInt = cata alg
    where
        -- just change alg and types
        alg :: NatF Int -> Int
        alg FZ     = 0
        alg (FS n) = n + 1

data ListF x k = Empty | Cons x k deriving Functor
type List a = Fix (ListF a)

foldLF :: (ListF b a -> a) -> List b -> a
foldLF alg = cata alg

length :: List a -> Int
length = foldLF alg
    where
        alg Empty = 0
        alg (Cons _ n) = n + 1

foldrF :: (a -> b -> b) -> b -> List a -> b
foldrF f k = cata alg
  where
    alg Empty = k
    alg (Cons x acc) = f x acc

length' :: List a -> Int
length' = foldrF (\_ acc -> 1 + acc) 0

-- data fix

fix :: (a -> a) -> a
fix f = f (fix f)

factF :: (Int -> Int) -> Int -> Int
factF = \k -> \n -> if n == 0 then 1 else n * k (n-1)
-- factF = \k -> \n ->  n * k (n-1) -- wont stop

fact :: Int -> Int
fact = fix factF
-- f = f (fix f)

-- for more https://yangzhixuan.github.io/pdf/fantastic-morphisms.pdf


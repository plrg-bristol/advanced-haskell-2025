{-# language InstanceSigs #-}

module Optics where

import Prelude hiding ((.))
import Control.Category

-- record syntax (aside)
data R = R Int Char deriving Show

exR :: R
exR = R 5 'x'

getInt :: R -> Int
getInt (R i _) = i

data R' = R'
    { int :: Int
    , char :: Char
    }

-- lens

data Lens s v = Lens
    { get :: s -> v
    , put :: s -> v -> s
    }

-- laws:
-- PutGet
--   get L (put L s v) = v
-- GetPut
--   put L s (get L s) = s
-- (GetGet)
-- PutPut
--  (put L s v) = put L (put L s v) v
--   put l v' (put l v s) ≡ put l v' s

-- building block lenses:
-- monolithically defined
r :: Lens R (Int, Char)
r = Lens g p
    where
        g :: R -> (Int, Char)
        g (R i c) = (i,c)
        p (R _ _) (i', c') = R i' c'

rInt :: Lens R Int
rInt = Lens g p
    where
        g (R i _) = i
        p (R _ c) i' = R i' c

rChar :: Lens R Char
rChar = Lens g p
    where
        g (R _ c) = c
        p (R i _) c' = R i c'

rTup :: Lens R (Int, Char)
rTup = Lens g p
    where
        g r = (get rInt r, get rChar r)
        p r (i', c') = put rInt (put rChar r c') i'

-- map (\r -> (get rInt r, get rChar r) (R 4 'c')
-- map <$> <*> <**> <.> <>

-- combinators:
tup :: Lens s v -> Lens s u -> Lens s (v,u)
tup lv lu = Lens g p
    where
        g r = (get lv r, get lu r)
        p r (i', c') = put lv (put lu r c') i'

rTup' :: Lens R (Int, Char)
rTup' = tup rInt rChar

-- double zoom
(>>>>) :: Lens s v -> Lens v u -> Lens s u
(>>>>) l1 l2 = Lens g p
    where
        g s = get l2 (get l1 s)
        -- p :: s -> u -> s
        p s u = put l1 s (put l2 (get l1 s) u)

data World = World
    { time :: Int
    , player :: Player}

data Player = Player
    { name :: String
    , hairColour :: Bool}

w2p :: Lens World Player
w2p = Lens g p
    where
        g s = player s
        -- p w pl = World (time w) pl
        -- record syntax update
        p w pl = w { player = pl}

p2h :: Lens Player Bool
p2h = Lens g p
    where
        g = hairColour
        p pl b = Player (name pl) b

worldToHairColour :: Lens World Bool
worldToHairColour = w2p >>>> p2h

-- type class combinators (profunctors, category)

instance Category Lens where
    id :: Lens a a
    id = Lens Prelude.id (\s v -> v)

    (.) :: Lens v u -> Lens s v -> Lens s u
    (.) = flip (>>>>)

-- functor is covariant
instance Functor (Lens s) where
    fmap :: (v -> v') -> Lens s v -> Lens s v'
    fmap f (Lens g1 p1) = Lens g (\s v' -> p1 s (undefined v'))
    -- p1 expects a v not a v'
    -- need v' -> v
    -- but all we have is (v -> v')

        where
            g = f . g1
            -- p s v' = _ -- ut oh!
            -- this is not a functor -- its a profunctor

-- contravariant functor
-- Data.Functor.Contravariant (base)
class Contravariant f where
    contramap :: (a -> b) -> f b -> f a

instance Contravariant (Lens s) where
    contramap :: (a -> b) -> Lens s b -> Lens s a
    contramap f (Lens g1 p1)
        = Lens
            (undefined . g1)
                -- goal : s -> a
                -- g1 : s -> b
                -- fwd arrow missing!
            (\s v' -> p1 s (f v'))


-- profunctors
class Profunctor p where
    rmap :: (a -> b) -> p c a -> p c b -- covariant
    lmap :: ((->) d c) -> p c a -> p d a -- contravariant

instance Profunctor (->) where
    -- rmap :: (a -> b) -> ((->) c a) -> ((->) c b)
    rmap :: (a -> b) -> (c -> a) -> (c -> b)
    rmap aToB cToA = aToB . cToA -- cToB
                   -- b <- a <- c
    lmap :: (d -> c) -> (c -> a) -> (d -> a)
    lmap dToC cToA= \d -> cToA (dToC d)

instance Profunctor Lens where
    lmap :: (s -> s') -> Lens s' v -> Lens s v
    lmap f (Lens g1 p1) = Lens (g1 . f) (\s v -> undefined (p1 (f s) v))
    -- all we did was combine the classes, each of which had functions we could
    -- not define!
    -- lmap has a problem with p
    -- rmap has a problem with g

-- either lens needs to change
-- or profunctor needs to
    -- maybe instead of a function we take p
-- for the answer see https://www.pure.ed.ac.uk/ws/portalfiles/portal/286181739/Composing_Bidirectional_XIA_DOA25012019_VOR_CC_BY.pdf
-- or the journal extension that sam is involved with, see pdf monadic-profunctors-journal.pdf

-- prisms

data FunEither a b c = West a | Center b | East c

-- for comemnted out p
-- get L (put L s v) = v
-- get L (put L (Center b) (Just a)) = (Just a)
-- get L (Center b) = (Just a)
-- Nothing = (Just a)
-- !!!!!
-- get L (put L s v) = v YES
-- put L s (get L s) = s YES
-- put l v' (put l v s) ≡ put l v' s
-- put l v' (put l (Just a) (Center b)) ≡ put l v' s
-- put l Nothing (West a) ≡ put l Nothing (Center b)
-- West a ≡ Center b
-- !!!!!!
westL :: Lens (FunEither a b c) (Maybe a)
westL = Lens g p
    where
        g (West a) = Just a
        g _ = Nothing
        p :: (FunEither a b c) -> (Maybe a) -> (FunEither a b c)
        -- p (West a) (Just a') = West a'
        p _ (Just a') = West a'
        p fe _ = fe

-- get funEitherL (put funEitherL s v) = v ? YES
-- put L s (get L s) = s ? YES
-- put l v' (put l v s) ≡ put l v' s ? YES
-- good cos its structure preserving
-- do most law breaks come from product type?

funEitherL :: Lens (FunEither a a a) a
funEitherL = Lens g p
    where
        g (West a) = a
        g (Center a) = a
        g (East a) = a
        p (West _) a = West a
        p (Center _) a = Center a
        p (East _) a = East a

-- -> Prism

data Prism s v = Prism
    { preview :: s -> Maybe v -- "get"
    -- ,  reviewLuna :: s -> v -> Maybe s
    -- Q what if prism's sums are products?
    -- A we can compose a prism with a lens
    ,  review :: v -> s -- ~ smart constructor
    -- BX where one dir (preview) can fail, the other always succeeds
    -- we can always make a sum type, we cant always map out
    }

westP :: Prism (FunEither a b c) a
westP = Prism p r
    where
        p :: (FunEither a b c) -> Maybe a
        p (West a) = Just a
        p _ = Nothing
        r :: a -> (FunEither a b c)
        r a = West a

-- laws?
--   preview (review p v) == Just v
--   preview p s == Just v => review p v == s


-- postCompWL :: prism s a -> Lens a b -> prism s b

-- many more!
-- https://hackage.haskell.org/package/optics-0.4.2.1/docs/Optics.html
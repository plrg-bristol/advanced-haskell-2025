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
        p (R i c) (i', c') = R i' c'

rInt :: Lens R Int
rInt = Lens g p
    where
        g (R i c) = i
        p (R i c) i' = R i' c

rChar :: Lens R Char
rChar = Lens g p
    where
        g (R i c) = c
        p (R i c) c' = R i c'

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

instance Functor (Lens s) where
    fmap :: (v -> v') -> Lens s v -> Lens s v'
    fmap f (Lens g1 p1) = Lens g p
        where
            g = f . g1
            p s v' = undefined -- ut oh!
            -- this is not a functor -- its a profunctor

-- prisms
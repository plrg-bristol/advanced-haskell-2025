
module Optics where

-- record syntax (aside)
data R = R Int Char deriving Show

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


-- type class combinators (profunctors, category)
-- prisms
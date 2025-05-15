{-# language InstanceSigs #-}
{-# language ScopedTypeVariables #-}


module AppParsers where

-- based off https://hackage.haskell.org/package/yoda

newtype Parser a = Parser { parse :: String -> [(a, String)]}

-- parser :: (String -> [(a, String)])

item :: Parser Char
item = Parser { parse = p }
    where
        p [] = []
        p (c:cs) = [(c,cs)]

look :: Parser String
look = Parser { parse = p }
    where
        p [] = []
        p cs = [(cs,cs)]

instance Functor Parser where
    fmap :: forall a b. (a -> b) -> Parser a -> Parser b
    fmap f pa = Parser { parse = p }
        where
            p :: String -> [(b, String)]
            p s = map (\(a,st) -> (f a, st)) (parse pa s)
            -- parse pa s :: [(a, String)]

-- >>> number "1"
-- [(1,"")]

-- item, fmap

number :: Parser Int
-- number = fmap (read . pure ) item
number = (read . pure ) <$> item

-- HW: make this work for more than single digits

instance Applicative Parser where
    pure :: a -> Parser a
    pure a = Parser p
        where
            p s = [(a,s)]

    (<*>) :: forall a b. Parser (a -> b) -> Parser a -> Parser b
    (<*>) (Parser pf) (Parser pa) = Parser p
        where
            p :: String -> [(b,String)]
            p s = [ (f a, ts'') | (f, ts') <- (pf s)
                                , (a, ts'') <- (pa ts')]

            -- (pf s) :: [(a->b, s)]
            -- (pa s) :: [(a, s)]



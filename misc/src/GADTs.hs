{-# language GADTs, DataKinds, NoStarIsType #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- {-# language GHC2024 #-}
module GADTs where

import Prelude hiding (Either(..))
import Data.Kind (Type)

-- Highly, highly recommend Richard Eisenberg's series of videos about GADTs and length-index vectors: https://www.youtube.com/watch?v=PHS3Q-tRjFQ

-- ADTs

-- Sum type
data Either a b = Left a | Right b

-- Product type
data Pair a b = Pair a b

-- GADT Syntax

-- Sum types
data Either' a b where
  Left'  :: a -> Either' a b
  Right' :: b -> Either' a b

-- Product types
data Pair' a b where
  P :: a -> b -> Pair' a b

-- Full GADTs

data Foo a where
  Bar :: Int  -> Foo Int
  Baz :: Char -> Foo Char

data Foo' a = Bar' Int | Baz' Char

unFoo :: Foo a -> Either Int Char
unFoo (Bar x) = Left x
unFoo (Baz x) = Right x

-- Nat
data Nat = Nil | Succ Nat

two :: Nat
two = Succ (Succ Nil)

three :: Nat
three = Succ two

data SNat (a :: Nat) where
  SNil :: SNat Nil
  SSucc :: SNat a -> SNat (Succ a)

-- >>> :t Nil
-- Nil :: Nat
-- >>> :k Nil
-- Nil :: Nat
-- >>> :i SNat
-- type role SNat nominal
-- type SNat :: Nat -> Type
-- data SNat a where
--   SNil :: SNat 'Nil
--   	-- Defined at /home/jess/Teaching/advanced-haskell-2025/misc/src/GADTs.hs:46:1

zeroSing :: SNat Nil
zeroSing = SNil

twoSing :: SNat (Succ (Succ Nil))
twoSing = SSucc (SSucc SNil)

-- impossible :: Nil
-- impossible = undefined

data Vec (a :: Type) (n :: Nat) where
  VNil :: Vec a Nil
  VCons :: a -> Vec a n -> Vec a (Succ n)
  -- VCons :: a -> List a   -> List a 

oneTwoThree :: Vec Integer (Succ (Succ (Succ Nil)))
oneTwoThree = VCons 1 (VCons 2 (VCons 3 VNil))

vhead :: Vec a (Succ n) -> a
vhead (VCons x _) = x
-- vhead VNil = undefined


head' :: [a] -> a
head' (x : xs) = x

class LT (m :: Nat) (n :: Nat) where
  leqProof :: LTProof m n

instance LT Nil (Succ n) where leqProof = LTNil
instance LT m n => LT (Succ m) (Succ n)
  where
    leqProof = LTSucc leqProof

data LTProof (m :: Nat) (n :: Nat) where
  LTNil  :: LTProof Nil (Succ n)
  LTSucc :: LTProof m n -> LTProof (Succ m) (Succ n)

index :: LTProof m n -> Vec a n -> SNat m -> a
-- index VNil SNil = _
index _ (VCons x xs) SNil = x
index (LTSucc ltMN) (VCons x xs) (SSucc m')
  = index ltMN xs m'

baz :: Integer
baz = index leqProof oneTwoThree (SSucc SNil)

-- >>> baz
-- 2

index' :: forall m n a. LT m n => Vec a n -> SNat m -> a
-- index VNil SNil = _
index' = index leqProof
-- index' (VCons x xs) SNil = x
-- index' (VCons x xs) (SSucc m')
--   = case leqProof @m @n of
--       LTSucc pm' -> index' xs m'

bat :: Integer
bat = index' oneTwoThree (SSucc SNil)

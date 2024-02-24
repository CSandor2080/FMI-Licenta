module ListClass where

import Prelude (Show(..), (<>), undefined)
import qualified Data.List as List

import Prelude
import BoolClass
import MaybeClass
import NatClass
import PairClass 

class ListClass l where
  nil :: l a
  cons :: a -> l a -> l a
  foldr :: (a -> b -> b) -> b -> l a -> b

instance ListClass [] where
  nil = []
  cons = (:)
  foldr = List.foldr

-- | Append two lists
(++) :: (ListClass l) => l a -> l a -> l a
(++) l1 l2 = foldr cons l2 l1
 
length :: ListClass l => l a -> CNat
length = foldr (\_ n -> CSucc n) CZero
 
isNull :: ListClass l => l a -> CBool
isNull = foldr (\_ _ -> CFalse) CTrue
 
map :: ListClass l => (a -> b) -> l a -> l b
map f = foldr (\x xs -> cons (f x) xs) nil

filter :: ListClass l => (a -> CBool) -> l a -> l a
filter p = foldr (\x xs -> ifThenElse (p x) (cons x xs) xs) nil
 
foldl :: (ListClass l) => (b -> a -> b) -> b -> l a -> b
foldl f z l = foldr (\x g -> (\a -> g (f a x))) id l z
 
uncons :: ListClass l => l a -> CMaybe (CPair a (l a))
uncons = foldr (\x _ -> CJust (CPair x nil)) CNothing
 
head :: ListClass l => l a -> CMaybe a
head = foldr (\x _ -> CJust x) CNothing
 
tail :: ListClass l => l a -> CMaybe (l a)
tail = foldr (\_ xs -> CJust xs) CNothing
 
reverse :: ListClass l => l a -> l a
reverse = foldl (flip cons) nil
 
sum :: ListClass l => l CNat -> CNat
sum = foldr add CZero
 
product :: ListClass l => l CNat -> CNat
product = foldr mul COne
 
maximum :: ListClass l => l CNat -> CNat
maximum = foldr max CZero 

natToList :: ListClass l => CNat -> l CNat
natToList n = foldr cons nil (map CSucc (iterate CPred n))
 

newtype CList a = CList { getCList :: forall b . (a -> b -> b) -> b -> b }

instance ListClass CList where
  foldr f i l = getCList l f i
  nil = CList (\_ i -> i)
  cons x xs = CList (\f i -> f x (getCList xs f i))
 
fromListClass :: (ListClass l1, ListClass l2) => l1 a -> l2 a
fromListClass = foldr cons nil
 
instance Show a => Show (CList a) where
  show cl = "{" <> show (fromListClass cl :: [a]) <> "}"
 
factorial :: CNat -> CNat
factorial n = foldr mul COne (natToList n)
 

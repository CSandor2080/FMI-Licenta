module NatClass where

import Prelude (Show(..), (<>), Num(fromInteger), undefined)
import qualified GHC.Natural as Natural

import MyPrelude
import BoolClass (CBool(..))
import MaybeClass (CMaybe(..))

class NatClass n where
  zero :: n
  succ :: n -> n
  iter :: (a -> a) -> a -> n -> a

instance NatClass Natural.Natural where
  zero = 0
  succ n = n `Natural.plusNatural` 1
  iter f i 0 = i
  iter f i n = f (iter f i (n `Natural.minusNatural` 1))

one :: NatClass n => n
one = succ zero

isZero :: NatClass n => n -> CBool
isZero n = case n `eq` zero of
  CTrue -> CTrue
  CFalse -> CFalse

add :: NatClass n => n -> n -> n
add n1 n2 = iter succ n1 n2

mul :: NatClass n => n -> n -> n
mul n1 n2 = iter (add n1) zero n2

exp :: NatClass n => n -> n -> n
exp n1 n2 = iter (mul n1) one n2

pred :: NatClass n => n -> CMaybe n
pred n = case isZero n of
  CTrue -> CNothing
  CFalse -> CJust (iter pred zero n)

sub :: NatClass n => n -> n -> CMaybe n
sub n1 n2 = case lt n2 n1 of
  CTrue -> CJust (iter pred n1 (sub n1 n2))
  CFalse -> CNothing

lt :: NatClass n => n -> n -> CBool 
lt n1 n2 = case sub n1 n2 of
  CNothing -> CFalse
  CJust _ -> CTrue

gt :: NatClass n => n -> n -> CBool 
gt n1 n2 = lt n2 n1

gte :: NatClass n => n -> n -> CBool 
gte n1 n2 = case lt n1 n2 of
  CTrue -> CFalse
  CFalse -> CTrue

lte :: NatClass n => n -> n -> CBool 
lte n1 n2 = gte n2 n1

eq :: NatClass n => n -> n -> CBool 
eq n1 n2 = case sub n1 n2 of
  CNothing -> case sub n2 n1 of
    CNothing -> CTrue
    CJust _ -> CFalse
  CJust _ -> CFalse

max :: NatClass n => n -> n -> n
max n1 n2 = case lt n1 n2 of
  CTrue -> n2
  CFalse -> n1

newtype CNat = CNat { getCNat :: forall a . (a -> a) -> a -> a }

instance NatClass CNat where
  iter f i n = getCNat n f i
  zero = CNat (\_ i -> i)
  succ n = CNat (\f i -> f (getCNat n f i))

fromNatClass :: (NatClass n, NatClass m) => n -> m
fromNatClass n = iter succ zero n

instance Show CNat where
  show cn = "C" <> show (fromNatClass cn :: Natural.Natural)

instance Num CNat where
  fromInteger n = fromNatClass (fromInteger n :: Natural.Natural)

module EitherClass where

import Prelude (Show(..), (<>), undefined) -- for show instances
import qualified Data.Either as Either (Either (..), either)

 
import BoolClass
 
class EitherClass e where
  left :: a -> e a b
  right :: b -> e a b
  either :: (a -> c) -> (b -> c) -> e a b -> c

instance EitherClass Either.Either where 
  left = Either.Left
  right = Either.Right
  either = Either.either 

fromLeft :: (EitherClass e) => a -> e a b -> a
fromLeft def e = either id (const def) e
 
fromRight :: (EitherClass e) => b -> e a b -> b
fromRight def e = either (const def) id e
 
isLeft :: (EitherClass e, BoolClass b) => e l r -> b
isLeft e = either (const true) (const false) e
 
isRight :: (EitherClass e, BoolClass b) => e l r -> b
isRight e = either (const false) (const true) e
 
eitherLeftMap :: EitherClass e => (a -> a') -> e a b -> e a' b
eitherLeftMap f e = either (left . f) right e 

eitherRightMap :: EitherClass e => (b -> b') -> e a b -> e a b'
eitherRightMap f e = either left (right . f) e
 
newtype CEither a b = CEither { getCEither :: forall c . (a -> c) -> (b -> c) -> c }

instance EitherClass CEither where
  left x = CEither (\l _ -> l x)
  right y = CEither (\_ r -> r y)
  either lHandle rHandle e = getCEither e lHandle rHandle
 
fromEitherClass :: (EitherClass m, EitherClass n) => m a b -> n a b
fromEitherClass e = either left right e
 
instance (Show a, Show b) => Show (CEither a b) where
  show ce = "C" <> show (fromEitherClass ce :: Either.Either a b)
 

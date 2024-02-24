module BoolClass where

import Prelude (undefined)
import qualified GHC.Show as Show
import qualified Data.Bool as Bool
import GHC.Show (Show(..))
import Data.Monoid ((<>))

-- | Identity function.
id :: a -> a
id a = a
 
const :: a -> b -> a
const x _ = x
 
flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x
 
(.) :: (b -> c) -> (a -> b) -> a -> c
(g . f) x = g (f x)
 
class BoolClass b where
  false :: b
  true :: b
  bool :: a -> a -> b -> a

instance BoolClass Bool.Bool where
  true = Bool.True
  false = Bool.False
  bool = Bool.bool
 
ite :: BoolClass b => b -> a -> a -> a
ite b x y = bool x y b
 
(&&) :: BoolClass b => b -> b -> b
(&&) x y = bool y false x

 
(||) :: BoolClass b => b -> b -> b
(||) x y = bool y true x
 
not :: BoolClass b => b -> b
not x = bool true false x
 

newtype CBool = CBool { getCBool :: forall a. a -> a -> a }

instance BoolClass CBool where
  true = CBool const
  false = CBool (\_ y -> y)
  bool f t (CBool b) = b f t
 
fromBoolClass :: (BoolClass a, BoolClass b) => a -> b
fromBoolClass = bool false true
 
instance Show CBool where
  show cb = "C" <> show (fromBoolClass cb :: Bool.Bool)

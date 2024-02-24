module MaybeClass where

import Prelude (Show(..), (<>), undefined)
import qualified Data.Maybe as Maybe

import BoolClass
 
class MaybeClass m where
  nothing :: m a
  just :: a -> m a

instance MaybeClass Maybe.Maybe where
  nothing = Maybe.Nothing
  just = Maybe.Just

data CMaybe a = CNothing | CJust a

instance MaybeClass CMaybe where
  nothing = CNothing
  just = CJust
 
instance Show a => Show (CMaybe a) where
  show CNothing = "CNothing"
  show (CJust a) = "CJust " <> show a

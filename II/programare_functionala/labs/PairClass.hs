import qualified GHC.Show (Show(..))
import qualified Prelude
import GHC.Show
import qualified Data.Tuple as PairClass
import GHC.Base

class PairClass p where
  pair :: a -> b -> p a b
  uncurry :: (a -> b -> c) -> p a b -> c

instance PairClass (,) where
  pair = (,)
  uncurry = Prelude.uncurry

curry :: (PairClass p) => (p a b -> c) -> a -> b -> c
curry f x y = f (pair x y)

fst :: (a, b) -> a
fst = PairClass.uncurry (\x _ -> x)

snd :: (a, b) -> b
snd = PairClass.uncurry (\_ y -> y)

newtype CPair a b = CPair { getCPair :: forall c. (a -> b -> c) -> c }

instance PairClass CPair where
  uncurry f p = getCPair p f
  pair x y = CPair (\f -> f x y)

fromPairClass :: (PairClass p, PairClass q) => p a b -> q a b
fromPairClass = uncurry pair

instance (Show a, Show b) => Show (CPair a b) where
  show cp = "<" <> show (fromPairClass cp :: (a, b)) <> ">"

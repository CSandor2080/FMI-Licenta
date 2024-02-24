{-
class Functor f where
fmap :: ( a -> b ) -> f a -> f b
-}

{- Scrieti instante ale clasei Functor pentru tipurile de date descrise mai jos.
newtype Identity a = Identity a
data Pair a = Pair a a
data Constant a b = Constant b
data Two a b = Two a b
data Three a b c = Three a b c
data Three' a b = Three' a b b
data Four a b c d = Four a b c d
data Four'' a b = Four'' a a a b
data Quant a b = Finance | Desk a | Bloor b
S-ar putea să fie nevoie să adăugat,i unele constrângeri la definirea instantelor
data LiftItOut f a = LiftItOut (f a)
data Parappa f g a = DaWrappa (f a) (g a)
data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
data Notorious g o a t = Notorious (g o) (g a) (g t)
data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
data TalkToMe a = Halt | Print String a | Read (String -> a) -}

newtype Identity a = Identity a
    deriving Show 
instance Functor Identity where 
    fmap :: (a->b) -> Identity a -> Identity b
    fmap f(Identity a) = Identity (f a)

data Pair = Pair a a 
instance Functor Pair where 
    fmap f(Pair a1 a2) = Pair (f a1) (f a2) 

data Two a b = Two a b
instance Functor (Two a b) where
    fmap :: (a2 -> b) -> Two a1 a2 -> Two a1 b
    fmap f(Two a b)=Two a (f b)

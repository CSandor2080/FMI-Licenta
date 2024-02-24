-- Se dau următoarele tipuri de date ce reprezinta puncte cu numar variabil de coordonate intregi:
data Punct = Pt [Int]

-- Arbori cu informatia în frunze 
data Arb = Vid | F Int | N Arb Arb
    deriving Show

-- a) Să se scrie o instantă a clasei Show pentru tipul de date Punct, astfel încât lista
-- coordonatelor sa fie afisată sub forma de tuplu
instance Show Punct where 
    show :: Punct -> String 
    show (Pt p) = "(" ++ fct p ++ ")"
        where 
            fct [] = ""
            fct [x] = show x 
            fct (x:xs) = show x ++ "," ++ fct xs 

-- clasă de tipuri ToFromArb
class ToFromArb a where
    toArb :: a -> Arb
    fromArb :: Arb -> a

-- b) Să se scrie o instanţă a clasei ToFromArb pentru tipul de date Punct astfel incat
-- lista coordonatelor punctului sa coincidă cu frontiera arborelui.

instance ToFromArb Punct where 
    toArb :: Punct -> Arb
    toArb (Pt []) = Vid
    toArb (Pt [x]) = F x 
    toArb (Pt (x:xs)) = N (F x) (toArb (Pt xs))

    fromArb :: Arb -> Punct
    fromArb (Vid) = Pt []
    fromArb (F x) = Pt [x] 
    fromArb (N arb1 arb2) = Pt (a1 ++ a2) 
        where 
            Pt a1 = fromArb arb1
            Pt a2 = fromArb arb2 


--Se dă următorul tip de date reprezentând figuri geometrice.
data Geo a = Square a | Rectangle a a | Circle a
    deriving Show

--Si clasa GeoOps în care se definesc operatiile perimeter si area.
class GeoOps g where
    perimeter :: (Floating a) => g a -> a
    area :: (Floating a) => g a -> a

-- a) Să se instantieze clasa GeoOps pentru tipul de date Geo. Pentru valoarea pi există
-- functia cu acelasi nume (pi).

instance GeoOps Geo where 
    perimeter :: (Floating a) => Geo a -> a
    perimeter (Square a) = a * a 
    perimeter (Rectangle a b) = a * b
    perimeter (Circle a) = pi * a * 2
    area :: (Floating a) => Geo a -> a
    area (Square a) = a * 4 
    area (Rectangle a b) = (a + b) * 2
    area (Circle a) = a * a * pi

-- b) Să se instantieze clasa Eq pentru tipul de date Geo, astfel încât două figuri
-- geometrice să fie egale dacă au perimetrul egal.

{-instance Eq Geo where 
    eq :: Geo -> Geo -> Bool
    eq (Square a) (Square b) 
        |perimeter (Square a) == perimeter (Square b) = True
        |otherwise = False-} -- gresit (??)

instance (Eq a, Floating a) => Eq (Geo a) where
    (==) :: Geo a -> Geo a -> Bool
    a == b = perimeter a == perimeter b 
 
import System.Win32 (xBUTTON1, SECURITY_ATTRIBUTES (bInheritHandle), lANGIDFROMLCID, UnicodeSubsetBitfield, COORD (yPos), sCS_32BIT_BINARY)
import System.Win32.SimpleMAPI (FileTag(ftEncoding))
import Control.Applicative.Lift (Lift)
import GHC.Types (Multiplicity(One)) 

import System.Win32 (COORD(yPos), mAXIMUM_ALLOWED)
import Foreign (FinalizerEnvPtr)

{-data Punct = Pt [Int]

data Arb = Vid | F Int | N Arb Arb
    deriving Show

class ToFromArb a where
    toArb :: a -> Arb
    fromArb :: Arb -> a

-- a) Să se scrie o instantă a clasei Show pentru tipul de date Punct, astfel încât lista
-- coordonatelor sa fie afisată sub forma de tuplu.

instance Show Punct where 
    show :: Punct -> String
    show (Pt p) = "(" ++ f p ++ ")"
        where 
            f [] = ""
            f [x] = show x
            f (x:xs) = show x ++ "," ++ f xs


-- b) Să se scrie o instanţă a clasei ToFromArb pentru tipul de date Punct astfel incat
-- lista coordonatelor punctului sa coincidă cu frontiera arborelui

instance ToFromArb Punct where 
    toArb :: Punct -> Arb 
    toArb (Pt [])= Vid 
    toArb (Pt [x])= F x
    toArb (Pt (x:xs))= N (F x)(toArb (Pt xs))

    fromArb :: Arb -> Punct 
    fromArb (Vid) = Pt []
    fromArb (F x) = Pt [x]
    fromArb (N arb1 arb2) = Pt(a1 ++ a2)
        where 
            Pt a1 = fromArb arb1
            Pt a2 = fromArb arb2   


data Geo a = Square a | Rectangle a a | Circle a
    deriving Show

class GeoOps g where
    perimeter :: (Floating a) => g a -> a
    area :: (Floating a) => g a -> a

-- a) Să se instantieze clasa GeoOps pentru tipul de date Geo. Pentru valoarea pi există
-- functia cu acelasi nume (pi).

instance GeoOps Geo where 
    perimeter :: (Floating a) => Geo a -> a
    perimeter (Square a) = a * a
    perimeter (Rectangle a b) = a * b
    perimeter (Circle a) = a * pi * 2


    area :: (Floating a) => Geo a -> a
    area (Square a) = a * 4
    area (Rectangle a b) = (a + b) * 2
    area (Circle a) = a * pi * a 

-- b) Să se instantieze clasa Eq pentru tipul de date Geo, astfel încât două figuri
-- geometrice să fie egale dacă au perimetrul egal.

instance (Eq a, Floating a) => Eq(Geo a) where 
    (==) :: Geo a -> Geo a -> Bool
    (==) a b = perimeter a == perimeter b 


type Nume = String
data Prop
    = Var Nume
    | F
    | T
    | Not Prop
    | Prop :|: Prop
    | Prop :&: Prop
    | Prop :->: Prop
    | Prop :<->: Prop
    deriving Eq
infixr 2 :|:
infixr 3 :&:

 Ex 2. Faceti tipul Prop instantă a clasei de tipuri Show, înlocuind conectivele Not, :|: s, i :&:
cu ~, | si & si folosind direct numele variabilelor în loc de constructia Var nume.
instance Show Prop where
show = undefined
test_ShowProp :: Bool
test_ShowProp =
show (Not (Var "P") :&: Var "Q") == "((~P)&Q)" 


instance Show Prop where 
    show :: Prop -> String 
    show (F) = "False"
    show (T) = "True"
    show (Var x) = x
    show (Not x) = "~" ++ show x
    show (p :|: q) = show p ++ "|" ++ show q 



data Fruct                          -- Fruct este constructor de date iar Mar si Portocala constructori de tip
    = Mar String Bool               --este un tip de date algebric ptc foloseste si suma si produs
    | Portocala String Int 


ionatanFaraVierme = Mar "Ionatan" False
goldenCuVierme = Mar "Golden Delicious" True
portocalaSicilia10 = Portocala "Sanguinello" 10
listaFructe = [Mar "Ionatan" False,
                Portocala "Sanguinello" 10,
                Portocala "Valencia" 22,
                Mar "Golden Delicious" True,
                Portocala "Sanguinello" 15,
                Portocala "Moro" 12,
                Portocala "Tarocco" 3,
                Portocala "Moro" 12,
                Portocala "Valencia" 2,
                Mar "Golden Delicious" False,
                Mar "Golden" False,
                Mar "Golden" True]

{-a) Scrieti o functie
ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia = undefined
care indică dacă un fruct este o portocală de Sicilia sau nu. Soiurile de portocale din
Sicilia sunt Tarocco, Moro si Sanguinello.-}

ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia (Mar _ _)=False
ePortocalaDeSicilia (Portocala x _) 
    |x == "Tarocco" || x == "Moro" || x == "Sanguinello" = True 
    |otherwise = False 

{-b) Scrieti o functie
nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia = undefined
test_nrFeliiSicilia = nrFeliiSicilia listaFructe == 52
care calculează numărul total de felii ale portocalelor de Sicilia dintr-o listă de fructe.-}

nr :: Fruct -> Int
nr (Portocala _ x) =x
nr(Mar _ _)=0

nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia [] = 0
nrFeliiSicilia (x:xs) = nr x + nrFeliiSicilia xs 


{-c) Scrieti o functie
nrMereViermi :: [Fruct] -> Int
nrMereViermi = undefined
test_nrMereViermi = nrMereViermi listaFructe == 2
care calculează numărul de mere care au viermi dintr-o lista de fructe.-}


nrv :: Fruct -> Bool
nrv (Portocala _ _) = False 
nrv (Mar _ nr)=True  

nrMereViermi :: [Fruct]->Int
{-nrMereViermi (x:xs) 
    |nrv x = 1 + nrMereViermi xs
    |otherwise = nrMereViermi xs -}

nrMereViermi l = sum [1| Mar soi vierme <- l, vierme] 


type NumeA = String
type Rasa = String 

data Animal = Pisica NumeA | Caine NumeA Rasa
    deriving Show 

{- a) Scrieti o functie
vorbeste :: Animal -> String
vorbeste = undefined 
care întoarce "Meow!" pentru pisică si "Woof!" pentru câine. -}


vorbeste :: Animal -> String 
vorbeste (Pisica _) ="Meow!"
vorbeste (Caine _ _) ="Woof!"

{-b) Vă reamintiti tipul de date predefinit Maybe
data Maybe a = Nothing | Just a
scrieti o functie
rasa :: Animal -> Maybe String
rasa = undefined
care întoarce rasa unui câine dat ca parametru sau Nothing dacă parametrul este o pisică.-}

rasa :: Animal -> Maybe String
rasa (Pisica _) = Nothing 
rasa (Caine _ x) = Just x  

data Linie = L [Int]
    deriving Show

data Matrice = M [Linie]
    deriving Show

{-a) Scrieti o functie care verifica daca suma elementelor de pe fiecare linie este egala
cu o valoare n. Rezolvati cerinta folosind foldr.
verifica :: Matrice -> Int -> Bool
verifica = undefined
test_veri1 = verifica (M[L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) 10 == False
test_verif2 = verifica (M[L[2,20,3], L[4,21], L[2,3,6,8,6], L[8,5,3,9]]) 25 == True-}

verifica :: Matrice -> Int -> Bool
verifica (M l) x = foldr (&&) True [sum a == x | L a <- l]

{-b) Scrieti o functie doarPozN care are ca parametru un element de tip Matrice si un
numar intreg n, si care verifica daca toate liniile de lungime n din matrice au
numai elemente strict pozitive.
doarPozN :: Matrice -> Int -> Bool
doarPozN = undefined
testPoz1 = doarPozN (M [L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) 3 == True
testPoz2 = doarPozN (M [L[1,2,-3], L[4,5], L[2,3,6,8], L[8,5,3]]) 3 == False-}

doarPozN1 :: [Int]-> Bool
doarPozN1 l = foldr (&&) True [x > 0 | x <- l]

doarPozN :: Matrice -> Int -> Bool
doarPozN (M l) nr = foldr (&&) True [doarPozN1 a | L a <- l , length a == nr]


{-c) Definiti predicatul corect care verifică dacă toate liniile dintr-o matrice au aceeasi lungime.
corect :: Matrice -> Bool
corect = undefined
testcorect1 = corect (M[L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) == False
testcorect2 = corect (M[L[1,2,3], L[4,5,8], L[3,6,8], L[8,5,3]]) == True-}

corect :: Matrice -> Bool
corect (M []) = True
corect (M [x]) = True
corect (M (L x : L y : xs))
    |length x == length y = corect (M (L y:xs)) 
    |otherwise = False  


data Expr = Const Int
    |Expr :+: Expr 
    |Expr :*: Expr 
    deriving Eq

data Operation = Add
    |Mult
    deriving (Eq, Show)

data Tree = Lf Int  --frunza
    | Node Operation Tree Tree -- subarbore 
    deriving (Eq, Show)


-- 1.1. Să se instantieze clasa Show pentru tipul de date Expr, astfel încât să se afiseze
-- mai simplu expresiile

instance Show Expr where 
    show :: Expr -> String 
    show (Const x) = show x
    show (a :+: b) = "(" ++ show a ++ show b ++ ")"

{- 1.2 Să se scrie o functie evalExp :: Expr -> Int care evaluează o expresie determinând 
valoarea acesteia.
evalExp :: Expr -> Int
evalExp = undefined
Exemplu:
exp1 = ((Const 2 :*: Const 3) :+: (Const 0 :*: Const 5))
exp2 = (Const 2 :*: (Const 3 :+: Const 4))
exp3 = (Const 4 :+: (Const 3 :*: Const 3))
exp4 = (((Const 1 :*: Const 2) :*: (Const 3 :+: Const 1)) :*: Const 2)
test11 = evalExp exp1 == 6
test12 = evalExp exp2 == 14
test13 = evalExp exp3 == 13
test14 = evalExp exp4 == 16 


evalExp :: Expr -> Int
evalExp (Const x) = x
evalExp (a :+: b) = evalExp a+ evalExp b


{- 1.3. Să se scrie o functie evalArb :: Tree -> Int care evaluează o expresie modelată
sub formă de arbore, determinând valoarea acesteia.
evalArb :: Tree -> Int
evalArb = undefined
arb1 = Node Add (Node Mult (Lf 2) (Lf 3)) (Node Mult (Lf 0)(Lf 5))
arb2 = Node Mult (Lf 2) (Node Add (Lf 3)(Lf 4))
arb3 = Node Add (Lf 4) (Node Mult (Lf 3)(Lf 3))
arb4 = Node Mult (Node Mult (Node Mult (Lf 1) (Lf 2)) (Node Add (Lf 3)(Lf 1))) (Lf 2)
test21 = evalArb arb1 == 6
test22 = evalArb arb2 == 14
test23 = evalArb arb3 == 13
test24 = evalArb arb4 == 16 -}

evalArb :: Tree -> Int
evalArb (Lf x) = x
evalArb (Node Add arb1 arb2) = evalArb arb1 + evalArb arb2 


{-1.4 Să se scrie o functie expToArb :: Expr -> Tree care transformă o expresie în
arborele corespunzător.
expToArb :: Expr -> Tree
expToArb = undefined -}

expToArb :: Expr -> Tree
expToArb (Const x) = Lf x
expToArb (x :+: y) = Node Add (expToArb x) (expToArb y) -}

class Collection c where
    empty :: c key value
    singleton :: key -> value -> c key value
    insert  :: Ord key  => key -> value -> c key value -> c key value
    lookup :: Ord key => key -> c key value -> Maybe value
    delete :: Ord key => key -> c key value -> c key value
    keys :: c key value -> [key]
    keys x = [fst a| a <- toList x]
    values :: c key value -> [value]
    values x = [snd a | a <- toList x]
    toList :: c key value -> [(key, value)]
    fromList :: Ord key => [(key,value)] -> c key value
    fromList ((k,v):t) = insert k v (fromList t) -}

{- 2.1. Adaugati definitii implicite (in functie de functiile celelalte) pentru
a. keys
b. values
c. fromList -}

{- 2.2. Fie tipul listelor de perechi de forma cheie-valoare:
newtype PairList k v
= PairList { getPairList :: [(k, v)] }
Faceti PairList instanta a clasei Collection. -}



{-Se dau următoarele:
Un tip de date ce reprezinta puncte cu numar variabil de coordonate intregi:
data Point = Pt [Int]
deriving Show
Un tip de date ce reprezinta arbori binari de cautare (cu nodurile sortate):
data Arb = Empty | Node Int Arb Arb
deriving Show
O clasă de tipuri ToFromArb
class ToFromArb a where
toArb :: a -> Arb
fromArb :: Arb -> a
Sa se faca o instanta a clasei ToFromArb pentru tipul Point. Inserarea in arbore se va face tinand
cont de proprietatea arborelui de a fi sortat.


data Point = Pt [Int]
    deriving Show


data Arb = Empty | Node Int Arb Arb
    deriving Show

class ToFromArb a where
    toArb :: a -> Arb
    fromArb :: Arb -> a

instance ToFromArb Point where 
    toArb :: Point -> Arb 
    toArb (Pt[])=Empty
    toArb (Pt(x:xs)) = Node x (toArb (Pt (filter (<x) xs))) (toArb (Pt (filter (>=x) xs )))

    fromArb :: Arb -> Point 
    fromArb (Empty) = Pt []
    fromArb (Node x arb1 arb2) = Pt (a1 ++ [x] ++ a2)
        where 
            Pt a1 = fromArb arb1
            Pt a2 = fromArb arb2 

-- Sa se scrie o functie care primeste doua numere intregi si o lista de numere intregi si construieste din
-- lista initiala, lista numerelor aflate in intervalul definit de cele doua numere

verif :: Int -> Int -> [Int] -> [Int]
-- verif x y l
--     |x==y+1 = []
--     |x `elem` l = x : verif (x+1) y l
--     |otherwise = verif (x+1) y l

verif x y l = [a | a<-[x..y] , a `elem` l] 



{-class Functor f where
fmap :: ( a -> b ) -> f a -> f b-}

--Scrieti instante ale clasei Functor pentru tipurile de date descrise mai jos.

newtype Identity a = Identity a

instance Functor Identity where 
    fmap f(Identity a) = Identity (f a)

data Pair a = Pair a a

instance Functor Pair where 
    fmap f(Pair a b) = Pair (f a)(f a)

data Constant a b = Constant b

instance Functor (Constant a) where 
    fmap f (Constant a) = Constant (f a) 


data Two a b = Two a b

instance Functor (Two a) where 
    fmap f(Two a b) = Two a (f b)


data Three a b c = Three a b c
instance Functor (Three a b) where 
    fmap f (Three a b c) = Three a b (f c)


data Three' a b = Three' a b b
instance Functor (Three' a ) where 
    fmap f(Three' a b c) = Three' a (f b)(f c)


data Four a b c d = Four a b c d
instance Functor (Four a b c) where 
    fmap f(Four a b c d) = Four a b c (f d)


data Four'' a b = Four'' a a a b
instance Functor (Four'' a ) where 
    fmap f(Four'' a b c d) = Four'' a b c (f d)


data Quant a b = Finance | Desk a | Bloor b
instance Functor (Quant a) where 
    fmap f(Finance) = Finance
    fmap f(Desk a) = Desk a 
    fmap f (Bloor b)=Bloor (f b)

data LiftItOut f a = LiftItOut (f a)
instance Functor f => Functor (LiftItOut f) where 
    fmap fToA (LiftItOut fA) = LiftItOut (fmap fToA fA)            

 


data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
instance Functor (GoatLord) where 
    fmap f(NoGoat)= NoGoat
    fmap f(OneGoat a)= OneGoat (f a)
    fmap f(MoreGoats a b c)=MoreGoats (fmap f a)(fmap f b)(fmap f c)

data TalkToMe a = Halt | Print String a | Read (String -> a)
instance Functor (TalkToMe) where 
    fmap f (Halt) =Halt
    fmap f(Print b a)=Print b (f a) 
    fmap f(Read stra)=Read (fmap f stra) 


data List a = Nil
    | Cons a (List a)
    deriving (Eq, Show)

--Să se scrie instante Functor si Applicative pentru tipul de date List.
instance Functor List where 
    fmap :: (a->b) -> List a -> List b
    fmap f Nil = Nil
    fmap f (Cons a b) = Cons (f a) (fmap f b)

opp :: List a -> List a -> List a
opp (Nil) l = l
opp (Cons h t) l = Cons h (opp t l) 


instance Applicative List where
    pure :: a -> List a
    pure a = Cons a Nil
    
    (<*>) :: List (a->b) -> List a -> List b
    (<*>) Nil l = Nil
    (<*>) (Cons h t) l = (fmap h l) `opp` (t <*> l) 

data Cow = Cow {
    name :: String
    , age :: Int
    , weight :: Int
    } deriving (Eq, Show)
-}
-- a) Să se scrie functiile noEmpty, respectiv noNegative care valideaza un string,
-- respectiv un intreg

{-noEmpty :: String -> Maybe String
noEmpty = undefined
noNegative :: Int -> Maybe Int
noNegative = undefined
test21 = noEmpty "abc" == Just "abc"
test22 = noNegative (-5) == Nothing


noEmpty :: String -> Maybe String 
noEmpty []=Nothing
noEmpty s=Just s


noNegative :: Int -> Maybe Int
noNegative x 
    | x >= 0 = Just x 
    | otherwise = Nothing
-- b) Sa se scrie o functie care construieste un element de tip Cow verificând numele,
-- varsta si greutatea cu functiile de la a).
-- cowFromString :: String -> Int -> Int -> Maybe Cow
-- cowFromString = undefined
-- test24 = cowFromString "Milka" 5 100 == Just (Cow {name = "Milka", age = 5, weight = 100})

cowFromString :: String -> Int -> Int -> Maybe Cow
{-cowFromString a b c 
    |noEmpty a/= Nothing && noNegative b /= Nothing && noNegative c /= Nothing = Just (Cow a b c)
    |otherwise = Nothing -}

--c) Se se scrie functia de la b) folosind fmap si <*>

cowFromString a b c = fmap Cow (noEmpty a) <*> noNegative b <*> noNegative c 


newtype Name = Name String 
    deriving (Eq, Show)

newtype Address = Address String 
    deriving (Eq, Show)

data Person = Person Name Address
    deriving (Eq, Show)

-- a) Să se implementeze o funct,ie validateLength care validează lungimea unui sir (sa
-- fie mai mică decât numărul dat ca parametru).

validateLength :: Int -> String -> Maybe String 
validateLength x s  
    |length s < x = Just s
    |otherwise = Nothing 

-- b) Să se implementeze funct,iile mkName s, i mkAddress care transformă un sir de
-- caractere într-un element din tipul de date asociat, validând stringul cu functia
-- validateLength (numele trebuie sa aiba maxim 25 caractere iar adresa maxim
-- 100).
-- mkName :: String -> Maybe Name
-- mkName = undefined
-- mkAddress :: String -> Maybe Address
-- mkAddress = undefined

mkName :: String -> Maybe Name 
mkName s
    |validateLength 25 s /= Nothing = Just (Name s)
    |otherwise = Nothing 

mkAddress :: String -> Maybe Address
mkAddress adresa 
    |validateLength 100 adresa == Just adresa = Just (Address adresa) 
    |otherwise = Nothing   

-- Să se implementeze funct,ia mkPerson care primeste ca argument două s, iruri de
-- caractere si formeaza un element de tip Person daca sunt validate condit,iile,
-- folosind functiile implementate mai sus.
-- mkPerson :: String -> String -> Maybe Person
-- mkPerson = undefined

mkPerson :: String -> String -> Maybe Person
{-mkPerson a b 
    |mkName a == Just (Name a) && mkAddress b == Just (Address b) = Just (Person (Name a) (Address b))
    |otherwise = Nothing-}

--d) Să se implementeze funct,iile de la b) si c) folosind fmap s, i <*>

--cowFromString nume varsta gr = fmap Cow (noEmpty nume) <*> noNegative varsta <*> noNegative gr  
mkPerson a b = fmap Person (mkName a) <*> mkAddress b 


-- 2. Scrieti instante ale lui Foldable pentru următoarele tipuri, implementand functia
-- foldMap.

data Constant a b = Constant b 
instance Foldable (Constant a) where 
    foldMap f(Constant a) =f a

data Two a b = Two a b
instance Foldable (Two a) where
    foldMap f(Two a b) = f b


data Three a b c = Three a b c
instance Foldable (Three a b) where 
     foldMap f(Three a b c) = f c 

data Three' a b = Three' a b b
instance Foldable (Three' a) where 
    foldMap f(Three' a b1 b2) = f b1 `mappend` f b2 -}

 

pos :: Int -> Bool
pos  x = if (x>=0) then True else False

fct :: Maybe Int ->  Maybe Bool
--fct  mx =  mx  >>= (\x -> Just (pos x))

fct mx = do 
    x <- mx
    return (pos x)


addM :: Maybe Int -> Maybe Int -> Maybe Int
addM mx my = mx >>= (\x -> (my >>= (\y -> (return (x+y)))))
--addM mx my = mx >>= (\x -> (my >>= \y-> return (x+y)))


-- addM (Just x)(Just y) = Just (x+y)
-- addM _ _=Nothing

-- addM mx my = do
--     x <- mx 
--     y <- my
--     return (x+y)


cartesian_product :: Monad m => m a -> m b -> m (a, b)
--cartesian_product xs ys = xs >>= ( \x -> (ys >>= \y-> return (x,y)))
cartesian_product mx my = do 
    x <- mx 
    y <- my
    return (x,y)

prod :: (t1 -> t2 -> a) -> [t1] -> [t2] -> [a]
--prod f xs ys = [f x y | x <- xs, y<-ys]

prod f mx my = do 
    x <- mx 
    y <- my 
    return (f x y)


myGetLine :: IO String
-- myGetLine = getChar >>= \x ->
--      if x == '\n' then
--          return []
--      else
--          myGetLine >>= \xs -> return (x:xs)

myGetLine = do 
    x <- getChar 
    if x ==  '\n' then 
        return []
    else do 
        xs <- myGetLine 
        return (x:xs)

prelNo :: Floating a => a -> a
prelNo noin = sqrt noin
-- ioNumber = do
-- noin <- readLn :: IO Float
-- putStrLn $ "Intrare\n" ++ (show noin)
-- let noout = prelNo noin
-- putStrLn $ "Iesire"
-- print noout

--newtype WriterS a = Writer { runWriter :: (a, String) }

data Person = Person { name :: String, age :: Int }


showPersonN :: Person -> String
showPersonN (Person a b) = "Name: " ++ a

showPersonA :: Person -> String
showPersonA (Person a b ) = "Varsta: " ++ show b 

showPerson :: Person -> String 
showPerson (Person a b) = showPersonN(Person a b) ++ showPersonA(Person a b)


-- Sa se scrie o functie care primeste doua numere intregi si o lista de numere intregi si construieste din
-- lista initiala, lista numerelor aflate in intervalul definit de cele doua numere

 


--verif :: Ord b => b -> b -> [b] -> [b]
verif a b l = do 
    x <- l 
    if a <= x && x <= b then 
        return x 
    else []

getFromInterval :: Ord b => b -> b -> [b] -> [b]
getFromInterval a b list = do 
    x<-list 
    if a <= x && x <= b then return x else []
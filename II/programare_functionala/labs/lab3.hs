import Data.Char -- pt isDigit si DigitToChar

{-1) Sa se scrie o functie nrVocale care pentru o lista de siruri de caractere, calculeaza numarul
total de vocale ce apar în cuvintele palindrom. Pentru a verifica daca un sir e palindrom,
puteti folosi functia reverse, iar pentru a cauta un element într-o lista puteti folosi functia
elem. Puteti defini oricâte functii auxiliare-}

pal :: String -> Bool
pal l 
    | l == reverse l = True 
    | otherwise = False 

nrVoc :: [Char] -> Int
nrVoc [] =0
nrVoc (h:t)
    |elem h "aeiou" = 1 + nrVoc t
    |otherwise = nrVoc t

nrVocale :: [String] -> Int 
nrVocale []=0
nrVocale (h:t) 
    |pal h = nrVoc h + nrVocale t 
    |otherwise = nrVocale t 

{-2) Sa se scrie o functie care primeste ca parametru un numar si o lista de întregi,si adauga
elementul dat dupa fiecare element par din lista. Sa se scrie si prototipul functiei-}

par :: Int -> [Int] -> [Int]
par _ [] = []
par x (h:t) 
    |even h = h : x : par x t
    |otherwise = h : par x t
    

--3) Sa se scrie o functie care are ca parametru un numar întreg si determina lista de divizori ai acestui numar. Sa se scrie si prototipul functiei.
f :: Int -> [Int]
f x = [i | i <- [1..x], x `mod` i == 0]

--4) Sa se scrie o functie care are ca parametru o lista de numere întregi si calculeaza lista listelor de divizori.
g :: [Int] -> [[Int]]
g [] = []
--g (h:t) = f h : g t
g l = [f x | x <- l]

{-5) Scrieti o functie care date fiind limita inferioara si cea superioara (întregi) a unui interval
închis si o lista de numere întregi, calculeaza lista numerelor din lista care apartin intervalului.
De exemplu:
-- inInterval 5 10 [1..15] == [5,6,7,8,9,10]
-- inInterval 5 10 [1,3,5,2,8,-1] = [5,8]
a) Folositi doar recursie. Denumiti functia inIntervalRec
b) Folositi descrieri de liste. Denumiti functia inIntervalComp-}

--b)cu descriere de liste (comprehesion)
inIntervalComp :: Int -> Int -> [Int] -> [Int] 
inIntervalComp x y l= [i | i <- [x..y], i `elem` l]

--a) cu recursivitate
inInterval :: Int -> Int -> [Int] -> [Int] 
inInterval x y l 
    |x==y+1 = []
    |x `elem` l = x : inInterval (x+1) y l
    |otherwise = inInterval (x+1) y l 

{-6) Scrieti o functie care numara câte numere strict pozitive sunt într-o lista data ca argument.
De exemplu:
-- pozitive [0,1,-3,-2,8,-1,6] == 3
a) Folositi doar recursie. Denumiti functia pozitiveRec
b) Folositi descrieri de liste. Denumiti functia pozitiveComp.
• Nu puteti folosi recursie, dar veti avea nevoie de o functie de agregare.(Consultati
modulul Data.List ). De ce nu e posibil sa scriem pozitiveComp doar folosind descrieri de liste?-}

-- a) recursie 

pozitiveRec :: [Int] -> Int
pozitiveRec [] = 0
pozitiveRec (h:t) = 
    if h>0 
        then 1 + pozitiveRec t
        else pozitiveRec t

-- b) descrieri de liste 
pozitiveComp :: [Int] -> Int
pozitiveComp l = length [x | x <-l , x>0 ]

{-7) Scrieti o functie care data fiind o lista de numere calculeaza lista pozitiilor elementelor impare
din lista originala. De exemplu:
-- pozitiiImpare [0,1,-3,-2,8,-1,6,1] == [1,2,5,7]
a) Folositi doar recursie. Denumiti functia pozitiiImpareRec.
• Indicatie: folositi o functie ajutatoare, cu un argument în plus reprezentând pozitia curenta din lista.
b) Folositi descrieri de liste. Denumiti functia pozitiiImpareComp.
Indicatie: folositi functia zip pentru a asocia pozitii elementelor listei (puteti cauta exemplu în curs)-}

-- a)
pozImp :: [Int] -> Int -> [Int]
pozImp [] _ = []
pozImp (h:t) x =
    if odd h 
        then x : pozImp t (x+1)
        else pozImp t (x+1)

pozitiiImpareRec :: [Int] -> [Int] 
pozitiiImpareRec v = pozImp v 0

-- b)
pozitiiImpareComp :: [Int] -> [Int]
pozitiiImpareComp l = [y | (x,y) <- zip l[1..],  odd x]

{-Scrieti o functie care calculeaza produsul tuturor cifrelor care apar în sirul de caractere dat ca
intrare. Daca nu sunt cifre în sir, raspunsul functiei trebuie sa fie 1 . De exemplu:
-- multDigits "The time is 4:25" == 40
-- multDigits "No digits here!" == 1
a) Folositi doar recursie. Denumiti functia multDigitsRec
b) Folositi descrieri de liste. Denumiti functia multDigitsComp
• Indicatie: Veti avea nevoie de functia isDigit care verifica daca un caracter e cifra si functia digitToInt care 
transforma un caracter in cifra. Cele 2 functii se afla în pachetul Data.Char.-}

-- a)
multDigitsRec :: String -> Int
multDigitsRec [] = 1 
multDigitsRec (h:t)
    |isDigit h = digitToInt h * multDigitsRec t
    |otherwise = multDigitsRec t 

--b)
multDigitsComp :: String -> Int
multDigitsComp s = product[digitToInt x | x <- s, isDigit x]
data Fruct                          -- Fruct este constructor de date iar Mar si Portocala constructori de tip
    = Mar String Bool               --este un tip de date algebric ptc foloseste si suma si produs
    | Portocala String Int 

{- O expresie de tipul Fruct este fie un Mar String Bool sau o Portocala String Int. Vom
folosi un String pentru a indica soiul de mere sau portocale, un Bool pentru a indica
dacă mărul are viermi si un Int pentru a exprima numărul de felii dintr-o portocală-}


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
ePortocalaDeSicilia (Mar _ _) = False 
ePortocalaDeSicilia (Portocala x _) = x `elem` ["Tarocco", "Moro", "Sanguinello"]

{-b) Scrieti o functie
nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia = undefined
test_nrFeliiSicilia = nrFeliiSicilia listaFructe == 52
care calculează numărul total de felii ale portocalelor de Sicilia dintr-o listă de fructe.-}

nrFelii :: Fruct -> Int
nrFelii (Mar _ _) = 0
nrFelii (Portocala _ nr) = nr

nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia [] = 0
nrFeliiSicilia (h:t) = nrFelii h + nrFeliiSicilia t

{-c) Scrieti o functie
nrMereViermi :: [Fruct] -> Int
nrMereViermi = undefined
test_nrMereViermi = nrMereViermi listaFructe == 2
care calculează numărul de mere care au viermi dintr-o lista de fructe.-}

verifViermi :: Fruct -> Bool
verifViermi (Portocala _ _) = False 
verifViermi (Mar _ ver) = True

nrMereViermi :: [Fruct] -> Int
{-nrMereViermi [] = 0
nrMereViermi (h:t)
    |verifViermi h = 1 + nrMereViermi t 
    |otherwise = nrMereViermi t-}

nrMereViermi l = sum [1 | Mar soi vierme <- l, vierme] 
---------------------------------------------------------------------
type NumeA = String
type Rasa = String 

data Animal = Pisica NumeA | Caine NumeA Rasa
    deriving Show 

{-a) Scrieti o functie
vorbeste :: Animal -> String
vorbeste = undefined 
care întoarce "Meow!" pentru pisică si "Woof!" pentru câine.-}

vorbeste :: Animal -> String
vorbeste (Pisica _) = "Meow"
vorbeste (Caine _ _) = "Woof"

{-b) Vă reamintiti tipul de date predefinit Maybe
data Maybe a = Nothing | Just a
scrieti o functie
rasa :: Animal -> Maybe String
rasa = undefined
care întoarce rasa unui câine dat ca parametru sau Nothing dacă parametrul este o pisică.-}

rasa :: Animal -> Maybe String
rasa (Caine _ rasa) = Just rasa
rasa (Pisica _) = Nothing

---------------------------------------------------------------

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
verifica (M l) nr = foldr (&&) True [sum x == nr | L x <- l]


{-b) Scrieti o functie doarPozN care are ca parametru un element de tip Matrice si un
numar intreg n, si care verifica daca toate liniile de lungime n din matrice au
numai elemente strict pozitive.
doarPozN :: Matrice -> Int -> Bool
doarPozN = undefined
testPoz1 = doarPozN (M [L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) 3 == True
testPoz2 = doarPozN (M [L[1,2,-3], L[4,5], L[2,3,6,8], L[8,5,3]]) 3 == False-}

poz :: [Int] -> Bool
poz ls = foldr (&&) True [x > 0 | x <- ls]

doarPozN :: Matrice -> Int -> Bool
doarPozN (M l) nr = foldr (&&) True [ poz x | L x <- l, length x == nr]

--doarPozN (M l) nr = foldr (&&) True [all (>0) x | L x <- l, length x == nr]

{-c) Definiti predicatul corect care verifică dacă toate liniile dintr-o matrice au aceeasi lungime.
corect :: Matrice -> Bool
corect = undefined
testcorect1 = corect (M[L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) == False
testcorect2 = corect (M[L[1,2,3], L[4,5,8], L[3,6,8], L[8,5,3]]) == True-}

corect :: Matrice -> Bool
corect (M []) = True
corect (M [x]) = True
corect (M (L x : L y : t))
    |length x == length y = corect (M(L y : t))
    |otherwise = False 
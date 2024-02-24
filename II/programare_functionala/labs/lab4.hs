
{-1. Folosind numai metoda prin selectie definiti o functie
factori :: Int -> [Int]
factori = undefined
atfel încât factori n întoarce lista divizorilor pozitivi ai lui n-}

factori :: Int -> [Int] 
factori x = [ i | i <- [1..x], x `mod` i == 0]

{-2. Folosind functia factori, definiti predicatul prim n care întoarce True dacă
si numai dacă n este număr prim.
prim :: Int -> Bool
prim = undefined-}

prim :: Int -> Bool 
prim n 
    |length (factori n) == 2 = True
    |otherwise = False 

{-3. Folosind numai metoda prin selectie si functiile definite anterior,definiti functia
numerePrime :: Int -> [Int]
numerePrime = undefined 
astfel încât numerePrime n întoarce lista numerelor prime din intervalul [2..n].-}
numerePrime :: Int -> [Int]
numerePrime n = [i | i <- [2..n], prim i ]

{-4. Definiti functia myzip3 care se comportă asemenea lui zip dar are trei
argumente:myzip3 [1,2,3] [1,2] [1,2,3,4] == [(1,1,1),(2,2,2)]-}

myzip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
myzip3 [] _ _ = []
myzip3 _ [] _ = []
myzip3 _ _ [] = []
myzip3 (x:xs) (y:ys) (z:zs) = (x,y,z) : myzip3 xs ys zs

{-(<+) :: [Int] -> String -> Bool
(<+) [] _ = False
(<+) x y = True-}

{-Rezolvati următoarele exercitii folosind map si fillter (fara recursivitate sau selectie). 
Pentru fiecare functie scrieti si prototipul acesteia.
5. Scrieti o functie generică firstEl care are ca argument o listă de perechi
de tip (a,b) si întoarce lista primelor elementelor din fiecare pereche:
firstEl [('a',3),('b',2), ('c',1)]="abc"
6. Scrieti functia sumList care are ca argument o listă de liste de valori Int si întoarce lista sumelor elementelor 
din fiecare listă (suma elementelor unei liste de întregi se calculează cu functia sum):
sumList [[1,3], [2,4,5], [], [1,3,5,6]]=[4,11,0,15]
7. Scrieti o functie prel2 care are ca argument o listă de Int si întoarce o listă
în care elementele pare sunt înjumătătite, iar cele impare sunt dublate:
*Main> prel2 [2,4,5,6]=[1,2,10,3]
8. Scrieti o functie care primeste ca argument un caracter si o listă de siruri,
rezultatul fiind lista sirurilor care contin caracterul respectiv (folositi functia elem).
9. Scrieti o functie care primeste ca argument o listă de întregi si întoarce lista pătratelor numerelor impare.
10. Scrieti o functie care primeste ca argument o listă de întregi si întoarce lista pătratelor 
numerelor din pozitii impare. Pentru a avea acces la pozitia elementelor folositi zip.
11. Scrieti o functie care primeste ca argument o listă de siruri de caractere si întoarce lista obtinută prin eliminarea 
consoanelor din fiecare sir numaiVocale ["laboratorul", "PrgrAmare", "DEclarativa"] = ["aoaou","Aae","Eaaia"]
12. Definiti recursiv functiile mymap si myfilter cu aceeasi functionalitate ca si functiile predefinite.-}

--5
firstEl :: [(a,b)] -> [a]
firstEl l = map fst l

--6
sumList :: [[Int]] -> [Int]
sumList = map sum

--7
see :: Int -> Int
see x = 
    if odd x 
        then 2*x
        else x `div` 2

prel2 :: [Int] -> [Int] 
prel2 = map see 

--8
verif :: Char -> [String] -> [String]
verif a l= filter (elem a) l 

--9 

patrat :: [Int] -> [Int]
patrat = map (^2) . filter odd

--10

patrat1 :: [Int] -> [Int]
--patrat1 l= map ((^2).snd) . filter (odd.fst) . zip [1..] $ l
patrat1 = map ((^2).snd) . filter (odd.fst) . zip [1..] 

--11
numaiVocale :: [String] -> [String]
numaiVocale l = [filter (`elem` "aeiouAEIOU") x | x <- l] 

--12 
mymap :: (t -> a) -> [t] -> [a]
mymap f [] = []
mymap f (h:t) = f h : mymap f t  

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter f [] = []
myfilter f (h:t)
    |f h = h : myfilter f t 
    |otherwise = myfilter f t 
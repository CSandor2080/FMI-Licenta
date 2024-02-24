{-Rezolvati următoarele exercitii folosind map, filter si fold (fara recursivitate
sau selectie). Pentru fiecare functie scrieti si prototipul acesteia.
1. Calculati suma pătratelor elementelor impare dintr-o listă dată ca parametru.
2. Scrieti o functie care verifică faptul că toate elementele dintr-o listă sunt True, folosind foldr.
3. Scrieti o functie care verifică dacă toate elementele dintr-o listă de numere
întregi satisfac o proprietate dată ca parametru.
allVerifies :: (Int -> Bool) -> [Int] -> Bool
allVerifies = undefined
4. Scrieti o functie care verifică dacă există elemente într-o listă de numere
întregi care satisfac o proprietate dată ca parametru.
anyVerifies :: (Int -> Bool) -> [Int] -> Bool
anyVerifies = undefined
5. Redefiniti functiile map si filter folosind foldr. Le puteti numi mapFoldr si filterFoldr.
6. Folosind functia foldl, definiti functia listToInt care transformă o lista de cifre (un număr foarte mare stocat sub formă de listă) 
în numărul intreg asociat. Se presupune ca lista de intrare este dată corect.
listToInt :: [Integer] -> Integer
listToInt = undefined
-- listToInt [2,3,4,5] = 2345
7.
(a) Scrieti o functie care elimină un caracter din sir de caractere.
rmChar :: Char -> String -> String
rmChar = undefined
(b) Scrieti o functie recursivă care elimină toate caracterele din al doilea argument care se găsesc în primul argument, folosind rmChar.
rmCharsRec :: String -> String -> String
rmCharsRec = undefined
-- rmCharsRec ['a'..'l'] "fotbal" == "ot"
(c) Scrieti o functie echivalentă cu cea de la (b) care foloseste rmChar si foldr în locul recursiei.
rmCharsFold :: String -> String -> String
rmCharsFold = undefined-}

--1
sumI :: [Int] -> Int 
-- sumI l = foldr (+) 0 . map (^2) . filter odd $ l
sumI = foldr (+) 0 . map (^2) . filter odd 

--2

verif :: [Bool] -> Bool
-- verif l= foldr (&&) True l
verif = foldr (&&) True 

--3 
allVerifies :: (Int -> Bool) -> [Int] -> Bool
allVerifies f x = foldr (&&) True (map f x)

--4 
anyVerifies :: (Int -> Bool) -> [Int] -> Bool
anyVerifies f x = foldr (||) False (map f x)

--5
mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr f = foldr (\x xs -> (f x) : xs ) []

filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr f = foldr (\x xs -> if f x then x : xs else xs) []

--6
--show transforma int in char, iar read face string in int
listToInt :: [Integer] -> Integer
--listToInt v = read(foldr(++) "" (map (show) v)) 
-- listToInt l= foldl (\cif nr -> 10*cif + nr ) 0 $ l
listToInt = foldl (\cif nr -> 10*cif + nr ) 0 

--7 a)
rmChar :: Char -> String -> String
--rmChar a= foldr (\h t -> if h == a then t else h :t) []
rmChar c = filter (/=c) 

-- b)
rmCharsRec :: String -> String -> String
rmCharsRec s [] = ""
rmCharsRec s (h:t)
    |h `elem` s = rmCharsRec s (rmChar h t)
    |otherwise = h : rmCharsRec s t 

-- c)
rmCharsFold :: String -> String -> String
rmCharsFold a = foldr (\x xs-> if x `elem` a then (rmChar x xs) else x : xs) []


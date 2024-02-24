--1. Să se scrie o functie poly2 care are patru argumente de tip Double, a,b,c,x s,i calculează a*xˆ2+b*x+c.

poly2 :: Double -> Double -> Double -> Double -> Double
poly2 a b c x = a*(x*2)+b*x+c

--2. Să se scrie o functie eeny care întoarce “eeny” pentru input par si “meeny” pentru input impar. Hint:puteti folosi functia even

eeny :: Int -> String 
eeny x = 
    if odd x 
        then "meeny" 
        else "eeny"

{-3. Să se scrie o functie fizzbuzz care întoarce “Fizz” pentru numerele divizibile cu 3, “Buzz” pentru
numerele divizibile cu 5 si “FizzBuzz” pentru numerele divizibile cu ambele. Pentru orice alt număr se
întoarce sirul vid. Pentru a calcula modulo a două numere puteti folosi functia mod. Să se scrie această
functie în 2 moduri: folosind if si folosind gărzi (conditii).-} 

{-fizzbuzz :: Int -> String 
fizzbuzz x = 
    if mod x 3 == 0 && mod x 5 == 0
        then "Fizzbuzz"
        else if x `mod` 5 == 0 
                then "Buzz"
                else if mod x 3 == 0 
                        then "Fizz"
                        else ""-}
fizzbuzz :: Int-> String  
fizzbuzz x  
    |mod x 15 == 0 = "Fizzbuzz"
    |mod x 3 == 0 = "Fizz"
    |mod x 5 == 0 = "Buzz"
    |otherwise = ""

--sirul lui fibonacci
fibonacciCazuri :: Integer -> Integer
{-ecuational
fibonacciCazuri 0 = 0
fibonacciCazuri 1 = 1 
fibonacciCazuri n = fibonacciCazuri(n-1) + fibonacciCazuri(n-2)

-- cu if
fibonacciCazuri n = 
    if n >= 2 
        then fibonacciCazuri(n-1) + fibonacciCazuri(n-2)
        else n-}
-- cu garzi
fibonacciCazuri n
    | n >= 2 =fibonacciCazuri (n - 1) + fibonacciCazuri (n - 2)
    | otherwise = n
{-4. Numerele tribonacci sunt definite de ecuatia
Tn = 
1 dacă n = 1
1 dacă n = 2
2 dacă n = 3
Tn−1 + Tn−2 + Tn−3 dacă n > 3
Să se implementeze functia tribonacci atât cu cazuri cât si ecuational-}
{-ecuational

tribonacci 1 = 1
tribonacci 2 = 1 
tribonacci 3 = 2 
tribonacci n = tribonacci(n-1) + tribonacci (n-2) + tribonacci(n-3)-}

--cu cazuri 
tribonacci :: Integer -> Integer
tribonacci n = 
    if n>3
        then tribonacci(n-1) + tribonacci(n-2) + tribonacci (n-3)
        else if n==3 
                then 2 
                else if n==2 || n==1
                        then 1
                        else undefined

{-6. Să se implementeze următoarele functii folosind liste:
a) verifL - verifică dacă lungimea unei liste date ca parametru este pară-}
verifL :: [Int] -> Bool
--verifL x = even(length x)
verifL l 
    | even (length l) = True
    | otherwise = False 
--b) takefinal - pentru o listă dată ca parametru si un număr n, întoarce lista cu ultimele n elemente.
--Dacă lista are mai putin de n elemente, se intoarce lista nemodificată

takefinal :: [Int] -> Int -> [Int]
takefinal l n = 
    if n <= length l
        then reverse (take n l)
        else l

--c) remove - pentru o listă si un număr n se întoarce lista din care se sterge elementul de pe pozitia n.
--(Hint: puteti folosi functiile take si drop). Scriti si prototipul functiei.
 
remove :: [Int] -> Int -> [Int]
remove v n = take (n-1) v ++ drop (n) v  

--Dată fiind o listă de numere întregi, să se scrie o functie ‘semiPareRec' care elimină 
--numerele impare si le injumătăteste pe cele pare.

semiPareRec :: [Int] -> [Int]
semiPareRec [] = []
semiPareRec (h:t)
    | even h = h `div` 2 : semiPareRec t
    | otherwise = semiPareRec t

{-7. Exercitii: să se scrie urmatoarele functii folosind recursivitate:
a) myreplicate - pentru un întreg n si o valoare v întoarce lista de lungime n ce are doar elemente egale
cu v. Să se scrie si prototipul functiei.
b) sumImp - pentru o listă de numere întregi, calculează suma valorilor impare. Să se scrie si prototipul
functiei.
c) totalLen - pentru o listă de siruri de caractere, calculează suma lungimilor sirurilor care încep cu
caracterul ‘A’.-}

myreplicate :: Int -> Int -> [Int]
myreplicate x y 
    | x == 0 = []
    | otherwise = y : myreplicate(x-1) y 

sumImp :: [Int] -> Int
sumImp [] = 2
sumImp (h:t) 
    | odd h = h + sumImp t 
    | otherwise = sumImp t

totalLen :: [[Char]] -> Int
totalLen [] = 0
totalLen (h:t)
    |take 1 h == "A" = length h + totalLen t 
    |otherwise = totalLen t
{- Să se scrie următoarele functii:
a) functie cu 2 parametri care calculeaza suma pătratelor celor două numere;
b) functie cu un parametru ce întoarce mesajul “par” dacă parametrul este par si “impar” altfel;
c) functie care calculează factorialul unui număr;
d) functie care verifică dacă primul parametru este mai mare decât dublul celui de-al doilea
parametru-}
f :: Int -> Int ->Int 
f x y = x*x + y*y

g :: Int -> String
g x = 
    if odd x
        then "impar"
        else "par"

fact :: Int -> Int
fact 0 = 1 
fact n = n * fact(n-1)

verif :: Int -> Int -> Bool
verif x y =
    if x > 2*y
        then True
        else False 
 
import Data.Type.Bool (Not)
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

-- .1 Să se instantieze clasa Show pentru tipul de date Expr, astfel încât să se afiseze mai simplu expresiile.

instance Show Expr where 
    show :: Expr -> String
    show (Const a) = show a 
    show (e1 :+: e2 ) = "(" ++ show e1 ++ "+" ++ show e2 ++ ")"
    show (e1 :*: e2 ) = "(" ++ show e1 ++ "*" ++ show e2 ++ ")"

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
test14 = evalExp exp4 == 16 -}

evalExp :: Expr -> Int
evalExp (Const a) = a
evalExp (e1 :+: e2) = evalExp e1 + evalExp e2
evalExp (e1 :*: e2) = evalExp e1 * evalExp e2

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
evalArb (Lf a) = a 
evalArb (Node Add e1 e2) = evalArb e1 + evalArb e2
evalArb (Node Mult e1 e2) = evalArb e1 * evalArb e2

{-1.4 Să se scrie o functie expToArb :: Expr -> Tree care transformă o expresie în
arborele corespunzător.
expToArb :: Expr -> Tree
expToArb = undefined -}

expToArb :: Expr -> Tree
expToArb (Const a) = Lf a
expToArb (e1 :+: e2) = Node Add (expToArb e1) (expToArb e2)
expToArb (e1 :*: e2) = Node Mult (expToArb e1) (expToArb e2)


{- In acest exercitiu vom exersa manipularea listelor si tipurilor de date prin
implementarea catorva colectii de tip tabela asociativa cheie-valoare.
Aceste colectii vor trebui sa aiba urmatoarele facilitati
• crearea unei colectii vide
• crearea unei colectii cu un element
• adaugarea/actualizarea unui element intr-o colectie
• cautarea unui element intr-o colectie
• stergerea (marcarea ca sters a) unui element dintr-o colectie
• obtinerea listei cheilor
• obtinerea listei valorilor
• obtinerea listei elementelor -}

class Collection c where 
    empty :: c key value 
    singleton :: key -> value -> c key value 
    insert :: Ord key => key -> value -> c key value -> c key value 
    clookup :: Ord key => key -> c key value -> Maybe value 
    delete :: Ord key => key -> c key value -> c key value 
    keys :: c key value -> [key]
    keys x = [fst a| a <- toList x]
    -- keys = map fst . toList
    values :: c key value -> [value]
    values x = [snd a | a <- toList x ]
    -- values = map snd . toList
    toList :: c key value -> [(key, value)]
    fromList :: Ord key => [(key, value)] -> c key value 
    fromList [] = empty
    fromList ((k,v):t) = insert k v (fromList t)  

{- 2.1. Adaugati definitii implicite (in functie de functiile celelalte) pentru
a. keys
b. values
c. fromList -}

{- 2.2. Fie tipul listelor de perechi de forma cheie-valoare:
newtype PairList k v
= PairList { getPairList :: [(k, v)] }
Faceti PairList instanta a clasei Collection. -}

newtype PairList k v = PairList { getPairList :: [(k, v)] }

instance Collection PairList where 
    empty :: PairList key value 
    empty = PairList []
    
    singleton :: key -> value -> PairList key value
    singleton k v = PairList [(k,v)]

    insert :: Ord key => key -> value -> PairList key value -> PairList key value
    insert k v (PairList l) = PairList $ (k,v) : filter (\(k1,v1) -> k1/=k) l

    clookup :: Ord key => key -> PairList key value -> Maybe value 
    clookup k = lookup k . getPairList

    delete :: Ord key => key -> PairList key value -> PairList key value 
    delete k (PairList l) = PairList $ filter (\(k1, v1) -> k1 /= k) l 
    --delete k (PairList l) = PairList $ filter ((/=k).fst) l  

    keys :: PairList key value -> [key]
    keys (PairList l) = [fst a | a <- toList (PairList l)]

    values :: PairList key value -> [value]
    values (PairList l) = [snd a | a <- toList (PairList l)]

    toList :: PairList key value -> [(key,value)]
    toList = getPairList

    fromList :: Ord key => [(key,value)] -> PairList key value 
    fromList [] = empty
    fromList ( (k,v) : t) = insert k v ( fromList t)   -- ??


{- 2.3 Fie tipul arborilor binari de cautare (ne-echilibrati):
data SearchTree key value
= Empty
| BNode
    (SearchTree key value) -- elemente cu cheia mai mica
    key -- cheia elementului
    (Maybe value) -- valoarea elementului
    (SearchTree key value) -- elemente cu cheia mai mare
Observati ca tipul valorilor este Maybe value. Acest lucru se face pentru a reduce timpul
operatiei de stergere prin simpla marcare a unui nod ca fiind sters. Un nod sters va
avea valoarea Nothing.
Faceti SearchTree instanta a clasei Collection. -}

data SearchTree key value
  = Empty
  | BNode
      (SearchTree key value) 
      key (Maybe value)           
      (SearchTree key value)  

instance Collection SearchTree where 
    empty :: SearchTree key value 
    empty = Empty

    singleton :: key -> value -> SearchTree key value 
    singleton k v = BNode Empty k (Just v) Empty

    insert :: Ord key => key -> value -> SearchTree key value -> SearchTree key value 
    insert k v Empty = singleton k v
    insert k v (BNode t1 k1 v1 t2)
        | k == k1 = BNode t1 k1 (Just v) t2
        | k < k1 = BNode (insert k v t1) k1 v1 t2 
        | k > k1 = BNode t1 k1 v1 (insert k v t2)

    clookup :: Ord key => key -> SearchTree key value -> Maybe value
    clookup k (BNode t1 k1 v1 t2)
        | k==k1 = v1
        | k<k1 = clookup k t1
        | otherwise = clookup k t2

    delete :: Ord key => key -> SearchTree key value -> SearchTree key value
    delete k Empty = Empty
    delete k (BNode t1 k1 v1 t2)
        | k == k1 = BNode t1 k1 Nothing t2
        | k < k1 = BNode (delete k t1) k1 v1 t2
        | k > k1 = BNode t1 k1 v1 (delete k t2) 

    keys :: SearchTree key value -> [key]
    keys Empty = []
    keys (BNode t1 k v t2) = keys t1 ++ val k v ++ keys t2
        where 
            val _ Nothing = []
            val k (Just v) = [k]

    values :: SearchTree key value -> [value]
    values Empty = []
    values (BNode t1 k v t2) = values t1 ++ val1 k v ++ values t2
        where 
            val1 _ Nothing = []
            val1 k (Just v) = [v]

    toList :: SearchTree key value -> [(key, value)]
    toList Empty = []
    toList (BNode t1 k v t2) = toList t1 ++ val2 k v ++ toList t2 
        where 
            val2 _ Nothing = []
            val2 k (Just v) = [(k,v)]

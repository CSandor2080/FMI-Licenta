import Data.Maybe (fromJust)
import Data.List (nub)

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

{- Tipul Prop este o reprezentare a formulelor propozitionale. Variabilele propozitionale,
precum p si q pot fi reprezentate ca Var "p" si Var "q". În plus, constantele booleene
F si T reprezintă false si true, operatorul unar Not reprezintă negatia (¬; a nu se
confunda cu functia not :: Bool -> Bool) si operatorii (infix) binari :|: si :&:reprezintă
disjunctia (∨) si conjunctia (∧). -}

-- ex 1. Scrieti următoarele formule ca expresii de tip Prop, denumindu-le p1, p2, p3.
-- 1. (P ∨ Q) ∧ (P ∧ Q)
p1 :: Prop
p1 = (Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q")
--2. (P ∨ Q) ∧ (¬P ∧ ¬Q)
p2 :: Prop
p2 = (Var "P" :|: Var "Q") :&: (Not (Var "P") :&: Not (Var "Q")) 

--3. (P ∧ (Q ∨ R)) ∧ ((¬P ∨ ¬Q) ∧ (¬P ∨ ¬R))
p3 :: Prop
p3 = (Var "P" :&: (Var "Q" :|: Var "R")) :&: ((Not (Var "P") :|: Not (Var "Q")) :&: (Not (Var "P") :|: Not (Var "R"))) 

{- Ex 2. Faceti tipul Prop instantă a clasei de tipuri Show, înlocuind conectivele Not, :|: s, i :&:
cu ~, | si & si folosind direct numele variabilelor în loc de constructia Var nume.
instance Show Prop where
show = undefined
test_ShowProp :: Bool
test_ShowProp =
show (Not (Var "P") :&: Var "Q") == "((~P)&Q)" -}

instance Show Prop where 
    show :: Prop -> String
    show (Var p) = p
    show (F) = "False"
    show (T) = "True"
    show (Not p) = "(" ++ "~" ++ show p ++ ")"
    show (p :&: q) = "(" ++ show p ++ "&" ++ show q ++ ")"
    show (p :|: q) = "(" ++ show p ++ "|" ++ show q ++ ")"
    show (p :->: q) = "(" ++ show p ++ "->" ++ show q ++ ")"
    show (p :<->: q) = "(" ++ show p ++ "<->" ++ show q ++ ")"
test_ShowProp :: Bool
test_ShowProp =
    show (Not (Var "P") :&: Var "Q") == "((~P)&Q)"

{-Ex 3.Definiti o functie eval care dat fiind o expresie logică si un mediu de evaluare,
calculează valoarea de adevăr a expresiei.
eval :: Prop -> Env -> Bool
eval = undefined
test_eval = eval (Var "P" :|: Var "Q") [("P", True), ("Q", False)] == True -}

type Env = [(Nume, Bool)] 

impureLookup :: Eq a => a -> [(a,b)] -> b
impureLookup a = fromJust.lookup a 

eval :: Prop -> Env -> Bool
eval F e = False 
eval T e = True 
eval (Var v) e = impureLookup v e
eval (Not (Var v)) e = not(impureLookup v e)  -- de ce nu merge not??
eval (p :|: q) e = (eval p e) || (eval q e) 
eval (p :&: q) e = (eval p e) && (eval q e)

{- Definiti o functie variabile care colectează lista tuturor variabilelor dintr-o formulă.
Indicatie: folositi functia nub.
variabile :: Prop -> [Nume]
variabile = undefined
test_variabile =
variabile (Not (Var "P") :&: Var "Q") == ["P", "Q"] -}

variable :: Prop -> [Nume]
variable (F) = []
variable (T) = []
variable (Var x) = [x]
variable (Not (Var x)) = [x]
variable (p :|: q) = nub(variable p ++ variable q) 
variable (p :&: q) = nub(variable p ++ variable q) 

{- Ex 5. Dată fiind o listă de nume, definiti toate atribuirile de valori de adevăr
posibile pentru ea.
envs :: [Nume] -> [Env]
envs = undefined
test_envs =
envs ["P", "Q"]
==
[ [ ("P",False)
, ("Q",False)
  ]
, [ ("P",False)
, ("Q",True)
  ]
, [ ("P",True)
, ("Q",False)
  ]
, [ ("P",True)
, ("Q",True)
  ]
] -}

envs :: [Nume] -> [Env]
envs [] = [[]]
envs [p] = [[(p, False)],[(p, True)]]
envs(h:t) = let r = envs t in map((h,False):) r ++ map((h,True):) r



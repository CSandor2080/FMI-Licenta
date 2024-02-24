{-Subiectul 1 [ADT + Clase] [2pct]
Se dau următoarele:
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
Subiectul 2 [Liste + Monade] [3 pct]
Sa se scrie o functie care primeste doua numere intregi si o lista de numere intregi si construieste din
lista initiala, lista numerelor aflate in intervalul definit de cele doua numere. Sa se rezolve problema in
doua moduri (o solutie fara monade si o solutie cu monade).
getFromInterval 5 7 [1..10] == [5,6,7]
Subiectul 3 [Monade] [1 pct]
Se da tipul de date
newtype ReaderWriter env a = RW {getRW :: env-> (a,String)}
Sa se scrie instanta completa a clasei Monad pentru tipul ReaderWriter, astfel incat sa pastreze
proprietatea de monada, env fiind o memorie nemodificabila si concatenand toate stringurile. Nu este
nevoie sa faceti instante si pentru clasele Applicative si Functor. -}


data Point = Pt [Int]
  deriving Show

data Arb = Empty | Node Int Arb Arb
  deriving Show

class ToFromArb a where
  toArb :: a -> Arb
  fromArb :: Arb -> a 

instance ToFromArb Point where 
  toArb (Pt []) = Empty 
  toArb (Pt (x:xs)) = Node x (toArb (Pt (filter (< x) xs)))  (toArb (Pt (filter (>= x) xs)))
  fromArb Empty = Pt [] 
  fromArb (Node x st dr) = let Pt l1 = fromArb st 
                               Pt l2 = fromArb dr
                           in Pt (l1 ++ [x] ++ l2)

-- Subiectul 2
-- cu selectie 
getFromIntervalSel a b list = [x | x<-list, x>=a, x<=b]
-- monade
getFromInterval a b list = do 
    x<-list 
    if a <= x && x <= b then return x else []

-- Subiectul 3
newtype ReaderWriter env a = RW {getRW :: env-> (a,String)}

instance Monad (ReaderWriter env) where
  return va = RW (\_ -> (va,""))
  ma >>= k = RW f 
      where f env = let (va, str1) = getRW ma env
                        (vb, str2)  = getRW (k va) env
                    in (vb, str1 ++ str2)

instance Applicative (ReaderWriter env) where
  pure = return
  mf <*> ma = do
    f <- mf
    va <- ma
    return (f va)       

instance Functor (ReaderWriter env) where              
  fmap f ma = pure f <*> ma  
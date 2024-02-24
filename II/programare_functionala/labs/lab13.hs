import System.Win32 (COORD(yPos), mAXIMUM_ALLOWED)
import Foreign (FinalizerEnvPtr)
{- Monada Maybe este definita in GHC.Base 

instance Monad Maybe where
  return = Just
  Just va  >>= k   = k va
  Nothing >>= _   = Nothing

instance Applicative Maybe where
  pure = return
  mf <*> ma = do
    f <- mf
    va <- ma
    return (f va)       

instance Functor Maybe where              
  fmap f ma = pure f <*> ma  

return 3 :: Maybe Int
Just 3
(Just 3) >>= (\ x -> if (x>0) then Just (x*x) else Nothing) 
-}

pos :: Int -> Bool
pos  x = if (x>=0) then True else False

fct :: Maybe Int ->  Maybe Bool
--fct  mx =  mx  >>= (\x -> Just (pos x))

-- Definiti functia fct folosind notatia do.

fct mx = do
    x <- mx 
    return (pos x)

--Vrem să definim o functie care adună două valori de tip Maybe Int


addM :: Maybe Int -> Maybe Int -> Maybe Int
--varianta fara monade
--addM (Just x) (Just y) = Just (x+y)
--addM _ _ = Nothing
    
--varianta cu monade
--fara do
-- add mx my = mx >>= (\x -> (my >>= \y-> return (x+y)))

--cu do
addM mx my = do
    x <- mx 
    y <- my
    return (x+y)

cartesianProduct :: Monad m => m a1 -> m a2 -> m (a1, a2)
--cartesianProduct xs ys = xs >>= ( \x -> (ys >>= \y-> return (x,y)))
cartesianProduct xs ys = do
    x <- xs 
    y <- ys 
    return (x,y)

--prod :: (t1 -> t2 -> a) -> [t1] -> [t2] -> [a]
--prod f xs ys = [f x y | x <- xs, y<-ys]

prod f xs ys = do
    x <- xs 
    y <- ys 
    return (f x y)

myGetLine :: IO String
-- myGetLine = getChar >>= \x ->
--       if x == '\n' then
--           return []
--       else
--           myGetLine >>= \xs -> return (x:xs)

myGetLine = do
    x <- getChar
    if x == '\n' then
        return []
    else do
        xs <- myGetLine
        return (x:xs)

prelNo noin =  sqrt noin

-- ioNumber = do
--      noin  <- readLn :: IO Float
--      putStrLn $ "Intrare\n" ++ (show noin) 
--     -- analog _ <- putStrLn $ "Intrare\n" ++ (show noin)
--      let  noout = prelNo noin
--      putStrLn $ "Iesire"
--      print noout


-- ioNumber = do
--     (readLn :: IO Float)  
--     >>= \noin -> putStrLn ("Intrare\n" ++ (show noin)) 
--     >>= \ _ ->  let  noout = prelNo noin in
--                 putStrLn "Iesire" 
--     >>= \ _ ->  print noout


-- analog scriere mai simpla
ioNumber = do
    (readLn :: IO Float)  
    >>= \noin -> putStrLn ("Intrare\n" ++ show noin) 
    >>  (let  noout = prelNo noin in
                putStrLn "Iesire" 
        >>  print noout)

-- ===============================
    
--- Monada Writer

newtype WriterS a = Writer { runWriter :: (a, String) } 

instance  Monad WriterS where
  return va = Writer (va, "")
  ma >>= k = let (va, log1) = runWriter ma
                 (vb, log2) = runWriter (k va)
             in  Writer (vb, log1 ++ log2)


instance  Applicative WriterS where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance  Functor WriterS where              
  fmap f ma = pure f <*> ma     

tell :: String -> WriterS () 
tell log = Writer ((), log)
  
logIncrement :: Int  -> WriterS Int
logIncrement x = undefined

logIncrementN :: Int -> Int -> WriterS Int
logIncrementN x n =  undefined

-- ===============================

data Person = Person { name :: String, age :: Int }

showPersonN :: Person -> String
showPersonN (Person a b) = "NAME: " ++ a
showPersonA :: Person -> String
showPersonA (Person a b)= "AGE: " ++ show b
{-
showPersonN $ Person "ada" 20
"NAME: ada"
showPersonA $ Person "ada" 20
"AGE: 20"
-}

showPerson :: Person -> String
showPerson (Person a b) = "(NAME: " ++ a ++ ", AGE: " ++ show b ++ ")"
{-
showPerson $ Person "ada" 20
"(NAME: ada, AGE: 20)"
-}

newtype Reader env a = Reader { runReader :: env -> a }

instance Monad (Reader env) where
  return x = Reader (\_ -> x)
  ma >>= k = Reader f
    where f env = let a = runReader ma env
                  in  runReader (k a) env


instance Applicative (Reader env) where
  pure :: a -> Reader env a
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance Functor (Reader env) where              
  fmap f ma = pure f <*> ma    


ask::Reader env env 
ask= Reader(\env -> env)

mshowPersonN ::  Reader Person String
mshowPersonN = do
    pers <- ask
    return $ "NAME: " ++ name pers

mshowPersonA ::  Reader Person String
mshowPersonA = do
    pers <- ask
    return $ "AGE: " ++ show (age pers) 

mshowPerson ::  Reader Person String
mshowPerson = do 
    name <- mshowPersonN
    age <- mshowPersonA
    return $ "(" ++ name ++ "," ++ age ++","

{-
runReader mshowPersonN  $ Person "ada" 20
"NAME:ada"
runReader mshowPersonA  $ Person "ada" 20
"AGE:20"
runReader mshowPerson  $ Person "ada" 20
"(NAME:ada,AGE:20)"
-}
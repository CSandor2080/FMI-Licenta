
{-class Functor f where
fmap :: (a -> b) -> f a -> f b
class Functor f => Applicative f where
pure :: a -> f a
(<*>) :: f (a -> b) -> f a -> f b
Just length <*> Just "world"
Just (++" world") <*> Just "hello,"
pure (+) <*> Just 3 <*> Just 5
pure (+) <*> Just 3 <*> Nothing
(++) <$> ["ha","heh"] <*> ["?","!"]
-}

--fmap :: (a -> b) -> f a -> f b
--pure :: a -> f a 

--(pure 1 :: Maybe Int) == Just 1 



data List a = Nil
    | Cons a (List a)
    deriving (Eq, Show)

instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons a b) = Cons (f a) (fmap f b)

opp :: List a -> List a -> List a
opp (Nil) l2 = l2
opp (Cons h t) l2 = Cons h (opp t l2)


instance Applicative List where 

    pure :: a -> List a
    pure a = Cons a Nil
    
    (<*>) :: List (a -> b) -> List a -> List b
    (<*>) Nil l2 = Nil
    (<*>) (Cons h t) l2 = (fmap h l2) `opp` (t <*> l2)


data Cow = Cow {
    name :: String
    , age :: Int
    , weight :: Int
    } deriving (Eq, Show)


noEmpty :: String -> Maybe String
noEmpty [] = Nothing
noEmpty a = Just a 
noNegative :: Int -> Maybe Int
noNegative x 
    | x >= 0 = Just x 
    | otherwise = Nothing


cowFromString :: String -> Int -> Int -> Maybe Cow
{-cowFromString s a b 
    | noEmpty s /= Nothing && noNegative a /= Nothing && noNegative b /= Nothing = Just (Cow s a b) 
    | otherwise = Nothing  -}

--Cow :: String -> Int -> Int -> Cow 
cowFromString nume varsta gr = fmap Cow (noEmpty nume) <*> noNegative varsta <*> noNegative gr        
    -- fmap Cow (noEmpty nume) :: Maybe (Int -> Int -> Cow)
    -- fmap Cow (noEmpty nume) = <*> noNegative varsta :: Maybe (Int -> Cow)
    -- fmap Cow (noEmpty nume) = <*> noNegative varsta <*> noNegative gr   :: Maybe Cow



newtype Name = Name String 
    deriving (Eq, Show)
newtype Address = Address String 
    deriving (Eq, Show)
data Person = Person Name Address
    deriving (Eq, Show)

validateLength :: Int -> String -> Maybe String
validateLength x s 
    |length s < x = Just s
    | otherwise = Nothing 

mkName :: String -> Maybe Name
mkName nume
    |validateLength 25 nume == Just nume = Just (Name nume)
    |otherwise = Nothing  
mkAddress :: String -> Maybe Address
mkAddress adresa 
    |validateLength 100 adresa == Just adresa = Just (Address adresa) 
    |otherwise = Nothing    

mkPerson :: String -> String -> Maybe Person
mkPerson nume adresa 
    |mkName nume == Just (Name nume) && mkAddress adresa == Just (Address adresa) = Just (Person (Name nume) (Address adresa))
    |otherwise = Nothing 

mkPerson1 :: String -> String -> Maybe Person
mkPerson1 nume adresa = fmap Person (mkName nume) <*> mkAddress adresa 


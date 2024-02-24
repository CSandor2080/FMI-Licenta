import Control.Applicative
import Data.Char
import System.Win32 (xBUTTON1, COORD (xPos))
import Control.Monad (void)  

newtype Parser a = Parser { apply :: String -> [(a, String)] }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser go
  where
    go [] = []   -- imposibil de parsat șirul vid
    go (c:input)
      | p c = [(c, input)]   -- dacă predicatul ține, întoarce c și restul șirului de intrare
      | otherwise = []       -- în caz contrar, imposibil de parsat

anychar :: Parser Char
anychar = satisfy (\_ -> True)

--- | acceptă doar caracterul dat ca argument
char :: Char -> Parser Char
char c = satisfy (\x-> x == c) 

--- | acceptă o cifră
digit :: Parser Char
digit = satisfy isDigit 

--- | acceptă un spațiu (sau tab, sau sfârșit de linie -- vedeți funcția din Data.Char )
space :: Parser Char
space = satisfy isSpace

--- | succes doar dacă am șirul de intrare este vid 
endOfInput :: Parser ()
endOfInput  = Parser go
  where
    go "" = [((), "")]
    go _ = []

instance Functor Parser where
    fmap f pa = Parser (\input -> [(f a, rest) | (a, rest) <- apply pa input])

instance Applicative Parser where
    pure a = Parser (\input -> [(a, input)])
    pf <*> pa = Parser (\input -> [(f a, resta) | (f, restf) <- apply pf input, (a, resta) <- apply pa restf])

parse :: Parser a -> String -> Either String a
parse = undefined

instance Monad Parser where
    pa >>= k = Parser (\input -> [(b, restb) | (a, resta) <- apply pa input, (b, restb) <- apply (k a) resta])

cifraSemn :: Parser Int
cifraSemn = do
    ch <- satisfy(\x -> elem x "+-")
    d <- digitToInt <$> digit
    case ch of
      '+' -> return d
      _ -> return (-d)

string :: String -> Parser String
string (x:xs) = do
    char x
    string xs
    return (x:xs)

instance Alternative Parser where
    empty = Parser (const [])
    p <|> p' = Parser (\input -> apply p input ++ apply p' input)

naiveNatural :: Parser Int
naiveNatural = read <$> some digit

-- | Elimină zero sau mai multe apariții ale lui `space`
whiteSpace :: Parser ()
whiteSpace = many (satisfy isSpace) *> pure ()

-- | parses a natural number (one or more digits)
nat :: Parser Int
nat = read <$> some (satisfy isDigit)

-- | aplică un parser, și elimină spațiile de după
lexeme :: Parser a -> Parser a
lexeme p = p <* whiteSpace

-- | parses a natural number and skips the space after it
natural :: Parser Int
natural = lexeme nat

-- | Parses the string and skips whiteSpace after it
symbol :: String -> Parser String
symbol str = lexeme (string str)

-- | Parses the string, skips whiteSpace, returns unit
reserved :: String -> Parser ()
reserved str = lexeme (string str) *> pure ()

-- | parsează virgulă, eliminând spațiile de după
comma :: Parser ()
comma = void $ symbol ","

-- | parsează argumentul intre paranteze rotunde
--   elimină spațiile de după paranteze
parens :: Parser a -> Parser a
parens parser = lexeme (symbol "(") *> parser <* lexeme (symbol ")")

-- | parsează argumentul intre paranteze pătrate
--   elimină spațiile de după paranteze
brackets :: Parser a -> Parser a
brackets parser = lexeme (symbol "[") *> parser <* lexeme (symbol "]")

-- | una sau mai multe instanțe, separate de virgulă,
--   cu eliminarea spațiilor de după fiecare virgulă
--   intoarce lista obiectelor parsate
commaSep1 :: Parser a -> Parser [a]
commaSep1 parser = (:) <$> parser <*> many (lexeme (symbol ",") *> parser)

-- | zero sau mai multe instanțe, separate de virgulă,
--   cu eliminarea spațiilor de după fiecare virgulă
--   intoarce lista obiectelor parsate
commaSep :: Parser a -> Parser [a]
commaSep parser = commaSep1 parser <|> pure []

-- | date fiind parsere pentru prima literă si pentru felul literelor următoare
--   scrieți un parser pentru un identificator
ident :: Parser Char -> Parser Char -> Parser String
ident identStart identLetter = (:) <$> identStart <*> many identLetter

-- | ca mai sus, dar elimină spatiile de după
identifier :: Parser Char -> Parser Char -> Parser String
identifier start letter = lexeme (ident start letter)

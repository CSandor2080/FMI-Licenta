import Data.Char (isSpace) 

-- | Elimină zero sau mai multe apariții ale lui `space`
whiteSpace :: Parser ()
whiteSpace = () <$ many (satisfy isSpace)

-- | parses a natural number (one or more digits)
nat :: Parser Int
nat = read <$> some (satisfy isDigit)

-- | aplică un parser, și elimină spațiile de după
lexeme :: Parser a -> Parser a
lexeme p = p <* whiteSpace

-- | Parses a natural number and skips the space after it
natural :: Parser Int
natural = lexeme nat

-- | Parses the string and skips whiteSpace after it
symbol :: String -> Parser String
symbol s = lexeme (string s)

-- | Parses the string, skips whiteSpace, returns unit
reserved :: String -> Parser ()
reserved s = symbol s *> pure ()

-- | parsează virgulă, eliminând spațiile de după
comma :: Parser ()
comma = symbol "," *> pure ()


-- elimină zero sau mai multe apariții ale lui `space`
whiteSpace :: Parser ()
whiteSpace = many (oneOf " \t\n") *> pure ()

-- parsează un număr natural (una sau mai multe cifre)
nat :: Parser Int
nat = read <$> some (oneOf "0123456789")

-- parsează număr natural și elimină spațiile de după el
natural :: Parser Int
natural = lexeme nat

-- elimină spațiile de după stringul dat ca argument și întoarce stringul
symbol :: String -> Parser String
symbol str = lexeme (string str)

-- parsează stringul dat ca argument și elimină spațiile de după el,
-- întorcând unit
reserved :: String -> Parser ()
reserved str = lexeme (string str) *> pure ()

-- elimină spațiile de după paranteza rotundă și parsează un parser dat
parens :: Parser a -> Parser a
parens parser = lexeme (symbol "(") *> parser <* lexeme (symbol ")")

-- elimină spațiile de după paranteza pătrată și parsează un parser dat
brackets :: Parser a -> Parser a
brackets parser = lexeme (symbol "[") *> parser <* lexeme (symbol "]")

-- parsează una sau mai multe instanțe separate de virgulă,
-- cu eliminarea spațiilor de după fiecare virgulă,
-- întorcând lista obiectelor parsate
commaSep1 :: Parser a -> Parser [a]
commaSep1 parser = (:) <$> parser <*> many (lexeme (symbol ",") *> parser)

-- parsează zero sau mai multe instanțe separate de virgulă,
-- cu eliminarea spațiilor de după fiecare virgulă,
-- întorcând lista obiectelor parsate
commaSep :: Parser a -> Parser [a]
commaSep parser = commaSep1 parser <|> pure []

-- parsează un identificator (o literă urmată de zero sau mai multe litere/cifre)
-- întorcând un string
ident :: Parser Char -> Parser Char -> Parser String
ident identStart identLetter = (:) <$> identStart <*> many identLetter

-- parsează un identificator (o literă urmată de zero sau mai multe litere/cifre),
-- eliminând spațiile de după el și întorcând un string
identifier :: Parser Char -> Parser Char -> Parser String
identifier start letter = lexeme (ident start letter)

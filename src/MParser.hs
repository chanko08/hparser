module Main where
import Control.Monad
import Parser

-- consume a single character of the string that is being parsed.
eat :: Parser Char
eat = Parser eat'
  where
    eat' (x:xs) = Just (x,xs)
    eat' _ = Nothing



-- a helper function for removing the remaining string of
-- the parsed data
extract :: Maybe (a, String) -> Maybe a
extract = fmap fst

-- a helper function for running a parser on a string
runParser :: (Parser a) -> String -> (Maybe a)
runParser p s = extract $ (parse p) s


-- a parser that uses one parser, then the next
-- this function returns the value for the second parser
combine :: Parser a -> Parser b -> Parser b
combine (Parser p1) (Parser p2) = Parser combine'
  where
    combine' = join . fmap (p2 . snd) . p1


-- consume a single given character
char :: Char -> Parser Char
char c = Parser char'
  where
    char' (x:xs) | x == c = Just (c, xs)
                 | otherwise = Nothing
    char' _ = Nothing

-- consume a single character from the given set of characters
oneof (c:cs) = choice (char c) (oneof cs)
oneof _ = nullParser

-- consume a letter of the alphabet
letter = oneof "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

-- use a parser 0 or more times until it fails and return the
-- results of the parsings in a list
many :: Parser a -> Parser [a]
many p = Parser $ many' []
  where
    many' xs cs = case parse p cs of
                        Nothing -> Just (xs, cs)
                        Just (x, cs') -> many' (x:xs) cs'

-- similar to many, but instead require that there's at least
-- one successful parsing.
many1 :: Parser a -> Parser [a]
many1 p = Parser $ \cs -> listToMaybe . parse (many p) $ cs
  where
    listToMaybe Nothing = Nothing
    listToMaybe (Just ([], _)) = Nothing
    listToMaybe x = x


-- consume a single digits
digit = oneof "0123456789"

-- parse a positive integer
integer = many1 digit

-- parse one of the expected symbols we are using as operators
operator = oneof "+*/-"

-- consume as many spaces as possible
spaces = many $ char ' '

-- the lisp-like language parser entry point
expression = expression' `mplus` integer

-- parse an expression
expression' = do
    char '('

    spaces
    
    operator
    
    spaces
    
    expression
    
    spaces
    
    expression

    char ')'
    
    return ""

-- test run our parser!
testProg = "(+ 1 (* (+ 2 1) (- 5 4)))"
main = print $ runParser expression testProg
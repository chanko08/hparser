module Main where
import Control.Monad
import Data.Monoid
import Control.Applicative
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


-- similar to many, but instead require that there's at least
-- one successful parsing.
many1 :: Parser a -> Parser [a]
many1 p = Parser $ \cs -> listToMaybe . parse (many p) $ cs
  where
    listToMaybe Nothing = Nothing
    listToMaybe (Just ([], _)) = Nothing
    listToMaybe x = x


-- consume a single character from the given set of characters
oneof (c:cs) = choice (char c) (oneof cs)
oneof _ = nullParser


-- consume a letter of the alphabet
letter = oneof "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"


-- consume a single digits
digit = oneof "0123456789"

-- consume as many spaces as possible
spaces = many $ char ' '




-- the lisp-like language parser entry point
{-
- The data types created to make our parser return a result.
-}

data Expression = Expr Op Expression Expression | Number Int deriving Show
data Op = ADD | SUB | MUL | DIV deriving (Show, Eq)

{-
- The monadic version of the parser
-}
expression = expression' `mplus` integer


-- parse a positive integer
integer = do
    digits <- many1 digit
    return $ Number (read digits :: Int)

-- parse one of the expected symbols we are using as operators
operator = liftM toOp $ oneof "+*/-"
toOp '+' = ADD
toOp '-' = SUB
toOp '*' = MUL
toOp '/' = DIV

-- parse an expression
expression' = do
    char '('

    spaces
    
    op <- operator
    
    spaces
    
    ex1 <- expression
    
    spaces
    
    ex2 <- expression

    char ')'
    
    return $ Expr op ex1 ex2

{-
- Now the applicative version of the parser
-}
expression2 = integer2 `mplus` expression2'

integer2 = Number . read <$> many1 digit

operator2 = toOp <$> oneof "+*/-"

expression2' = Expr <$> (char '(' *> operator2 <* spaces)
                    <*> (expression <* spaces)
                    <*> expression

-- test run both the monadic and applicative parsers!
testProg = "(+ 1 (* (+ 2 1) (- 5 4)))"
main = do
  print $ runParser expression testProg
  print $ runParser expression2 testProg
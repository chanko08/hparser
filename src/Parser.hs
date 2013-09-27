module Parser (Parser(..), parse, choice, nullParser) where
import Control.Monad
import Control.Applicative
import Data.Monoid

{-
- The Parser type, expressed as a function that takes a string,
- and returns a tuple of a value and a string wrapped in the
- Maybe monad
-}
newtype Parser a = Parser ( String -> Maybe (a, String) )

{-
- The monad instance declaration for the Parser type.
- This lets us use functions made for monads on the Parser type.
-}
instance Monad Parser where
    return x = Parser $ \cs -> Just (x, cs)
    
    (>>=) p f = Parser $ join . fmap applyf . parse p
      where
        applyf (v, ys) = parse (f v) ys

{-
- The monoid instance declaration for the Parser type.
- This lets us use the functions made for monoids on the Parser type.
-}
instance MonadPlus Parser where
    mzero = nullParser
    mplus = choice

{-
- The functor instance declaration for the Parser type.
- This lets us use the functions made for functors on the Parser type.
-}
instance Functor Parser where
    fmap = liftM

{-
- The applicative instance declaration for the Parser type.
- This lets us use the functions made for applicatives on the
- Parser type.
-}
instance Applicative Parser where
    pure = return
    (<*>) = ap

{-
- The alternative instance declaration for the Parser type.
- This lets us use the functions made for alternatives on the
- Parser type.
-}
instance Alternative Parser where
    empty = mzero
    (<|>) = mplus


-- a helper function for unwrapping a Parser
parse :: Parser a -> (String -> Maybe (a, String))
parse (Parser p) = p

-- the fail parser
nullParser = Parser $ const Nothing

-- attempt to use one parser.
-- if that parser returns Nothing, then use the other parser
choice p1 p2 = Parser $ \cs -> case parse p1 cs of
                                    Nothing -> parse p2 cs
                                    x -> x
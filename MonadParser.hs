module MonadParser where

import Data.Char

type RealParser a = String -> [(a, String)]
newtype Parser a = P (RealParser a)

parse :: Parser a -> RealParser a
parse (P rp) inp = rp inp

instance Monad Parser where
  p >>= f = P (\inp -> case parse p inp of
                        [] -> []
                        [(v, out)] -> parse (f v) out)
  return v = P (\inp -> [(v, inp)])

failure :: Parser a
failure = P (\_ -> [])

item :: Parser Char
item = P (\inp -> case inp of
                   [] -> []
                   x:xs -> [(x, xs)])

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = P (\inp -> case parse p inp of
  [] -> parse q inp
  [(v, out)] -> [(v, out)])

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x
             else failure

alphanum :: Parser Char
alphanum = sat isAlphaNum

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do
  char x
  string xs
  return (x:xs)

many :: Parser a -> Parser [a]
many p = many1 p +++ return []
many1 :: Parser a -> Parser [a]
many1 p = do
  v <- p
  vs <- many p
  return (v:vs)

nat :: Parser Int
nat = do
  xs <- many1 digit
  return (read xs)

space :: Parser ()
space = do
  many (sat isSpace)
  return ()

token :: Parser a -> Parser a
token p = do
  space
  v <- p
  space
  return v

natural :: Parser Int
natural = token nat

symbol :: String -> Parser String
symbol = token . string

expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t + e)
            +++ return t

term :: Parser Int
term = do f <- factor
          do symbol "*"
             t <- term
             return (f * t)
            +++ return f

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
         +++ natural

exec :: Parser a -> String -> a
exec p xs | null left = v
          | otherwise = error $ "Remained string not parsed: " ++ left
  where (v, left) = head (parse p xs)
  
main :: IO ()
main = do
  putStrLn "Input expression: "
  xs <- getLine
  putStrLn $ "Result = " ++ show (exec expr xs)

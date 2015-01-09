module Main where

import System.IO
import Prelude hiding ((>>=), return)
import qualified Prelude as P
import Data.Char (ord, chr, isLower, isUpper, isDigit, isAlpha, isAlphaNum, isSpace)

qsort [] = []
qsort (x:xs) = qsort left ++ [x] ++ qsort right
  where left = [a | a <- xs, a <= x]
        right = [a | a <- xs, a > x]

last' xs = xs !! (length xs - 1)          

init' xs = take (length xs - 1) xs

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x <= y = x: merge xs (y:ys)
  | otherwise = y: merge (x:xs) ys
  
mergesort [] = []
mergesort [a] = [a]
mergesort xs = mergesort left `merge` mergesort right
               where (left, right) = splitAt (length xs `div` 2) xs

-- twice f x = f (f x)
-- plaindrome xs = reverse xs == xs
-- double x = x * 2
-- pair x y = (x,y)
-- swap (x,y) = (y,x)
-- second xs = head(tail xs)

headWithA :: [Char] -> Bool
headWithA ('a':_) = True
headWithA _ = False


const' x = \_ -> x

-- and' :: [Bool] -> Bool
-- and' = foldr (&&) True

halve :: [a] -> ([a], [a])
halve [] = ([], [])
halve [a] = ([], [a])
halve xs = splitAt (length xs `div` 2) xs

safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

safetail' xs
  | null xs = []
  | otherwise = tail xs

safetail'' [] = []
safetail'' (_:xs) = xs

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True

or'' x y = if x == True then True else
             if y == True then True else False

or''' x y
  | x == y = x
  | otherwise = True
                
or'''' False a = a
or'''' _ _ = True

mult' = \x y z -> x * y * z

-- Dec. 21
factor n = [a | a <- [1..n], n `mod` a == 0]
prime n = factor n == [1, n]
primes n = [a | a <- [2..n], prime a]

-- 8
table = [ 8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1 ]
min' :: Ord a => [a] -> a
min' [a] = a
min' (x:xs) = if x < y then x else y where y = min' xs

minpos :: Ord a => [a] -> Int
minpos xs = head [p | (x', p) <- zip xs [0..n], x' == x]
  where n = length xs - 1
        x = min' xs

shift :: Int -> Char -> Char
shift n c | isLower c = chr (ord 'a' + (ord c + n - ord 'a') `mod` 26)
          | isUpper c = chr (ord 'A' + (ord c + n - ord 'A') `mod` 26)
          | otherwise = c                        

encode :: Int -> String -> String
encode n xs = [shift n c | c <- xs]

lowers :: String -> Int
lowers xs = sum [1 | x <- xs, isLower x || isUpper x]

count :: Char -> String -> Int
count x xs = sum [1 | x' <- xs, x == x']

freqs :: String -> [Double]
freqs xs = [(\x y -> fromIntegral x / fromIntegral y) (count (chr (ord 'a' + x)) xs + count (chr (ord 'A' + x)) xs) (lowers xs) | x <- [0..25]]

chisqr xs ys = sum [((x - y) ^ 2) / y | (x, y) <- zip xs ys]
rotate n xs = xs'' ++ xs' where (xs', xs'') = splitAt n xs

crack xs = encode (-factor) xs
  where factor = minpos chitab
        chitab = [chisqr (rotate n table') table | n <- [0..25]]
        table' = freqs xs

-- Exercise for chapter 5

-- 1
intsqr n = sum [a ^ 2 | a <- [1..n]]

-- 2
rep n x = [x | _ <- [1..n]]

-- 3
isPyth :: Int -> Int -> Int -> Bool
isPyth x y z | x ^ 2 + y ^ 2 == z ^ 2 = True
             | otherwise = False

pyths n = [(x, y, z) | x <- r, y <- r, z <- r, isPyth x y z] where r = [1..n]

-- 4
factors n = [a | a <- [1..n], n `mod` a == 0]
perfects n = [a | a <- [1..n], sum (factors a) - a == a]

-- 5
dual xs ys = concat [zip ((\x n -> [x | _ <- [1..n]]) x (length ys)) ys | x <- xs]

-- 6
find' k t = [v | (k', v) <- t, k == k']
positions x xs = find' x (zip xs [0..n]) where n = length xs - 1

-- 7
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]

-- ***** Chapter 6
insert' :: Ord a => a -> [a] -> [a]
insert' a [] = [a]
insert' a (x:xs) | a <= x = a:x:xs
                 | otherwise = x:insert' a xs
isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert' x (isort xs)

-- Exercise
-- 1
power :: Int -> Int -> Int
power _ 0 = 1
power x n = x * (power x (n - 1)) -- n + k pattern is deprecated in Haskell 2010

-- 3
and' [] = True
and' (x:xs) = (&&) x (and' xs)

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ (concat' xs)

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x:(replicate' (n - 1) x)

at' :: [a] -> Int -> a
at' [x] 0 = x
at' (x:xs) 0 = x
at' (_:xs) n = at' xs (n - 1)

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' y (x:xs) | x == y = True
               | otherwise = elem' y xs

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + (sum' xs)

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x:xs) = x:take (n - 1) xs

last'' :: [a] -> a
last'' [x] = x
last'' (_:xs) = last'' xs

-- ***** Chapter 7
t' = \x y -> x (\z -> z y)
iterate' f x = x : iterate f (f x)
repeat' x = [x | _ <- [1..]]

type Bit = Int
bin2int :: [Bit] -> Int
bin2int = foldr (\x v -> x + v * 2) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin x = x `mod` 2 : int2bin (x `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat' 0)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

encode' :: String -> [Bit]
encode' = concat . map (make8 . int2bin . ord)

channel = id

decode' :: [Bit] -> String
decode' = map (chr . bin2int) . chop8

transmit' = decode' . channel . encode'

-- ********** Dec 22 **********
-- Exercise
-- 1. filter p (map f xs) == [f x | x <- xs, p x]
-- 2
all' p = foldr (\x v -> p x && v) True
any' p = foldr (\x v -> p x || v) False
takeWhile' p [] = []
takeWhile' p (x:xs) | p x = x : takeWhile' p xs
                    | otherwise = []

dropWhile' p [] = []
dropWhile' p (x:xs) | p x = dropWhile' p xs
                    | otherwise = x : xs

-- 3
map' f = foldr (\x v -> f x : v) []
filter' p = foldr (\x v -> if p x then x : v else v) []

-- 4
dec2int = foldl (\v x -> 10 * v + x) 0

-- 5
-- compose = foldr (.) id (associate to right)
-- sumsqreven' = sum . map (^2) . filter even (correct)
-- sumsqreven = compose [sum, map (^2), filter even] (wrong, because of right association)

-- 6
curry' f = \x y -> f (x, y)
uncurry' f = \(x, y) -> f x y

-- 7
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

int2bin' = unfold (== 0) (`mod` 2) (`div` 2)
chop8' = unfold (null) (take 8) (drop 8)
map'' f = unfold (null) (f . head) (tail)
iterate'' f = unfold (\_ -> False) (id) (f)

-- 8
countone = foldr (\x v -> if x == 1 then v + 1 else v) 0
make9 xs | even (countone xs) = xs' ++ [0]
         | otherwise = xs' ++ [1]
  where xs' = make8 xs
chop9 = unfold (null) (take 9) (drop 9)
check9 xs | (last xs == 0) == even (countone xs') = xs'
          | otherwise = error "Parity failed"
  where xs' = take 8 xs

encode9 = concat . map (make9 . int2bin . ord)
decode9 = map (chr . bin2int . check9) . chop9
transmit9 = decode9 . channel . encode9

-- 9
badchannel = tail
badtransmit = decode9 . badchannel . encode9

-- ***** Chapter 8
type Parser a = String -> [(a, String)]

return :: a -> Parser a
return v = \inp -> [(v, inp)]

failure :: Parser a
failure = \inp -> []

item :: Parser Char
item = \inp -> case inp of
  [] -> []
  (x:xs) -> [(x, xs)]

parse :: Parser a -> String -> [(a, String)]
parse p inp = p inp

(>>=) :: Parser a -> (a -> Parser b) -> Parser b
p >>= f = \inp -> case parse p inp of
  [] -> []
  [(v, out)] -> parse (f v) out

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = \inp -> case parse p inp of
  [] -> parse q inp
  [(v, out)] -> [(v, out)]

sat :: (Char -> Bool) -> Parser Char
sat p = item >>= \x ->
  if p x then return x else failure

alphanum = sat isAlphaNum
digit = sat isDigit
lower = sat isLower
char x = sat (== x)

string [] = return []
string (x:xs) = char x >>= \_ ->
  string xs >>= \_ ->
  return (x:xs)

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = p >>= \v ->
  many p >>= \vs ->
  return (v:vs)

-- *** identifiers
ident = lower >>= \x ->
  many alphanum >>= \xs ->
  return (x:xs)

nat :: Parser Int
nat = many1 digit >>= \xs ->
  return (read xs)

space :: Parser ()
space = many (sat isSpace) >>= \_ ->
  return ()

token :: Parser a -> Parser a
token p = space >>= \_ ->
  p >>= \v ->
  space >>= \_ ->
  return v

identifier = token ident
natural = token nat
symbol = token . string

p :: Parser [Int]
p = symbol "[" >>= \_ ->
  natural >>= \n ->
  many (symbol "," >>= \_ -> natural) >>= \ns ->
  symbol "]" >>= \_ ->
  return (n:ns)

expr :: Parser Int
expr = term >>= \t ->
  (symbol "+" >>= \_ ->
    expr >>= \e ->
    return (t + e)) +++
  (symbol "-" >>= \_ ->
    expr >>= \e ->
    return (t - e)) +++
  return t

term :: Parser Int
term = facto >>= \f ->
  (symbol "*" >>= \_ ->
    term >>= \t ->
    return (f * t)) +++
  (symbol "/" >>= \_ ->
    term >>= \t ->
    return (f `div` t)) +++
  return f

facto :: Parser Int
facto = pow >>= \p ->
  (symbol "^" >>= \_ ->
    facto >>= \f ->
    return (p ^ f)) +++
  return p

pow :: Parser Int
pow = (symbol "(" >>= \_ ->
  expr >>= \e ->
  symbol ")" >>= \_ ->
  return e) +++ natural

evalf :: (Parser a) -> String -> a
evalf f xs = case parse f xs of
  [(n, [])] -> n
  [(_, out)] -> error ("unused input " ++ out)
  [] -> error "invalid input"

eval = evalf expr

-- Exercise
-- 1
int :: Parser Int
int = (symbol "-" >>= \_ ->
        natural >>= \n ->
        return (-n)) +++
      natural

-- 2
comment :: Parser String
comment = symbol "--" >>= \_ ->
  many (sat (\x -> x /= '\n')) >>= \s ->
  string "\n" >>= \_ ->
  return s

-- 8
expr' :: Parser Int
expr' = natural >>= \n ->
  (many (symbol "-" >>= \_ ->
          natural >>= \e ->
          return e) >>= \ns ->
    return (foldl (-) n ns)) +++
  return n

eval' = evalf expr'

-- ***** Chapter 9
getCh  = do hSetEcho stdin False
            c <- getChar
            hSetEcho stdin True
            P.return c
            
getDoubleChar = do
  x <- getCh
  putChar x
  P.return x

getLine' = do
  x <- getCh
  if x == '\n' then P.return [] else
                    do xs <- getLine
                       P.return (x:xs)

strlen = do putStr "Enter a string: "
            xs <- getLine'
            putStr "The string has "
            putStr . show . length $ xs
            putStrLn " characters."

beep = putStr "\BEL"
cls = putStr "\ESC[2J"
type Pos = (Int, Int)
goto (x, y) = putStr $ "\ESC[" ++ show y ++ ";" ++ show x ++ "H"

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

seqn :: [IO a] -> IO ()
seqn [] = P.return ()
seqn (a:as) = do a
                 seqn as
box = ["+---------------+",
       "|               |",
       "+---+---+---+---+",
       "| q | c | d | = |",
       "+---+---+---+---+",
       "| 1 | 2 | 3 | + |",
       "+---+---+---+---+",
       "| 4 | 5 | 6 | - |",
       "+---+---+---+---+",
       "| 7 | 8 | 9 | * |",
       "+---+---+---+---+",
       "| 0 | ( | ) | / |",
       "+---+---+---+---+"]

buttons = standard ++ extra where
  standard = "qcd=123+456-789*0()/"
  extra = "QCD \ESC\BS\DEL\n"

showbox = seqn [writeat (1, y) xs | (y, xs) <- zip [1..13] box]
display xs = do writeat (3, 2) "             "
                writeat (3, 2) (reverse (take 13 (reverse xs)))

calc xs = do display xs
             c <- getCh
             if c `elem` buttons then
               process c xs
               else do beep
                       calc xs

process c xs | c `elem` "qQ\ESC" = quit
             | c `elem` "dD\BS\DEL" = delete xs
             | c `elem` "=\n" = eval'' xs
             | c `elem` "cC" = clear
             | otherwise = press c xs

quit = goto (1, 14)
delete "" = calc ""
delete xs = calc . init $ xs

eval'' xs = case parse expr xs of
  [(n, "")] -> calc . show $ n
  _ -> do beep
          calc xs

clear = calc ""

press c xs = calc $ xs ++ [c]

run = do cls
         showbox
         clear

-- Exercise
-- 1
readLine = do xs <- readLnReal ""
              P.return xs
readLnReal xs = do c <- getChar
                   process' c xs
                   where process' c xs | c == '\n' = P.return xs
                                       | c `elem` "\BS\DEL" = do
                                           putStr "\ESC[1D"
                                           xs' <- readLnReal (if null xs then xs else init xs)
                                           P.return xs'
                                       | otherwise = do xs' <- readLnReal (xs ++ [c])
                                                        P.return xs'

-- main = do xs <- readLine
--           putStrLn $ "Input string is: " ++ xs

-- ***** Chapter 10
data Shape = Circle Float | Rect Float Float
square n = Rect n n
area (Circle r) = pi * r ^ 2
area (Rect x y) = x * y

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just . head $ xs

data Nat = Zero | Succ Nat
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n
int2nat 0 = Zero
int2nat n = Succ . int2nat $ n - 1

natadd Zero n = n
natadd (Succ m) n = Succ $ natadd m n

data List a = Nil | Cons a (List a)
listLen Nil = 0
listLen (Cons _ xs) = 1 + listLen xs

data Tree = Leaf Int | Node Tree Int Tree
occurs :: Int -> Tree -> Bool
occurs m (Leaf n) = m == n
occurs m (Node tl n tr) = m == n || occurs m tl || occurs m tr

flatten :: Tree -> [Int]
flatten (Leaf n) = [n]
flatten (Node l n r) = flatten l ++ [n] ++ flatten r

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop

type Assoc k v = [(k, v)]
type Subst = Assoc Char Bool

propEval :: Subst -> Prop -> Bool
propEval _ (Const b) = b
propEval s (Var x) = head $ find' x s
propEval s (Not p) = not $ propEval s p
propEval s (And p q) = propEval s p && propEval s q
propEval s (Imply p q) = propEval s p <= propEval s q

propVars (Const b) = []
propVars (Var x) = [x]
propVars (Not p) = propVars p
propVars (And p q) = propVars p ++ propVars q
propVars (Imply p q) = propVars p ++ propVars q

-- bools n = map (map conv . make n . int2bin) [0..limit]
--           where limit = (2 ^ n) - 1
--                 make n bs = take n $ bs ++ repeat 0
--                 conv 0 = False
--                 conv 1 = True

bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
  where bss = bools (n - 1)

subtr n xs = filter (\e -> not $ e `elem` xs) [1..n]
permu :: Int -> Int -> [[Int]]
permu _ 0 = [[]]
permu n m = [x ++ [i] | x <- xs, i <- subtr n x] where xs = permu n (m - 1)
permall n = permu n n

subtrl n xs = if null xs then [1..n] else [1..minimum xs - 1]
comb :: Int -> Int -> [[Int]]
comb _ 0 = [[]]
comb n m = [i:x | x <- xs, i <- subtrl n x] where xs = comb n (m - 1)

rmdups [] = []
rmdups (x:xs) = x:rmdups (filter (/= x) xs)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools . length $ vs) where vs = rmdups . propVars $ p

p2 = (Var 'A' `And` Var 'B') `Imply` Var 'B'

isTaut p = and [propEval s p | s <- substs p]

data Expr = Val Int | Add Expr Expr
type Cont = [Op]
data Op = EVAL Expr | ADD Int

evalExpr :: Expr -> Cont -> Int
evalExpr (Val n) c = exec c n
evalExpr (Add x y) c = evalExpr x (EVAL y : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVAL y : c) n = evalExpr y (ADD n : c)
exec (ADD n : c) m = exec c (n + m)

value e = evalExpr e []



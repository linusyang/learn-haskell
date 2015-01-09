module Main where

import MonadParser (lower, Parser, symbol, (+++), token, exec)

-- Chapter 10 exercises

-- 1
data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = nat2int n + 1

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult (Succ m) n = add n (mult m n)

-- 2
data STree = SLeaf Int | SNode STree Int STree
occurs :: Int -> STree -> Bool
occurs m (SLeaf n) = m == n
occurs m (SNode l n r)
  | c == EQ = True
  | c == LT = occurs m l
  | otherwise = occurs m r
  where c = compare m n

-- 3
data Tree = Leaf Int | Node Tree Tree

leaves :: Tree -> Int
leaves (Leaf _) = 1
leaves (Node l r) = leaves l + leaves r

balanced :: Tree -> Bool
balanced (Leaf _) = True
balanced (Node l r) = abs (leaves l - leaves r) <= 1 &&
                        balanced l && balanced r

-- 4
balance :: [Int] -> Tree
balance [n] = Leaf n
balance xs = Node (balance l) (balance r)
             where (l, r) = splitAt (length xs `div` 2) xs

-- 5
data Prop = Const Bool
            | Var Char
            | Not Prop
            | And Prop Prop
            | Imply Prop Prop
            | Or Prop Prop
            | Eqv Prop Prop

type Assoc k v = [(k, v)]
type Subst = Assoc Char Bool

find :: Eq k => k -> Assoc k v -> v
find k' (x:xs) | k == k' = v
               | otherwise = find k' xs
  where (k, v) = x

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = find x s
eval s (Not p) = not (eval s p)
eval s (And p q) = (eval s p) && (eval s q)
eval s (Imply p q) = (eval s p) <= (eval s q)
eval s (Or p q) = (eval s p) || (eval s q)
eval s (Eqv p q) = eval s (And (Imply p q) (Imply q p))

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q
vars (Eqv p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) xs ++ map (True:) xs
  where xs = bools (n - 1)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
  where vs = rmdups . vars $ p

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

-- 6
var :: Parser Prop
var = do x <- lower
         return (Var x)

ident :: Parser Prop
ident = token var +++ bool

bool :: Parser Prop
bool = do symbol "True"
          return (Const True)
        +++
       do symbol "False"
          return (Const False)

prop :: Parser Prop
prop = do t <- term
          do symbol "||"
             p <- prop
             return (Or t p)
            +++ do symbol "->"
                   p <- prop
                   return (Imply t p)
            +++ do symbol "<->"
                   p <- prop
                   return (Eqv t p)
            +++ return t

term :: Parser Prop
term = do f <- factor
          do symbol "&&"
             t <- term
             return (And f t)
            +++ return f

factor :: Parser Prop
factor = do symbol "~"
            p <- prop
            return (Not p)
         +++ do symbol "("
                p <- prop
                symbol ")"
                return p
         +++ ident

checkTaut :: String -> Bool
checkTaut = isTaut . exec prop

polish :: Parser Prop
polish = do
  symbol "P"
  symbol "("
  c <- cond
  symbol ")"
  return c

cond :: Parser Prop
cond = do
  symbol "C"
  symbol "("
  l <- cond
  symbol ","
  r <- cond
  symbol ")"
  return (Imply l r)
  +++ ident

instance Show Prop where
  show (Imply l r) = "(" ++ show l ++ " -> " ++ show r ++ ")"
  show (Var x) = [x]

unpol :: String -> String
unpol = show . exec polish

-- 7
data Expr = Val Int | Add Expr Expr | Mult Expr Expr
type Cont = [Op]
data Op = EVALADD Expr| EVALMULT Expr | ADD Int | MULT Int

evalM :: Expr -> Cont -> Int
evalM (Val n) c = execM c n
evalM (Add x y) c = evalM x (EVALADD y : c)
evalM (Mult x y) c = evalM x (EVALMULT y : c)

execM :: Cont -> Int -> Int
execM [] n = n
execM (EVALADD y : c) n = evalM y (ADD n : c)
execM (EVALMULT y : c) n = evalM y (MULT n : c)
execM (ADD n : c) m = execM c (n + m)
execM (MULT n : c) m = execM c (n * m)

-- 8
data Maybe' a = Nothing' | Just' a deriving (Show, Read, Eq, Ord)
instance Monad Maybe' where
  return x = Just' x
  Nothing' >>= _ = Nothing'
  (Just' x) >>= f = f x

-- Dummy main function

main :: IO ()
main = do
  putStrLn "Dummy main function"

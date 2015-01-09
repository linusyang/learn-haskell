module ChapterEleven where

data Op = Add | Sub | Mul | Div deriving (Eq)
data Expr = Val Int | App Op Expr Expr

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

instance Show Expr where
  show (Val n) = show n
  show (App o l r) | o `elem` [Add, Sub] = "(" ++ show l ++ show o ++ show r ++ ")"
                   | otherwise = show l ++ show o ++ show r

valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 1 && x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y]

-- Set operations
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
-- choices xs = concat (map perms (subs xs))
choices xs = [y | x <- subs xs, y <- perms x]

isChoice :: Eq a => [a] -> [a] -> Bool
-- isChoice xs ys = and [x `elem` ys | x <- xs]
isChoice [] _ = True
isChoice (x:xs) ys | x `elem` ys = isChoice xs ys
                   | otherwise = False

solution :: Expr -> [Int] -> [Int] -> Bool
solution e ns n = isChoice (values e) ns && eval e == n

-- Find solutions
split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x:xs) = ([x], xs):[(x:ls, rs) | (ls, rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split ns,
            l <- exprs ls,
            r <- exprs rs,
            e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns,
                  e <- exprs ns',
                  eval e == [n]]

-- Better method
type Result = (Expr, Int)
results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns = [res | (ls, rs) <- split ns,
              l <- results ls,
              r <- results rs,
              res <- combine' l r]

combine' :: Result -> Result -> [Result]
combine' (l, x) (r, y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns,
                   (e, m) <- results ns',
                   m == n]

main :: IO ()
main = do
  putStrLn . show . length $ solutions' [1,3,7,10,25,50] 765

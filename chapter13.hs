module ChapterThirteen where

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

erev :: [a] -> [a]
erev = foldl (flip (:)) []

data Tree = Leaf Int | Node Tree Tree
flatten :: Tree -> [Int]
flatten (Leaf n) = [n]
flatten (Node l r) = flatten l ++ flatten r

flatten' :: Tree -> [Int] -> [Int]
flatten' (Leaf n) xs = n:xs
flatten' (Node l r) xs = flatten' l (flatten' r xs)

data Expr = Val Int | Add Expr Expr
type Stack = [Int]
type Code = [Op]
data Op = PUSH Int | ADD deriving (Read, Show)

exec :: Code -> Stack -> Stack
exec [] s = s
exec (PUSH n : c) s = exec c (n:s)
exec (ADD : c) (m : n : s) = exec c (m + n : s)

comp :: Expr -> Code
comp (Val n) = [PUSH n]
comp (Add x y) = comp x ++ comp y ++ [ADD]

comp' :: Expr -> Code -> Code
comp' (Val n) c = PUSH n : c
comp' (Add x y) c = comp' x (comp' y (ADD : c))

-- Dummy main function 

main :: IO ()
main = do
  putStrLn "Dummy main function"

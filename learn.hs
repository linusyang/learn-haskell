module Main where

import Control.Monad (when, forever, forM)

splitWhile :: (a -> Bool) -> [a] -> ([a], [a])
splitWhile _ [] = ([], [])
splitWhile p a@(x:xs) | p x = (x:left, right)
                      | otherwise = ([], a)
  where (left, right) = splitWhile p xs

skipSpace ys = xs where (_, xs) = splitWhile (== ' ') ys
splitWord w = (xs, ys) where (xs, ys') = splitWhile (/= ' ') w
                             ys = skipSpace ys'

words' :: String -> [String]
words' [] = [[]]
words' xs = now : words remain
  where (now, remain) = splitWord xs

group :: Eq a => [a] -> [[a]]
group [] = []
group a@(x:_) = now : group remain
  where (now, remain) = splitWhile (== x) a

merge :: Ord a => (a -> a -> Bool) -> [a] -> [a] -> [a]
merge _ xs [] = xs
merge _ [] ys = ys
merge p (x:xs) (y:ys) | p x y = x:merge p xs (y:ys)
                      | otherwise = y:merge p (x:xs) ys

sort :: Ord a => (a -> a -> Bool) -> [a] -> [a]
sort _ [] = []
sort _ [a] = [a]
sort p xs = merge p (sort p left) (sort p right)
  where (left, right) = splitAt (length xs `div` 2) xs

type Assoc k v = [(k, v)]

wordnum :: String -> Assoc Int String
wordnum = sort (>) . map (\xs -> (length xs, head xs)) . group . sort (<=) . words'


foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' _ v [] = v
foldl' f v (x:xs) = (foldl' f $! (f v x)) xs

binsearch :: Ord a => a -> Int -> [a] -> Maybe Int
binsearch _ _ [] = Nothing
binsearch t base xs | mid == t = Just $ base + pos'
                    | mid < t = binsearch t pos right
                    | otherwise = binsearch t base left
  where pos' = length xs `div` 2
        (left, right') = splitAt pos' xs
        mid = head right'
        right = tail right'
        pos = pos' + 1

data Tree a = Nil | Node a (Tree a) (Tree a)

instance Functor Tree where
  fmap _ Nil = Nil
  fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

treeInsert :: Ord a => a -> Tree a -> Tree a
treeInsert x Nil = Node x Nil Nil
treeInsert x (Node n l r) | x == n = Node x l r
                          | x < n = Node n (treeInsert x l) r
                          | otherwise = Node n l (treeInsert x r)

listTree :: Ord a => [a] -> Tree a
listTree = foldr treeInsert Nil

treeList' :: Tree a -> [a] -> [a]
treeList' Nil xs = xs
treeList' (Node n l r) xs = treeList' l (n : treeList' r xs)

treeList :: Tree a -> [a]
treeList = flip treeList' $ []

getNumbers :: IO [Int]
getNumbers = do
  input <- getLine
  let xs = map (\x -> read x :: Int) $ words input
  return xs

main :: IO ()
main = do
  putStr "Input some numbers: "
  xs <- getNumbers
  putStr "Sorted numbers: "
  putStrLn . show . treeList . listTree $ xs
  mapM_ print ([1,2,3] :: [Int])


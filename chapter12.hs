module ChapterTwelve where

primes :: [Int]
primes = sieve [2..]
               
sieve :: [Int] -> [Int]
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

sumWith :: Int -> [Int] -> Int
sumWith v [] = v
sumWith v (x:xs) = (sumWith $! (v + x)) xs

-- Exercises
fibs :: [Integer]
fibs = 0:1:list
           where list = map (\(x, y) -> x + y) (zip fibs (tail fibs))

fib :: Int -> Integer
fib n = fibs !! n

findWhile :: (a -> Bool) -> [a] -> a
findWhile p (x:xs) | p x = findWhile p xs
                   | otherwise = x

fibThousand :: Integer             
fibThousand = findWhile (<1000) fibs

sqrts :: Double -> [Double]
sqrts n = (n / 2) : map (\x -> 0.5 * (x + n / x)) (sqrts n)

sqrtEps :: Double -> Double -> Double
sqrtEps eps n = findWhile (\x -> (abs $ x * x - n) > eps) (sqrts n)

sqrt' :: Double -> Double
sqrt' = sqrtEps 1e-6

perm :: Int -> Int -> [[Int]]
perm _ 0 = [[]]
perm n m = [x:xs | xs <- perm n (m - 1), x <- subtr n xs]
  where subtr t xs = reverse $ filter (\x -> not (x `elem` xs)) [1..t]

-- f [] v = v
-- f (g:gs) v = f gs (g v)


-- Dummy main function

main :: IO ()
main = putStrLn "Dummy"

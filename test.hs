pyth :: Floating a => a -> a -> a
pyth a b = sqrt (a * a + b * b)

fib :: Num a => Int -> a
fib n 
    | n == 0    = 0
    | n == 1    = 1 
    | otherwise = fib (n-1) + fib (n-2)

fact :: Integral a => a -> a
fact n = if n == 1 then 1 else ( (fact (n - 1)) * n )

pow :: Integral a => a -> a -> a
pow b p = if p == 1 then b else ( (pow b (p-1)) * b )

deleteN :: Int -> [a] -> [a]
deleteN _ []     = []
deleteN i (a:as)
   | i == 0    = as
   | otherwise = a : deleteN (i-1) as

removeMultiples :: Integer -> [Integer] -> [Integer]
removeMultiples _ [] = []
removeMultiples number arr = [x| x <- arr, x `mod` number /= 0]

es :: Integral a => [a] -> [a]
es [] = []
es (x:xs)
    | xs == []  = [x]
    | otherwise = x : es [ n | n <- xs, n `mod` x /= 0]

primesUpTo :: Integral a => a -> [a]
primesUpTo n
    | n < 2     = []
    | otherwise = es [2..n]

nsd :: Integer -> Integer -> Integer
nsd n m
    | m == 0    = n
    | n >= m    = nsd m (n `mod` m)
    | otherwise = nsd m n

-- 4. úloha
--
-- 1) Implementujte kódování a dekódování RLE (https://en.wikipedia.org/wiki/Run-length_encoding):

-- >>> rleEncode "hello"
-- [(1,'h'),(1,'e'),(2,'l'),(1,'o')]
--
rleEncode :: (Eq a) => [a] -> [(Int, a)]
rleEncode []        = []
rleEncode (x:xs)    = rleEncode_ x 1 xs
rleEncode_ :: (Eq a) => a -> Int -> [a] -> [(Int, a)]
rleEncode_ character number [] = [(number, character)]
rleEncode_ character number (x:xs)
	| x == character    = rleEncode_ character (number + 1) xs
	| otherwise         = (number, character): rleEncode_ x 1 xs

-- >>> rleDecode [(1,'h'),(1,'e'),(2,'l'),(1,'o')]
-- "hello"
--
rleDecode :: [(Int, a)] -> [a]
rleDecode [] = []
rleDecode ((number, char):xs) = rleDecode_ number char xs
rleDecode_ :: Int -> a -> [(Int, a)] -> [a]
rleDecode_ number character []
	| number == 1   = [character]
	| otherwise     = character: rleDecode_ (number - 1) character []
rleDecode_ number character ((num, nextChar):xs)
	| number == 1   = character: rleDecode_ num nextChar xs
	| otherwise     = character: rleDecode_ (number - 1) character ((num, nextChar):xs)

-- 2) Definujte nekonečný seznam všech prvočísel. Pokuste se o efektivní řešení.

-- >>> take 5 primes
-- [2,3,5,7,11]

-- Eratostenovo sito
primes :: [Integer]
primes = primes_ [2..]
	where
		primes_ (x:xs) = x: primes_ [ y | y <- xs, y `mod` x /= 0]

-- 3) Implementujte mergesort.

mergeWith :: (a -> a -> Bool) -> [a] -> [a] -> [a]
mergeWith _ x [] = x
mergeWith _ [] y = y
mergeWith comp (x:xs) (y:ys)
	| comp x y  = x: mergeWith comp xs (y:ys)
	| otherwise = y: mergeWith comp (x:xs) ys

sortWith  :: (a -> a -> Bool) -> [a] -> [a]
sortWith _ [] = []
sortWith _ (x:[]) = [x]
sortWith comp (x:xs) = 
	mergeWith comp firstSorted secondSorted
	where 
		halfLength = (length (x:xs)) `div` 2
		firstSorted = sortWith comp (take halfLength (x:xs))
		secondSorted = sortWith comp (drop halfLength (x:xs))

-- Prvním argumentem je funkce, která provádí porovnávání.
--
-- >>> sortWith (<) [10,9..1]
-- [1,2,3,4,5,6,7,8,9,10]
--
-- >>> sortWith (>) [10,9..1]
-- [10,9,8,7,6,5,4,3,2,1]
--
-- BONUS)
--
-- Implementujte následující funkce:

-- combinations n x vygeneruje seznam všech kombinací délky n ze seznamu x.
-- Na pořadí kombinací ve výsledném seznamu nezáleží.
--
-- >>> combinations 2 "abcd"
-- ["ab","ac","ad","bc","bd","cd"]
--
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = map (x :) (combinations (n - 1) xs) ++ combinations n xs

-- permutations x vygeneruje seznam všech permutací. Na pořadí permutací ve
-- výsledném seznamu nezáleží.
--
-- >>> permutations "abc"
-- ["abc","bac","bca","acb","cab","cba"]
--
permutations :: [a] -> [[a]]
permutations = undefined

-- Pomocí těchto funkcí definujte "variace" (občas najdete v české literatuře,
-- v angličtině pro to termín asi neexistuje): kombinace, kde záleží na pořadí
--
-- >>> variations 2 "abc"
-- ["ab","ba","ac","ca","bc","cb"]
--
variations :: Int -> [a] -> [[a]]
variations = undefined
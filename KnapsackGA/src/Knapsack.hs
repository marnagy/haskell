-- | Implementation of needed functions for GA.
module Knapsack 
    (newChromosome, loadDatabaseFrom, defaultCrossover,
    defaultChooseParent, getRandInts, mutate,
    getRandNum, getRandDouble, sortWith,
    Chromosome (Chromosome)) where

import System.Random

-- | Type used to store a solution.
--
-- Stores weight, value and list of items (True -> chosen, False -> NOT chosen)
data Chromosome = Chromosome Int Int [Bool]

instance Show Chromosome where
    show (Chromosome weight value values) =
        "Chromosome weight:" ++ show weight ++
            " value:" ++ show value ++ " choosing " ++ show values

-- | Generate ONE new random solution.
--
-- This adds random items until weight restriction is met.
newChromosome :: [(Int, Int)] -- ^ Database of (weight, value).
    -> Int -- ^ Maximum amount of weight allowed.
    -> IO Chromosome -- ^ New random chromosome.
newChromosome [] _ = error "Empty database"
newChromosome database weightRestriction = do
    let itemsNum = length database
    let chromosome = generateEmptyChromosome itemsNum
    takeRand itemsNum chromosome
    where
        takeRand :: Int -> Chromosome -> IO Chromosome
        takeRand _ (Chromosome _ _ []) = error "Invalid chromosome"
        takeRand iterVar chrom@(Chromosome weight value vals)
            | iterVar > 0   = do
                    randInt <- getRandNum (0, length database - 1)
                    let (iWeight, iValue) = database !! randInt
                    if vals !! randInt == False && weight + iWeight <= weightRestriction   then do
                            let newChrom = Chromosome (weight + iWeight) (value + iValue) (setItemInList vals randInt True)
                            takeRand (iterVar - 1) newChrom
                    else takeRand (iterVar - 1) chrom
            | iterVar == 0  = pure chrom
            | otherwise     = error "Invalid number to iterate."
        
        generateEmptyChromosome :: Int -> Chromosome
        generateEmptyChromosome len = Chromosome 0 0 (generateFalseList len)

        generateFalseList :: Int -> [Bool]
        generateFalseList n
            | n == 0    = []
            | n > 0     = False: generateFalseList (n-1)
            | otherwise = error "Cannot generate list of negative length."

-- | Set item on specific index in list.
setItemInList :: [a] -- ^ Original list.
    -> Int -- ^ Index to replace(starts with 0).
    -> a -- ^ Item to insert to list.
    -> [a] -- ^ List with replaced item.
setItemInList [] _ _ = []
setItemInList (x:xs) n v
    | n > 0         = x:setItemInList xs (n-1) v
    | n == 0        = v:xs
    |otherwise      = error "Invalid index on list."

-- | Get random int in range [lo, hi].
--
-- Does NOT check if lo <= hi.
getRandNum :: (Int, Int) -- ^ Interval [low, hi]
    -> IO Int -- ^ Random int in interval
getRandNum (lo, hi) = getStdRandom (randomR (lo, hi))

-- | Get random double in range [lo, hi].
getRandDouble :: (Double, Double) -- ^ Interval [low, hi]
    -> IO Double -- ^ Random Double in interval
getRandDouble (lo, hi) = getStdRandom (randomR (lo, hi))

-- | Load database from weight and value text files.
--
-- Expected format:
-- each file contains same amount of lines in both files
-- and each line contains one positive integer.
-- Function DOES NOT check the value of integers!
loadDatabaseFrom :: String -- ^ Relative path to file containing weights.
    -> String -- ^ Relative path to file containing values.
    -> IO [(Int, Int)] -- ^ Database of (weight, value) for each item.
loadDatabaseFrom weightsFileName valuesFileName = do
    allLinesW <- readFile weightsFileName
    allLinesV <- readFile valuesFileName
    let wList = map read $ lines allLinesW :: [Int]
    let vList = map read $ lines allLinesV :: [Int]
    merge wList vList
    where
        merge :: [Int] -> [Int] -> IO [(Int, Int)]
        merge (x:xs) (y:ys) = do
            rs <- merge xs ys
            pure ((x,y) : rs)
        merge [] [] = pure []
        merge _ _ = error "Weights and values files have different amounts of values"

-- | Default implementation of crossover between 2 chromosomes.
--
-- Takes half (random set of indices) of items as chosen in first parent,
-- rest from second parent. Does NOT check weight restriction.
defaultCrossover :: [(Int, Int)] -- ^ Database of (weight, value) for each item.
    -> (Chromosome, Chromosome) -- ^ Parents.
    -> IO Chromosome -- ^ New chromosome created from parents.
defaultCrossover database ((Chromosome _ _ vals1), (Chromosome _ _ vals2)) = do
    let len = length vals1
    let halfLen = len `div` 2
    randInts <- getRandInts halfLen len
    let randIntsOrd = sortWith (<) randInts
    pure $ getVals 0 len randIntsOrd
    where
        getVals :: Int -> Int -> [Int] -> Chromosome
        getVals currIndex maxIndex randInts
            | currIndex == maxIndex = Chromosome 0 0 []
            | otherwise             = do
                let Chromosome resW resV vals = getVals (currIndex + 1) maxIndex randInts
                let (weight, value) = database !! currIndex
                if randInts `contains` currIndex then ( 
                    if vals1 !! currIndex then Chromosome (resW + weight) (resV + value) (True:vals)
                    else Chromosome resW resV (False:vals) )
                else (
                    if vals2 !! currIndex then Chromosome (resW + weight) (resV + value) (True:vals)
                    else Chromosome resW resV (False:vals) )

-- | Get N random non-repeating unsorted Ints in range [0,maxIndex - 1]
--
-- If amount > maxIndex or amount < 0, throws error.
--
-- >>>getRandInts 5 10
-- [3,7,1,5,4]
getRandInts :: Int -- ^ Amount of random Ints to be generated.
    -> Int -- ^ Max index.
    -> IO [Int] -- ^ Unsorted list of non-repeating Ints
getRandInts amount maxIndex
    | amount > maxIndex || amount < 0   = error "Amount needs to be in range [0,maxIndex)."
    | otherwise                         = getRandInts' 0 []
    where
        getRandInts' :: Int -> [Int] -> IO [Int]
        getRandInts' currAmount akum
            | currAmount < amount   = do
                randInt <- getRandNum (0, maxIndex - 1)
                if akum `contains` randInt then getRandInts' currAmount akum
                else getRandInts' (currAmount + 1) (randInt:akum)
            | otherwise             = pure akum

-- | Simple linear test if list contains value
contains :: Eq a => [a] -- ^ List to search in.
    -> a -- ^ Item to find.
    -> Bool -- ^ Result.
contains [] _ = False
contains (x:xs) arg
    | x == arg      = True
    | otherwise     = contains xs arg

-- | Default implementation of choosing parent from last generation.
--
-- Choosing random chromosome with higher value than random fraction of best solution.
-- This algorithm prefers better solutions but can also choose worse ones.
defaultChooseParent :: [Chromosome] -- ^ Generation of Chromosome to choose from.
    -> IO Chromosome -- ^ Chosen parent.
defaultChooseParent lastGen = do
    randDouble <- getStdRandom (randomR (0 :: Double, 1 :: Double))
    let Chromosome _ value _ = lastGen !! 0
    let refValue = randDouble * fromIntegral value
    randInt <- getRandNum (0, length lastGen - 1)
    defaultChooseParent' (>= refValue) (lastGen !! randInt)
    where
        defaultChooseParent' :: (Double -> Bool) -> Chromosome -> IO Chromosome
        defaultChooseParent' test chrom@(Chromosome _ cValue _)
            | test $ fromIntegral cValue    = pure chrom
            | otherwise                     = defaultChooseParent lastGen

-- | Implementation of mutation of a given chromosome.
--
-- Changes decision for ONE random item in the given chromosome.
mutate :: [(Int, Int)] -- ^ Database of (weight, value) for each item.
    -> Chromosome -- ^ Chromosome to mutate.
    -> IO Chromosome -- ^ Mutated chromosome.
mutate database (Chromosome weight value vals) = do
    randInt <- getRandNum (0, length vals - 1)
    let (weight1, value1) = database !! randInt
    if vals !! randInt then pure (Chromosome (weight - weight1) (value - value1) $ setItemInList vals randInt False)
    else pure (Chromosome (weight + weight1) (value + value1) $ setItemInList vals randInt True)

-- | Sorts a list using merge sort algorithm.
sortWith  :: (a -> a -> Bool) -- ^ Comparing function.
    -> [a] -- ^ List to sort.
    -> [a] -- ^ Sorted list.
sortWith _ [] = []
sortWith _ (x:[]) = [x]
sortWith comp (x:xs) = do
    let (first, second) = toHalfs (x:xs)
    let firstSorted = sortWith comp first
    let secondSorted = sortWith comp second
    mergeWith comp firstSorted secondSorted
    where
        toHalfs :: [a] -> ([a], [a])
        toHalfs [] = ([],[])
        toHalfs (y:ys) = do
            let (a,b) = toHalfs ys
            (b, y:a)
        
        mergeWith :: (a -> a -> Bool) -> [a] -> [a] -> [a]
        mergeWith _ x [] = x
        mergeWith _ [] y = y
        mergeWith comp (x:xs) (y:ys)
            | comp x y  = x: mergeWith comp xs (y:ys)
            | otherwise = y: mergeWith comp (x:xs) ys

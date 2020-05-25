module Knapsack where

import System.Random
import System.IO

data Chromosome = Chromosome Int Int [Bool]
        deriving (Show)

newChromosome :: [(Int, Int)] -> Int -> IO Chromosome
newChromosome [] _ = error "Empty database"
newChromosome database@((weight, value):xs) weightRestriction = do
        let itemsNum = length ((weight, value):xs)
        let chromosome = generateEmptyChromosome 5
        takeRand database itemsNum weightRestriction chromosome

takeRand :: [(Int, Int)] -> Int -> Int -> Chromosome -> IO Chromosome
takeRand [] _ _ _ = error "Empty database"
takeRand _ _ _ (Chromosome _ _ []) = error "Invalid chromosome"
takeRand database iterVar weightRestriction chrom@(Chromosome weight value (x:xs))
        | iterVar > 0   = do
                randInt <- getStdRandom (randomR (0, length database - 1))
                let (iWeight, iValue) = database !! randInt
                if (x:xs) !! randInt == False && weight + iWeight <= weightRestriction   then do
                        let newChrom = Chromosome (weight + iWeight) (value + iValue) (setItemInList (x:xs) randInt True)
                        takeRand database (iterVar - 1) weightRestriction newChrom
                else takeRand database (iterVar - 1) weightRestriction chrom
        | iterVar == 0  = pure chrom
        | otherwise     = error "Invalid number to iterate."

setItemInList :: [a] -> Int -> a -> [a]
setItemInList [] _ _ = []
setItemInList (x:xs) n v
        | n > 0         = x:setItemInList xs (n-1) v
        | n == 0        = v:xs
        |otherwise      = error "Invalid index on list."

getRandNum :: (Int, Int) -> IO Int
getRandNum (lo, hi) = getStdRandom (randomR (lo, hi))

generateEmptyChromosome :: Int -> Chromosome
generateEmptyChromosome len = Chromosome 0 0 (generateFalseList len)

generateFalseList :: Int -> [Bool]
generateFalseList n
    | n == 0    = []
    | n > 0     = False: generateFalseList (n-1)
    | otherwise = error "Cannot generate list of negative length."

loadDatabaseFrom :: String -> String -> IO [(Int, Int)]
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
                merge _ _ = error "Merging error"

crossover :: (IO [Chromosome] -> IO Chromosome) -> ((Chromosome, Chromosome) -> IO Chromosome)
        -> IO [Chromosome] -> IO Chromosome
crossover chooseParent crossoverFunc lastGen = do
        parent1 <- chooseParent lastGen
        parent2 <- chooseParent lastGen
        crossoverFunc (parent1, parent2)

defaultCrossover :: [(Int, Int)] -> (Chromosome, Chromosome) -> IO Chromosome
defaultCrossover database (parent1@(Chromosome _ _ vals1), parent2@(Chromosome _ _ vals2)) = do
        --let newChrom = generateEmptyChromosome
        let len = length vals1
        let halfLen = len `div` 2
        randInts <- getRandInts halfLen len
        let randIntsOrd = sortWith (<) randInts
        pure $ getVals 0 len database randIntsOrd (vals1,vals2)
        where
                getVals :: Int -> Int -> [(Int,Int)] -> [Int] -> ([Bool], [Bool]) -> Chromosome
                getVals currIndex maxIndex database randInts (vals1, vals2)
                        | currIndex == maxIndex = Chromosome 0 0 []
                        | otherwise             = do
                                let Chromosome resW resV vals = getVals (currIndex + 1) maxIndex database randInts (vals1, vals2)
                                let (weight, value) = database !! currIndex
                                if randInts `contains` currIndex then ( 
                                        if vals1 !! currIndex then Chromosome (resW + weight) (resV + value) (True:vals)
                                        else Chromosome resW resV (False:vals) )
                                else (
                                        if vals2 !! currIndex then Chromosome (resW + weight) (resV + value) (True:vals)
                                        else Chromosome resW resV (False:vals) )


-- ### CONTINUE HERE ###


getRandInts :: Int -> Int -> IO [Int]
getRandInts amount max = getRandInts' 0 amount max
        where
                getRandInts' :: Int -> Int -> Int -> IO [Int]
                getRandInts' currAmount amount max
                        | currAmount < amount   = do
                                res <- getRandInts' (currAmount + 1) amount max
                                randInt <- getRandNum (0, max)
                                if res `contains` randInt then getRandInts' currAmount amount max
                                else pure (randInt:res)
                        | otherwise             = pure []
                
contains :: Eq a => [a] -> a -> Bool
contains [] arg = False
contains (x:xs) arg
        | x == arg      = True
        | otherwise     = contains xs arg

defaultChooseParent :: IO [Chromosome] -> IO Chromosome
defaultChooseParent lastGenIO = do
        lastGen <- lastGenIO
        randDouble <- getStdRandom (randomR (0 :: Double, 1 :: Double))
        let Chromosome weight value vals = lastGen !! 0
        let refValue = randDouble * fromIntegral value
        randInt <- getStdRandom (randomR (0, length lastGen - 1))
        defaultChooseParent' (>= refValue) (lastGen !! randInt)
        where
                defaultChooseParent' :: (Double -> Bool) -> Chromosome -> IO Chromosome
                defaultChooseParent' test chrom@(Chromosome cWeight cValue vals)
                        | test $ fromIntegral cValue    = pure chrom
                        | otherwise                     = defaultChooseParent lastGenIO

mergeWith :: (a -> a -> Bool) -> [a] -> [a] -> [a]
mergeWith _ x [] = x
mergeWith _ [] y = y
mergeWith comp (x:xs) (y:ys)
        | comp x y  = x: mergeWith comp xs (y:ys)
        | otherwise = y: mergeWith comp (x:xs) ys

toHalfs :: [a] -> ([a], [a])
toHalfs [] = ([],[])
toHalfs (x:xs) = do
        let (a,b) = toHalfs xs
        (b, x:a)

sortWith  :: (a -> a -> Bool) -> [a] -> [a]
sortWith _ [] = []
sortWith _ (x:[]) = [x]
sortWith comp (x:xs) = 
        mergeWith comp firstSorted secondSorted
        where 
                halfLength = (length (x:xs)) `div` 2
                firstSorted = sortWith comp (take halfLength (x:xs))
                secondSorted = sortWith comp (drop halfLength (x:xs))

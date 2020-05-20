module Knapsack where

import System.Random
import System.IO

data Chromosome = Chromosome Int Int [Bool]
        deriving (Show)

newChromosome :: [(Int, Int)] -> Int -> IO Chromosome
newChromosome [] _ = error "Empty database"
newChromosome database@((weight, value):xs) weightRestriction = do
        let itemsNum = length ((weight, value):xs)
        let chromosome = generateEmptyChromosome itemsNum
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


module Knapsack where

import System.Random
import System.IO

data Chromosome = Chromosome Integer [Bool]
        deriving Show

-- newChromosome :: [(Int, Int)] -> Chromosome
-- newChromosome [] = error "Empty database"
-- newChromosome ((weight, value):xs) = do
--         itemsNum <- length ((weight, value):xs)
--         chromosome <- generateEmptyChromosome
--         print chromosome
-- 
-- generateEmptyChromosome :: Int -> Chromosome
-- generateEmptyChromosome len = Chromosome 0 (generateList len)
-- 
-- generateList :: Int -> [Bool]
-- generateList n
--     | n == 0    = []
--     | n > 0     = False: generateList (n-1)
--     | otherwise = error "Cannot generate list of negative length."

loadDatabaseFrom :: String -> String -> IO [(Int, Int)]
loadDatabaseFrom weightsFileName valuesFileName = do
        allLinesW <- readFile weightsFileName
        allLinesV <- readFile valuesFileName
        let wList = map read $ lines allLinesW :: [Int]
        let vList = map read $ lines allLinesV :: [Int]
        merge wList vList
loadDatabaseFrom _ _    = error "Loading error"

merge :: [Int] -> [Int] -> IO [(Int, Int)]
merge (x:xs) (y:ys) = do
        rs <- merge xs ys
        pure ((x,y) : rs)
merge [] [] = pure []
merge _ _ = error "Merging error"
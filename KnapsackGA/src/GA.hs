module GA where

import System.IO
import Debug.Trace
import Knapsack

populationSize = 10 :: Int
mutationChance = 0.25 :: Double

train :: String -> String -> Int -> Int -> IO Chromosome
train weightsFileName valuesFileName genNum weightRestriction = do
    database <- loadDatabaseFrom weightsFileName valuesFileName
    if 0 == length database then (error "Invalid database")
    else do
        lastGen <- train' database 1 genNum weightRestriction mutationChance defaultCrossover $ 
            firstGen database populationSize weightRestriction
        let sorted = sortWith (\x@(Chromosome _ value1 _) y@(Chromosome _ value2 _) -> value1 > value2) lastGen
        pure (sorted !! 0)
    where
        train' :: [(Int, Int)] -> Int -> Int -> Int -> Double -> ([(Int, Int)] -> (Chromosome, Chromosome) -> IO Chromosome) -> IO [Chromosome] -> IO [Chromosome]
        train' database currGenNum genNum weightRestriction mutationProb crossoverFunc lastGenIO
            | currGenNum <= genNum  = do
                lastGen <- lastGenIO
                let sorted = sortWith (\x@(Chromosome _ value1 _) y@(Chromosome _ value2 _) -> value1 > value2) lastGen
                putStrLn ("Generation " ++ show currGenNum ++ " best: " ++ show (sorted !! 0) ++ "\n")
                train' database (currGenNum + 1) genNum weightRestriction mutationProb crossoverFunc $
                    generateNextGen database weightRestriction 1 populationSize mutationProb crossoverFunc $ pure sorted
            | otherwise             = lastGenIO

firstGen :: [(Int, Int)] -> Int -> Int -> IO [Chromosome]
firstGen database populationToGenerate weightRestriction
    | populationToGenerate == 0 = pure []
    | otherwise                 = do
        res <- firstGen database (populationToGenerate - 1) weightRestriction
        chrom <- newChromosome database weightRestriction
        pure (chrom : res)
        
generateNextGen :: [(Int,Int)] -> Int -> Int -> Int -> Double -> ([(Int, Int)] -> (Chromosome, Chromosome) -> IO Chromosome) -> IO [Chromosome] -> IO [Chromosome]
generateNextGen database maxWeight currAmount maxAmount mutationProb crossoverFunc lastGenIO
    | currAmount <= maxAmount    = do
        lastGen <- lastGenIO
        parent1 <- defaultChooseParent lastGen
        parent2 <- defaultChooseParent lastGen
        chrom@(Chromosome weight value vals) <- crossoverFunc database (parent1, parent2)
        gotProb <- getRandDouble (0.0 :: Double, 1.0 :: Double)
        if mutationProb >= gotProb then (do 
            chrom@(Chromosome weight value vals) <- mutate database chrom
            if weight <= maxWeight then do
                rest <- generateNextGen database maxWeight (currAmount + 1) maxAmount mutationProb crossoverFunc lastGenIO
                pure (chrom : rest)
            else generateNextGen database maxWeight currAmount maxAmount mutationProb crossoverFunc lastGenIO )
        else (
            if weight <= maxWeight then do
                res <- generateNextGen database maxWeight (currAmount + 1) maxAmount mutationProb crossoverFunc lastGenIO
                pure (chrom :res)
            else generateNextGen database maxWeight currAmount maxAmount mutationProb crossoverFunc lastGenIO
                )
    | otherwise                 = pure []
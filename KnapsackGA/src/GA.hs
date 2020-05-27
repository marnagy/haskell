module GA where

import System.IO
import Debug.Trace
import Knapsack

populationSize = 50 :: Int
mutationChance = 0.05 :: Double

train :: IO String -> IO String -> IO Int -> IO Int -> IO Chromosome
train weightsFileName valuesFileName genNumIO weightRestrictionIO = do
    weightRestriction <- weightRestrictionIO
    database <- loadDatabaseFrom weightsFileName valuesFileName
    genNum <- genNumIO
    lastGen <- train' database 1 genNum weightRestriction mutationChance defaultCrossover $ firstGen database populationSize weightRestriction
    pure (lastGen !! 0)
    where
        train' :: [(Int, Int)] -> Int -> Int -> Int -> Double -> ([(Int, Int)] -> (Chromosome, Chromosome) -> IO Chromosome) -> IO [Chromosome] -> IO [Chromosome]
        train' database currGenNum genNum weightRestriction mutationProb crossoverFunc lastGen
            | currGenNum <= genNum  = do
                trace ("Generation number " ++ show currGenNum ++ " of " ++ show genNum ++ "generations") $
                    train' database (currGenNum + 1) genNum weightRestriction mutationProb crossoverFunc $ 
                        generateNextGen database weightRestriction 1 populationSize mutationProb crossoverFunc lastGen                
            | otherwise             = lastGen

firstGen :: [(Int, Int)] -> Int -> Int -> IO [Chromosome]
firstGen database populationToGenerate weightRestriction
    | populationToGenerate == 0 = pure []
    | otherwise                 = do
        res <- firstGen database (populationToGenerate - 1) weightRestriction
        chrom <- newChromosome database weightRestriction
        pure (chrom : res)
        
generateNextGen :: [(Int,Int)] -> Int -> Int -> Int -> Double -> ([(Int, Int)] -> (Chromosome, Chromosome) -> IO Chromosome) -> IO [Chromosome] -> IO [Chromosome]
generateNextGen database maxWeight currAmount maxAmount mutationProb crossoverFunc lastGenIO = do
    lastgen <- lastGenIO
    if currAmount < maxAmount then do
        parent1 <- defaultChooseParent lastGenIO
        parent2 <- defaultChooseParent lastGenIO
        chrom@(Chromosome weight value vals) <- crossoverFunc database (parent1, parent2)
        gotProb <- getRandDouble (0.0 :: Double, 1.0 :: Double)
        if mutationProb >= gotProb then (do 
            chrom <- mutate database chrom
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
    else lastGenIO
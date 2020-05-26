module GA where

import System.IO
import Debug.Trace
import Knapsack

populationSize = 50 :: Integer
mutationChance = 0.05 :: Double

train :: IO String -> IO String -> IO Int -> IO Int -> Chromosome
train weightsFileName valuesFileName genNum weightRestriction = do
    database <- loadDatabaseFrom weightsFileName valuesFileName
    (train' database 1 genNum mutationChance defaultCrossover $ firstGen database populationSize weightRestriction) !! 0
    where
        train' :: Integral a => [(Int, Int)] -> a -> a -> Double -> ([(Int, Int)] -> (Chromosome, Chromosome) -> IO Chromosome) -> [Chromosome] -> [Chromosome]
        train' database currGenNum genNum mutationProb crossoverFunc lastGen
            | currGenNum <= genNum  = do
                trace ("Generation number " ++ show currGenNum ++ " of " ++ show genNum ++ "generations") $
                    train' database (currGenNum + 1) genNum mutationProb crossoverFunc $ 
                        generateNextGen database weightRestriction 1 populationSize mutationProb crossoverFunc lastGen
                --parent1 <- defaultChooseParent lastGen
                --parent2 <- defaultChooseParent lastGen
                
            | otherwise             = lastGen

firstGen :: [(Int, Int)] -> Int -> Int -> [Chromosome]
firstGen database populationToGenerate weightRestriction
    | populationToGenerate == 0 = []
    | otherwise                 = newChromosome database weightRestriction : firstGen database (populationToGenerate - 1) weightRestriction
        
generateNextGen :: [(Int,Int)] -> Int -> Int -> Int -> Double -> ([(Int, Int)] -> (Chromosome, Chromosome) -> IO Chromosome) -> [Chromosome] -> [Chromosome]
generateNextGen database maxWeight currAmount maxAmount mutationProb crossoverFunc lastGen
    | currAmount < maxAmount    = do
        parent1 <- defaultChooseParent lastGen
        parent2 <- defaultChooseParent lastGen
        chrom@(Chromosome weight value vals) <- crossoverFunc database (parent1, parent2)
        gotProb <- getRandDouble (0.0 :: Double, 1.0 :: Double)
        if mutationProb >= gotProb then do 
            chrom <- mutate database chrom
            if weight <= maxWeight then chrom : generateNextGen database maxWeight (currAmount + 1) maxAmount mutationProb crossoverFunc lastGen
            else generateNextGen database maxWeight currAmount maxAmount mutationProb crossoverFunc lastGen
        else (
            if weight <= maxWeight then chrom : generateNextGen database maxWeight (currAmount + 1) maxAmount mutationProb crossoverFunc lastGen
            else generateNextGen database maxWeight currAmount maxAmount mutationProb crossoverFunc lastGen
                )
        
    | otherwise                 = lastGen
module GA where

import System.IO
import Knapsack
import Debug.Trace

populationSize = 50 :: Integer
mutationProb = 0.05 :: Double

train :: IO String -> IO Int -> IO Int -> Chromosome
train databaseFileName genNum weightRestriction = do
    let database = loadData databaseFileName
    (train' database 1 genNum mutationProb crossoverFunc $ firstGen database population weightRestriction ) !! 0
    where
        train' :: Integral a => [(Int, Int)] -> a -> a -> Double -> ((Chromosome, Chromosome) -> IO Chromosome) -> [Chromosome] -> [Chromosome]
        train' database currGenNum genNum mutationProb crossoverFunc lastGen
            | currGenNum <= genNum  = do
                Trace ("Generation number " ++ show currGenNum ++ " of " ++ show genNum ++ "generations") $
                    train' database (currGenNum + 1) genNum mutationProb crossoverFunc $ 
                        generateNextGen database 
                --parent1 <- defaultChooseParent lastGen
                --parent2 <- defaultChooseParent lastGen
                
            | otherwise             = error "Invalid generation number"

firstGen :: [(Int, Int)] -> Int -> Int -> [Chromosome]
firstGen database populationToGenerate weightRestriction
    | populationToGenerate == 0 = []
    | otherwise                 = newChromosome database weightRestriction : firstGen database (populationToGenerate - 1) weightRestriction
        
generateNextGen :: [(Int,Int)] -> Int -> Int -> Double -> ((Chromosome, Chromosome) -> IO Chromosome) -> [Chromosome] -> [Chromosome]
generateNextGen database currAmount maxAmount mutationProb crossoverFunc lastGen
    | currAmount < maxAmount    = do
        -- ### CONTINUE HERE ###
    | otherwise                 = lastGen
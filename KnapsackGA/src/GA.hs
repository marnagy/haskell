module GA where

import System.IO
import Knapsack

populationSize = 50 :: Integer

train :: IO String -> IO Int -> IO Int
train databaseFileName genNum = do
    train' (loadData DatabaseFileName) 0 genNum mutationProb crossoverFunc ()
    where
        train' :: [(Int, Int)] -> Integer -> Integer -> Double -> ([(Int, Int)] -> [(Int,Int)]) -> [Chromosome] -> [(Int,Int)]
        train' database currGenNum genNum mutationProb crossoverFunc lastGen
            | currGenNum <= genNum  = do
                parent1 <- defaultChooseParent lastGen
                parent2 <- defaultChooseParent lastGen
                
            | otherwise             = error "Invalid generation number"

fRepeat :: (a -> b) -> Integer -> [b]
fRepeat f num = fRepeat' f 0 num
    where
        fRepeat' :: (a -> b) -> Integer -> Integer -> [b]
        fRepeat' f curr max = 
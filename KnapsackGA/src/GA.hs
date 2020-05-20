module GA where

import System.IO

populationSize = 50 :: Integer
currentGen = 0 :: Integer
currPopulation = [] :: [Chromosome]

train :: IO String -> IO Int -> IO Int
train DatabaseFileName genNum = do
    train' (loadData DatabaseFileName) 0 genNum mutationProb crossoverFunc
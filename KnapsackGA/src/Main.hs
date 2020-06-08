-- | Main module
module Main 
    ( main ) where 

import GA
import Knapsack

-- | Main function.
main :: IO ()
main = do
    putStrLn "Input name of database file for weigths:"
    weightsFileName <- getLine
    putStrLn "Input name of database file for values:"
    valuesFileName <- getLine
    putStrLn "Input number of maximum weight:"
    maxWeight <- readInt
    putStrLn "Input number of generations to train:"
    genNum <- readInt
    putStrLn "Starting training..."
    res <- train weightsFileName valuesFileName genNum maxWeight
    putStrLn ("Best: " ++ show res)
    
-- | Read Int from console.
readInt :: IO Int
readInt = do
    int <- getLine
    return (read int)
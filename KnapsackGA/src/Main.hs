module Main where

main :: IO ()
main = do
    putStrLn "Input name of database file:"
    fileName <- getLine
    putStrLn "Input number of generations to train:"
    genNum <- readInt
    putStrLn "Training initializing"
    

readInt :: IO Int
readInt = do
    int <- getLine
    return (read int)
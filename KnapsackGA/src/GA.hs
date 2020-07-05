-- | General implementation of GA for the knapsack problem.
module GA 
    (train, firstGen, generateNextGen) where

import System.IO
import Debug.Trace
import Knapsack

-- | Default population size.
populationSize = 20 :: Int

-- | Default mutation chance.
mutationChance = 0.001 :: Double

-- | Data type to store values that are not chaning thoughout the algorithm.
--
-- Contains: Database p(weight, values)], maximum weight, mutation probability, crossover function
data GA_Args = Args [(Int, Int)] Int Double ([(Int, Int)] -> (Chromosome, Chromosome) -> IO Chromosome)

-- | Main training function.
--
-- Sorts each generation and starts generating new one if amount of generations
-- has not been reached.
train :: String -- ^ Name of file containing weights.
    -> String -- ^ Name of file containing values.
    -> Int -- ^ Number of generations to train.
    -> Int -- ^ Maximum amount of weight allowed.
    -> IO Chromosome -- ^ Best solution from the last generation.
train weightsFileName valuesFileName genNum weightRestriction = do
    database <- loadDatabaseFrom weightsFileName valuesFileName
    if 0 == length database then (error "Invalid database")
    else do
        let args = Args database weightRestriction mutationChance defaultCrossover
        lastGen <- train' args 1 genNum $ 
            firstGen database populationSize weightRestriction
        let sorted = sortWith (\x@(Chromosome _ value1 _) y@(Chromosome _ value2 _) -> value1 > value2) lastGen
        pure (sorted !! 0)
    where
        train' :: GA_Args -> Int -> Int -> IO [Chromosome] -> IO [Chromosome]
        train' args@(Args database weightRestriction mutationProb crossoverFunc) currGenNum genNum lastGenIO
            | currGenNum <= genNum  = do
                lastGen <- lastGenIO
                let sorted = sortWith (\x@(Chromosome _ value1 _) y@(Chromosome _ value2 _) -> value1 > value2) lastGen
                putStrLn ("Generation " ++ show currGenNum ++ " best: " ++ show (sorted !! 0) ++ "\n")
                train' args (currGenNum + 1) genNum $
                    generateNextGen args 1 populationSize $ pure sorted
            | otherwise             = lastGenIO

-- | Generate first random unsorted generation.
firstGen :: [(Int, Int)] -- ^ Database of (weight, value).
    -> Int -- ^ Amount of solution in one generation.
    -> Int -- ^ Maximum amount of weight allowed.
    -> IO [Chromosome] -- ^ First generation.
firstGen database populationToGenerate weightRestriction
    | populationToGenerate == 0 = pure []
    | otherwise                 = do
        res <- firstGen database (populationToGenerate - 1) weightRestriction
        chrom <- newChromosome database weightRestriction
        pure (chrom : res)

-- | Generate next generation using given crossover and mutation.
generateNextGen :: GA_Args -- ^ Stores immutable arguments for generating next generation.
    -> Int -- ^ Current number of generated memberrs of next generation.
    -> Int -- ^ Amount of members to generate for next population.
    -> IO [Chromosome] -- ^ Last generation.
    -> IO [Chromosome] -- ^ Next generation.
generateNextGen args@(Args database weightRestriction mutationProb crossoverFunc) currAmount maxAmount lastGenIO
    | currAmount <= maxAmount    = do
        lastGen <- lastGenIO
        parent1 <- defaultChooseParent lastGen
        parent2 <- defaultChooseParent lastGen
        chrom@(Chromosome weight value vals) <- crossoverFunc database (parent1, parent2)
        gotProb <- getRandDouble (0.0 :: Double, 1.0 :: Double)
        if mutationProb >= gotProb then (do 
            chrom@(Chromosome weight value vals) <- mutate database chrom
            if weight <= weightRestriction then do
                rest <- generateNextGen args(currAmount + 1) maxAmount lastGenIO
                pure (chrom : rest)
            else generateNextGen args currAmount maxAmount lastGenIO )
        else (
            if weight <= weightRestriction then do
                res <- generateNextGen args (currAmount + 1) maxAmount lastGenIO
                pure (chrom :res)
            else generateNextGen args currAmount maxAmount lastGenIO
                )
    | otherwise                 = pure []
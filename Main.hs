module Main (main) where

import Coord
import Chain
import Moves
import Print
import HPModel
import Metro

import System.Random.MWC
import Control.Monad.Primitive
import qualified Data.Vector as V
import System.Environment

-- Get probability for one list element given uniform distribution
getProb :: [a] -> Double
getProb list = fromRational $ 1 / (fromIntegral $ length list)

-- Randomly select one candidate of all given pullMoves
generateCandidate :: (PrimMonad m, Coord a)    => 
                    Chain a     -> 
                    Gen (PrimState m) -> 
                    m (Candidate (Chain a))
generateCandidate ch gen = do 
    let list = pullMoves ch
    chosenMove <-  pick list gen
    let candidate = after chosenMove
    let prob = getProb list
    let probBack = getProb $ pullMoves candidate
    return $ Candidate candidate (prob, probBack)

-- Generate a chain with a fixed length
createChain :: Coord a => Int -> Chain a
createChain n = fromList $ generateList n


generateTemps :: Int -> [Double]
generateTemps n = [ef $ fromIntegral t | t <- [0..n]]
	    where
		ef :: Double -> Double
		ef t = a * exp ((- t) * b / fromIntegral n)
		a :: Double
		a = 100
		b :: Double
		b = 10
		pf :: Double -> Double
		pf t = a * (1 - t / fromIntegral n) ^ p
		p :: Int
		p = 2

run :: String -> Int -> IO ()
run input iterations = do    
    let residues = V.fromList $ createResidues input
    let chain = (createChain (V.length residues)) :: Chain Coord3d
    let temps = generateTemps iterations
    let score ch = - (energy residues ch) 
    run' score chain residues temps

-- Run the algorithm!
run' :: Coord a => (Chain a -> Double) -> Chain a -> V.Vector HPResidue -> [Double] -> IO() 
run' score chain residues temps = do 
            g <- createSystemRandom
            (x, i) <- metropolisHastings score generateCandidate chain g temps
            printJGReadable x residues 

printHReadable x i res = do
            printHP res x
            putStrLn "----------------------------"
            putStrLn (show x)
            putStrLn $ "Number of accepted transitions: " ++ (show i)
            putStrLn $ "Final energy: " ++ show (energy res x)

printJGReadable x res = do
        putStrLn $ unlines $ chainToJGList x $ V.toList res


-- TODO: lite felhantering kanske
main :: IO ()
main = do
    (input:iterations:_) <- getArgs
    run input (read iterations)

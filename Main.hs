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

data PullMoveState a = PMState {
                                currState :: (Chain a),
                                possMoves ::  (V.Vector (Chain a))
                                }

genPullCand :: (PrimMonad m, Coord a) => Gen (PrimState m) 
                                      -> PullMoveState a 
                                      -> m (Candidate (PullMoveState a))
genPullCand gen (PMState _ pMoves) = do
    candChain <- pick pMoves gen
    let candPMoves = V.fromList $ map after $ pullMoves candChain
    let px = getProb pMoves
    let py = getProb candPMoves
    let cand = PMState candChain candPMoves
    return $ Candidate cand px py

makePMS :: Coord a => Chain a -> PullMoveState a
makePMS ch = PMState ch (V.fromList $ map after $ pullMoves ch)


-- Select one element at random from a list
pick :: (PrimMonad m) => V.Vector a -> Gen (PrimState m) -> m a
pick xs gen = uniformR (0, (V.length xs)-1) gen >>= return . (xs V.!)

-- Get probability for one list element given uniform distribution
getProb :: V.Vector a -> Double
getProb list = 1 --fromRational $ 1 / (fromIntegral $ V.length list)

-- Generate a chain with a fixed length
createChain :: Coord a => Int -> Chain a
createChain n = fromList $ generateList n


generateTemps :: Int -> [Double]
generateTemps n = [ef $ fromIntegral t | t <- [0..n]]
	    where
		ef :: Double -> Double
		ef t = a * exp ((- t) * b / fromIntegral n)
		a :: Double
		a = 1
		b :: Double
		b = 1000
		pf :: Double -> Double
		pf t = a * (1 - t / fromIntegral n) ^ p
		p :: Int
		p = 2

expQuota :: Double -> Double -> Double -> Double
expQuota chx chy t = exp ((chx - chy) / t)

expScore :: Coord a => V.Vector HPResidue 
                    -> PullMoveState a 
                    -> PullMoveState a 
                    -> Double 
                    -> Double
expScore residues chx chy t = expQuota before after t
    where 
        before = -(energy residues $ currState chx) 
        after =  -(energy residues $ currState chy)

run :: String -> Int -> IO ()
run input iterations = do    
    let residues = V.fromList $ createResidues input
    let chain = (createChain (V.length residues)) :: Chain CoordFCC 
    let temps = generateTemps iterations
    g <- createSystemRandom
    let init = makePMS chain
    res <- metropolisHastings (expScore residues) (genPullCand g) g init temps 
    printJGReadable (currState $ head res) (length res) residues

printHReadable x i res = do
            printHP res x
            putStrLn "----------------------------"
            putStrLn (show x)
            putStrLn $ "Number of accepted transitions: " ++ (show i)
            putStrLn $ "Final energy: " ++ show (energy res x)

printJGReadable x i res = do
        putStrLn $ unlines $ chainToJGList x $ V.toList res


-- TODO: lite felhantering kanske
main :: IO ()
main = do
    (input:iterations:_) <- getArgs
    run input (read iterations)

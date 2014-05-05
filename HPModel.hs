module HPModel (
                 energy
               , energyWithList
               , HPResidue
               , createResidues
               , isHydrophobic
               ) where

import Chain
import Coord
import qualified Data.Vector as V
import Data.Maybe (catMaybes, mapMaybe)
import Data.Char (isDigit)

class NeighborResidue n where
    residueEnergy :: n -> n -> Int

data HPResidue = H | P deriving (Show, Read)

instance NeighborResidue HPResidue where
    residueEnergy H H = -1
    residueEnergy _ _ = 0

isHydrophobic :: HPResidue -> Bool
isHydrophobic H = True
isHydrophobic _ = False

-- Create a list of residues from an input string
createResidues :: String -> [HPResidue]
createResidues [] = []
createResidues (x:[]) = [read [x]]
createResidues (x:y:xs) = if isDigit y
                          then (replicate (read [y]) (read [x])) ++ createResidues xs
                          else (read [x]) : (createResidues (y:xs))

energy :: (Coord a, NeighborResidue n) => V.Vector n -> Chain a -> Double
energy res ch = fromIntegral $ V.ifoldl f 0 res
    where
        f acc i x = acc + ( foldr ((+) . residueEnergy x . (res V.!)) 0 $ 
                    mapMaybe (cIndex ch) $ 
                    validNeighbors ch i )

energyWithList :: (Coord a, NeighborResidue n) => [n] -> Chain a -> Double
energyWithList res = energy (V.fromList res)

validNeighbors :: (Coord a) => Chain a -> Int -> [a]
validNeighbors ch i = filter (notNextTo ch i) (neighbors (ch!i))

notNextTo :: (Coord a) => Chain a -> Int -> a -> Bool
notNextTo ch i coord = notElem coord . catMaybes $ [ch!?(i-1), ch!?(i+1)]


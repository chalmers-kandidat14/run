{-
 Module for assessing similarity between two chains
-}

module Similarity where

import Chain
import HPModel
import Coord

import Data.List (intersect)
import Data.Maybe (mapMaybe, catMaybes)
import GraphConverter

similarity :: Coord a => [HPResidue] -> Chain a -> Chain a -> Int
similarity res ch ch' = foldr f 0 $ zip a b 
    where
        f (xs,ys) acc = acc + (sum (overlap xs ys)) 
        
        -- We intersect with the indices of the H residues
        -- so we only check the connections of the H
        overlap xs ys = xs `intersect` ys
       
        connections = sum (concat a)
        a = buildGraph res ch
        b = buildGraph res ch'

-- Builds a matrix with the the connections for each residue (row)
-- observe that each connection is only counted once, so the matrix
-- is not symmetric

type Graph = [[Int]]

printGraph :: Graph -> String
printGraph = unlines . map (unwords . map show) . convertGraph 

buildGraph :: (Coord a) => [HPResidue] -> Chain a -> Graph
buildGraph res ch = map f indices
    where
        f = intersect indices .
            mapMaybe (cIndex ch) . 
            validNeighbors ch 
        
        indices = [0..(n-1)]
        hIndices = map snd $
                       filter (isHydrophobic . fst) $ 
                       zip res [0..]
        
        n = cLength ch

validNeighbors :: (Coord a) => Chain a -> Int -> [a]
validNeighbors ch i = filter (notNextTo ch i) (neighbors (ch!i))

notNextTo :: (Coord a) => Chain a -> Int -> a -> Bool
notNextTo ch i coord = notElem coord . catMaybes $ [ch!?(i-1), ch!?(i+1)]

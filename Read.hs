module Read  where

import Chain
import Coord
import Data.List
import Data.Maybe
import Data.Either
import Text.CSV

--Takes a string formatted as a csv matrix. A cell with 0
--means that position is empty, a cell with integer i>0 means
--that position contains residue i in the chain.
csvToChain :: String -> Chain Coord2d
csvToChain = fromNumGrid . csvToNumGrid

fromNumGrid :: [[Int]] -> Chain Coord2d
fromNumGrid grid = Chain.fromList $ map resCoord [1..(maxRes grid)]
	where
		maxRes = maximum . concat
		resCoord i = Coord2d (xRes i) (yRes i)
		xRes i = head $ mapMaybe (elemIndex i) grid
		yRes i = fromJust $ elemIndex True (map (elem i) grid)

csvToNumGrid :: String -> [[Int]]
csvToNumGrid str = let Right csv = Text.CSV.parseCSV "" str in map (map read) csv


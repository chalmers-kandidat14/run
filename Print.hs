{-#LANGUAGE FlexibleInstances#-}
module Print (
               printChain
             , printHP
             , chainToJGList
             ) where

import Chain
import Coord
import Moves
import HPModel
import qualified Data.Vector as V
import Data.List (elemIndex)

chainToJGList :: Coord a => Chain a -> [HPResidue] -> [String]
chainToJGList ch hpl = zipWith format (toList ch) hpl
	where
		format coord hp = show coord ++ " " ++ show hp

printChain :: Chain Coord2d -> IO ()
printChain = putStr . show

printHP :: V.Vector HPResidue -> Chain Coord2d -> IO ()
printHP res ch = putStr $ showRowsWith disp ch
    where disp = show . (res V.!)

showRowsWith :: (Int -> String) -> Chain Coord2d -> String
showRowsWith disp chain = unlines $ map (showRowWith disp coords) [yMin..yMax]
    where
        coords = toList chain
        yMin = snd $ getMin coords
        yMax = snd $ getMax coords

showRowWith ::  (Int -> String) -> [Coord2d] -> Int -> String
showRowWith disp chain j = 
    foldr print "" gridrow
    where
        gridrow = createCoords [xMin..xMax] [j]
        xMin = fst $ getMin chain
        xMax = fst $ getMax chain
        addSpace str =  if length str < 3
                        then addSpace (' ':str)
                        else str
        print cell output = case elemIndex cell chain of
                                Nothing -> "   " ++ output
                                Just i -> addSpace (disp i) ++ output

instance Show (Chain Coord2d) where
        show = (showRowsWith show)

instance Show (Chain Coord3d) where
        show = unlines . map show . toList

instance Show (Chain CoordFCC) where
        show = unlines . map show . toList

module Moves (
               pullMoves
             , Move (..)
             , Direction (..)
             , generatePullMoves
             ) where 

import Chain
import Coord

data Direction = Up | Down deriving (Eq)

data Move a = Move { before :: Chain a
                   , after :: Chain a
                   , id :: String
                   } deriving (Eq)

instance Show (Move a) where
    show (Move _ _ id) = show id

pullMoves :: (Coord a, Show a) => Chain a -> [Move a]
pullMoves ch = do
    dir <- [Up, Down]
    i   <- [2..(cLength ch -2)]
    generatePullMoves ch i dir

generatePullMoves :: (Show a, Coord a) => Chain a -> Int -> Direction -> [Move a]
generatePullMoves ch i Up = map f diffs
                where 
                    ch'   = cReverse ch
                    i'    = cLength ch - i - 1 -- We have to put in length ch here
                    f diff = Move ch (replace ch i diff) msg
                    diffs = map (pull ch' i') (nearbyPointPairs ch' i')
                    msg = show (ch!i) ++ " up"
                            
generatePullMoves ch i Down = map f diffs
                where 
                    diffs = map (pull ch i) (nearbyPointPairs ch i)
                    f diff = Move ch (replace ch (i+1-length diff) (reverse diff)) msg
                    msg    = show (ch!i) ++ " down"

pull :: Coord a => Chain a -> Int -> (a, a) -> [a]
pull ch i (c, l) | ch!(i-1) == c = [l]
                 | otherwise     = l:c:follow (i-2)
      where 
        follow 0 = [ ch ! 2 ]
        follow j | j < 0 = []
                 | ( ch!(j+2) ) `adj` ( ch!(j-1) ) = [ch!(j+2)]
                 | otherwise = ch!(j+2) : follow (j-1)

nearbyPointPairs :: Coord a => Chain a -> Int -> [(a, a)]
nearbyPointPairs ch i = filter (cEmpty ch . snd) $ -- L must be empty
                         filter (valid . fst) $       -- C must be empty or contain i-1
                         neighborPairs (ch!i) (ch!(i+1)) 
    where valid coord = cEmpty ch coord || coord == (ch!(i-1))

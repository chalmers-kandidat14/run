module Coord (
               Coord (..)
             , Coord3d 
             , CoordFCC
             , Coord2d
             , getMin
             , getMax
             , createCoords
             ) where

class (Show a, Eq a, Ord a) => Coord a where
    adj :: a -> a -> Bool
    dadj :: a -> a -> Bool
    neighbors :: a -> [a]
    neighborPairs :: a -> a -> [(a,a)]
    generateList :: Int -> [a]

data Coord3d = Coord3d { x3Coord :: Int
                       , y3Coord :: Int
                       , z3Coord :: Int
                       } deriving (Ord, Eq)

instance Show Coord3d where
   show (Coord3d x y z) = show x ++ " " ++ show y ++  " " ++ show z

instance Coord Coord3d where
    adj a b  = abs (x3Coord a - x3Coord b) +
               abs (y3Coord a - y3Coord b) +
               abs (z3Coord a - z3Coord b) == 1

    dadj a b = undefined
    
    generateList n = [Coord3d x 0 0| x <- [1..n]]

    neighbors (Coord3d x y z) = [Coord3d (x+1) y z
                                , Coord3d x (y+1) z
                                , Coord3d x y (z+1)]

    neighborPairs (Coord3d ax ay az) (Coord3d bx by bz)
                | ax == bx = [ (Coord3d (ax+1) ay az, Coord3d (bx+1) by bz),
                               (Coord3d (ax-1) ay az, Coord3d (bx-1) by bz) ,
                               (Coord3d ax ay (az+1), Coord3d bx by (bz+1)),
                               (Coord3d ax ay (az-1), Coord3d bx by (bz-1))]
                | ay == by = [ (Coord3d ax (ay+1) az, Coord3d bx (by+1) bz),
                               (Coord3d ax (ay-1) az, Coord3d bx (by-1) bz),
                               (Coord3d ax ay (az+1), Coord3d bx by (bz+1)),
                               (Coord3d ax ay (az-1), Coord3d bx by (bz-1)) ]
                | az == bz = [ (Coord3d (ax+1) ay az, Coord3d (bx+1) by bz),
                               (Coord3d (ax-1) ay az, Coord3d (bx-1) by bz),
                               (Coord3d ax (ay+1) az, Coord3d bx (by+1) bz),
                               (Coord3d ax (ay-1) az, Coord3d bx (by-1) bz) ]

data CoordFCC = CoordFCC { xFCC :: Int
                         , yFCC :: Int
                         , zFCC :: Int
                         } deriving (Ord, Eq)

instance Show CoordFCC where
    show (CoordFCC x y z) = show x ++ " " ++ show y ++ " " ++ show z

instance Coord CoordFCC where
    adj a b = abs (xFCC a - xFCC b) <= 1 &&
              abs (yFCC a - yFCC b) <= 1 &&
              abs (zFCC a - zFCC b) <= 1 &&
              abs (xFCC a - xFCC b) +
              abs (yFCC a - yFCC b) + 
              abs (zFCC a - zFCC b) == 2
    
    dadj a b = undefined

    generateList n = map createCoord $ zip3 [0..(n-1)] ys zs
        where 
            ys = cycle [0, 1, 0, 0]
            zs = cycle [0, 0, 0, 1]
            createCoord (x, y, z) = CoordFCC x y z

    neighbors (CoordFCC x y z) = [ CoordFCC x     (y+1) (z+1)
                                 , CoordFCC x     (y-1) (z+1)
                                 , CoordFCC x     (y+1) (z-1)
                                 , CoordFCC x     (y-1) (z-1)
                                 , CoordFCC (x+1) (y+1) z
                                 , CoordFCC (x-1) (y+1) z
                                 , CoordFCC (x+1) (y-1) z
                                 , CoordFCC (x-1) (y-1) z
                                 , CoordFCC (x+1) y     (z+1)
                                 , CoordFCC (x-1) y     (z+1)
                                 , CoordFCC (x+1) y     (z-1)
                                 , CoordFCC (x-1) y     (z-1) ]

    neighborPairs (CoordFCC ax ay az) (CoordFCC bx by bz)
       | ax == bx = [ (CoordFCC ax (ay-1) (az-1), CoordFCC bx (by-1) (bz-1)) 
                    , (CoordFCC ax (ay+1) (az+1), CoordFCC bx (by+1) (bz+1)) 
                    , (CoordFCC (ax+1) ay (az+1), CoordFCC (bx+1) by (bz+1))
                    , (CoordFCC (ax-1) ay (az+1), CoordFCC (bx-1) by (bz+1))
                    , (CoordFCC (ax+1) ay (az-1), CoordFCC (bx+1) by (bz-1))
                    , (CoordFCC (ax-1) ay (az-1), CoordFCC (bx-1) by (bz-1))
                    , (CoordFCC (ax-1) (ay-1) az, CoordFCC (bx-1) (by-1) bz)
                    , (CoordFCC (ax+1) (ay+1) az, CoordFCC (bx+1) (by+1) bz)
                    , (CoordFCC (ax+1) (ay-1) az, CoordFCC (bx+1) (by-1) bz)
                    , (CoordFCC (ax-1) (ay+1) az, CoordFCC (bx-1) (by+1) bz)
                    ]
       | ay == by = [ (CoordFCC ax (ay-1) (az+1), CoordFCC bx (by-1) (bz+1))
                    , (CoordFCC ax (ay+1) (az-1), CoordFCC bx (by+1) (bz-1))
                    , (CoordFCC ax (ay-1) (az-1), CoordFCC bx (by-1) (bz-1)) 
                    , (CoordFCC ax (ay+1) (az+1), CoordFCC bx (by+1) (bz+1)) 
                    , (CoordFCC (ax+1) ay (az+1), CoordFCC (bx+1) by (bz+1))
                    , (CoordFCC (ax-1) ay (az-1), CoordFCC (bx-1) by (bz-1))
                    , (CoordFCC (ax-1) (ay-1) az, CoordFCC (bx-1) (by-1) bz)
                    , (CoordFCC (ax+1) (ay+1) az, CoordFCC (bx+1) (by+1) bz)
                    , (CoordFCC (ax+1) (ay-1) az, CoordFCC (bx+1) (by-1) bz)
                    , (CoordFCC (ax-1) (ay+1) az, CoordFCC (bx-1) (by+1) bz)
                    ]
       | az == bz = [ (CoordFCC ax (ay-1) (az+1), CoordFCC bx (by-1) (bz+1))
                    , (CoordFCC ax (ay+1) (az-1), CoordFCC bx (by+1) (bz-1))
                    , (CoordFCC ax (ay-1) (az-1), CoordFCC bx (by-1) (bz-1)) 
                    , (CoordFCC ax (ay+1) (az+1), CoordFCC bx (by+1) (bz+1)) 
                    , (CoordFCC (ax+1) ay (az+1), CoordFCC (bx+1) by (bz+1))
                    , (CoordFCC (ax-1) ay (az+1), CoordFCC (bx-1) by (bz+1))
                    , (CoordFCC (ax+1) ay (az-1), CoordFCC (bx+1) by (bz-1))
                    , (CoordFCC (ax-1) ay (az-1), CoordFCC (bx-1) by (bz-1))
                    , (CoordFCC (ax-1) (ay-1) az, CoordFCC (bx-1) (by-1) bz)
                    , (CoordFCC (ax+1) (ay+1) az, CoordFCC (bx+1) (by+1) bz)
                    ]

 
data Coord2d = Coord2d {xCoord :: Int, yCoord :: Int} deriving (Ord, Eq)

instance Show Coord2d where
   show (Coord2d x y) = show x ++ " " ++ show y ++ " 0"

instance Coord Coord2d where
    adj a b  = abs (xCoord a - xCoord b) + 
               abs (yCoord a - yCoord b) == 1

    dadj a b = abs (xCoord a - xCoord b) == 1 && 
               abs (yCoord a - yCoord b) == 1
    
    neighbors (Coord2d x y) = [Coord2d (x+1) y, Coord2d x (y+1)]

    neighborPairs (Coord2d ax ay) (Coord2d bx by)
                | ax == bx = [ (Coord2d (ax+1) ay, Coord2d (bx+1) by),
                               (Coord2d (ax-1) ay, Coord2d (bx-1) by) ]
                | ay == by = [ (Coord2d ax (ay+1), Coord2d bx (by+1)),
                               (Coord2d ax (ay-1), Coord2d bx (by-1)) ]
    generateList n = [Coord2d x 0 | x <- [1..n]]

{- Some functions for printing the 2d chain in the terminal, see Print.hs -}

createCoords :: [Int] -> [Int] -> [Coord2d]
createCoords xs ys = [Coord2d x y | x <- xs, y <- ys]

getMin :: [Coord2d] -> (Int, Int)
getMin list = ((f xCoord list), (f yCoord list)) 
    where
       f fun = minimum . map fun 

getMax :: [Coord2d] -> (Int, Int)
getMax list = ((f xCoord list), (f yCoord list))
    where
        f fun = maximum . map fun


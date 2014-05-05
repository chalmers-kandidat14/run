module GraphConverter where

import Data.List

convertAndPrintFile :: String -> IO()
convertAndPrintFile file = do
    content <- readFile file
    let table = map ((map read) . words) $ lines content
    let graph = convertGraph table
    return ()
    writeFile (file ++ "-converted") $ unlines $ (map (unwords . map show)) $ graph

convertGraph :: [[Int]] -> [[Int]]
convertGraph table = map (convertLine $ length table) table

convertLine :: Int -> [Int] -> [Int]
convertLine n xs = take (length res - 1) res
    where
        res = foldr f [] (addOne $ listDiff $ sort (xs++[n])) 
        f x acc = (replicate (x-1) 0) ++ [1] ++ acc 
        addOne (y:ys) = (y+1):ys

listDiff :: [Int] -> [Int]
listDiff xs = zipWith (-) xs (0:xs)

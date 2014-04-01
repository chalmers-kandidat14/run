
module AAConverter (translate) where

import System.Environment
import Control.Monad
import Data.Char (toUpper)

type AA = String
type HP = String

polar :: [(AA, HP)]
polar = map (\x -> ([x], "P")) "DEHKNOQRSW"

hydrophobic :: [(AA, HP)]
hydrophobic = map (\x -> ([x], "H")) "YACFGILMPV"

translateAAtoHP :: [AA] -> [(AA, Maybe HP)]
translateAAtoHP = map (\x -> (x, lookup x aaTable))
  where
    aaTable = polar ++ hydrophobic

translate :: [AA] -> [HP]
translate = map f . translateAAtoHP
  where
    f (aa, Nothing) = error ("Could not match the amino acid \""
                            ++ aa ++ "\" with any entry in the dictionary.")
    f (aa, Just x)  = x

main :: IO ()
main = getArgs >>= putStrLn
                   . unwords
                   . translate
                   . map (map toUpper)
                   . concatMap (concatMap (words) . lines)


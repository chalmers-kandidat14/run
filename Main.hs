module Main where
import Folder
import HPModel
import GraphConverter
import Similarity
import Chain
import Print (chainToJGList) 
import System.IO
import Control.Monad (forM_)
import System.Environment
import System.Directory (createDirectoryIfMissing)

runLarge input iter repeats = do
    let residues = createResidues input
    let iterations = read iter :: Int
    let n = read repeats :: Int
    let writeOut energyH i res = do
                        writeFile ("output/chains/chain-" ++ show i ++ ".csv")
                                  (unlines $ map show $ toList (head res))
                        writeFile ("output/graphs/graph-" ++ show i ++ ".csv")
                                  (printGraph $ buildGraph residues (head res))
                        hPutStrLn energyH 
                                  (show $ energyWithList residues $ head res)
                        putStrLn $ "Finished number " ++ show i ++ " of " ++ show n
    
    createDirectoryIfMissing True "output/graphs"
    createDirectoryIfMissing True "output/chains"
    energyHandle <- openFile "output/energies.csv" WriteMode
    writeFile "output/residues.csv" (unlines $ map show residues)
    forM_ [1..n] (\i -> (run residues iterations :: IO FCC) >>= (writeOut energyHandle i))
    hClose energyHandle
    putStrLn "Saved results in folder 'output'"

run2d input iterations  = do
    let residues = createResidues input
    res <- run residues (read iterations) :: IO C2d
    printJGReadable (head res) (length res) residues
        
run2dReadable input iterations  = do
    let residues = createResidues input
    res <- run residues (read iterations) :: IO C2d
    printHReadable (head res) (length res) residues

runFCC input iterations = do
    let residues = createResidues input
    res <- run residues (read iterations) :: IO FCC
    printJGReadable (head res) (length res) residues

run3d input iterations  = do
    let residues = createResidues input
    res <- run residues (read iterations) :: IO C3d
    printJGReadable (head res) (length res) residues 

printHelp = do
    putStrLn "Usage: pfolder [-l latticetype] <residues> <iterations>"
    putStrLn ""
    putStrLn "Lattice types: 2d, 2d-r, 3d, fcc (default: fcc)"
    putStrLn ""
    putStrLn "Large run: pfolder large <residues> <iterations> <number of runs>"


main :: IO ()
main = do
    
    args <- getArgs
    case args of
        ["convert-graph", file] -> convertAndPrintFile file
        [input, iterations] -> runFCC input iterations 
        ["-l", "fcc", input, iterations] -> runFCC input iterations       
        ["-l", "2d", input, iterations] -> run2d input iterations
        ["-l", "2d-r", input, iterations] -> run2dReadable input iterations 
        ["-l", "3d", input, iterations] -> run3d input iterations
        ["large", input, iter, repeats] -> runLarge input iter repeats
        ["-h"] -> printHelp
        [] -> printHelp
        _ -> putStrLn "Error: Invalid arguments" >> printHelp
        

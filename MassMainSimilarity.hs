module MassMainSimilarity where
import System.Environment
import HPModel
import Folder
import Similarity
import qualified Data.Vector as V
import Chain

main :: IO ()
main = do
    (its:_) <- getArgs
    input <- getContents
    mapM (f its) $ lines input
    return ()
    where
        f its input = do
            let residues = createResidues input
            res <- run residues (read its) :: IO C2d
            printHReadable (head res) (length res) $ V.fromList residues
            putStr $ printGraph $ buildConnectionMatrix residues (head res)

module PROT where

import System.IO
import Prelude hiding (concat)
import Data.Text (Text, chunksOf, pack, concat, unpack)
import qualified Data.Map as M

toProtein :: M.Map Text Text -> [Text] -> Text
toProtein translationTable codons = concat $ map (codonToProtein translationTable) codons

codonToProtein :: M.Map Text Text -> Text -> Text
codonToProtein translationTable codon = case (M.lookup codon translationTable) of
    Nothing -> error $ "Value for key " ++ (unpack codon) ++ " not in a map."
    Just p  -> p

toPair :: [String] -> (Text, Text)
toPair [from, "Stop"] = (pack from, pack "\n")
toPair [from, to] = (pack from, pack to)
toPair _ = (pack "", pack "error")

main = do
    handle <- openFile "rosalind_prot.txt" ReadMode
    firstLine <- hGetLine handle
    ttHandle <- openFile "util/codon_table.txt" ReadMode
    ttContents <- hGetContents ttHandle

    let ttLines = lines ttContents
        ttWords = map words ttLines
        ttPairs = map toPair ttWords
        translationTable = M.fromList ttPairs

    let packed = pack firstLine
        -- all codons are of length 3
        threes = chunksOf 3 packed
        translated = toProtein translationTable threes

    putStr $ unpack translated

    hClose handle
    hClose ttHandle

    return translated
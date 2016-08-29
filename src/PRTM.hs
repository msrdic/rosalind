module PRTM where

import qualified Data.Map  as M
import qualified Numeric   as N
import           System.IO

toPair :: [String] -> (Char, Float)
toPair [protein, weight] = (head protein, (fst . head) $ N.readFloat weight)
toPair _ = ('B', -1000000000)

getWeight :: M.Map Char Float -> Char -> Float
getWeight weights prot = case (M.lookup prot weights) of
    Nothing -> -100000000
    Just w -> w

main = do
    massHandle <- openFile "util/mass_table.txt" ReadMode
    massContents <- hGetContents massHandle

    let massLines = lines massContents
        massWords = map words massLines
        massTable = M.fromList $ map toPair massWords

    prtmHandle <- openFile "rosalind_prtm.txt" ReadMode
    prtmLine <- hGetLine prtmHandle

    print $ sum $ map (getWeight massTable) prtmLine

    hClose prtmHandle
    hClose massHandle

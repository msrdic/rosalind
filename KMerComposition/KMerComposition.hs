-- problem URL: http://rosalind.info/problems/kmer/
module KMerComposition where

import qualified Data.ByteString.Lazy       as BSL
import           Data.ByteString.Lazy.Char8 (unpack)
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Map.Strict            as M
import           Data.Maybe                 (fromMaybe)

realData :: FilePath
realData = "rosalind_kmer.txt"

exampleData :: FilePath
exampleData = "example.data"

process :: IO ()
process = do
  content <- BSL.readFile realData
  let result = processContent (C.filter dnaBase content)
  mapM_ (lookupAndPrint result) (sequence $ replicate 4 ['A', 'C', 'G', 'T'])
  putStrLn ""

lookupAndPrint :: M.Map String Int -> String -> IO ()
lookupAndPrint m key =
  putStr $ (++ " ") $ show $ fromMaybe 0 $ M.lookup key m

processContent :: BSL.ByteString -> M.Map String Int
processContent = processContent' M.empty

dnaBase :: Char -> Bool
dnaBase w = w == 'A' || w == 'C' || w == 'G' || w == 'T'

processContent' :: M.Map String Int -> BSL.ByteString -> M.Map String Int
processContent' counts string
  | BSL.length string < 4 = counts
  | otherwise = processContent' updatedMap updatedString
  where updatedMap = M.insertWith (+) (unpack $ BSL.take 4 string) 1 counts
        updatedString = BSL.drop 1 string

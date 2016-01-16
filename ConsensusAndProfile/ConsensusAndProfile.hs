-- problem URL: http://rosalind.info/problems/cons/
module ConsensusAndProfile where

import           Data.List  (findIndex, transpose)
import           Data.Maybe (fromMaybe)

exampleData :: String
exampleData = "example.data"

realData :: String
realData = "rosalind_cons.txt"

-- process the file
process = do
    content <- readFile realData
    let ls = lines content
        sections = extractStrings' ls
        (consensus, matrix) = calculate sections
    print consensus
    printMatrix $ transpose matrix

printMatrix :: [[Int]] -> IO ()
printMatrix [as, cs, gs, ts] = do
  printRow "A:" as
  printRow "C:" cs
  printRow "G:" gs
  printRow "T:" ts

printRow :: String -> [Int] -> IO ()
printRow prefix values = do
  putStr prefix
  mapM_ (putStr . (' ':) . show) $ values
  putStrLn ""

calculate :: [String] -> (String, [[Int]])
calculate strings = (consensus, matrix)
  where matrix = calculateMatrix strings
        consensus = calculateConsensus matrix

calculateMatrix :: [String] -> [[Int]]
calculateMatrix strings = map countBases $ transpose strings

countBases :: String -> [Int]
countBases s = countBases' [0, 0, 0, 0] s

countBases' counts [] = counts
countBases' [as, cs, gs, ts] (b:bs)
  | b == 'A'  = countBases' [as + 1, cs, gs, ts] bs
  | b == 'C'  = countBases' [as, cs + 1, gs, ts] bs
  | b == 'G'  = countBases' [as, cs, gs + 1, ts] bs
  | otherwise = countBases' [as, cs, gs, ts + 1] bs

calculateConsensus :: [[Int]] -> String
calculateConsensus matrix = calculateConsensus' maximums matrix
  where maximums = map maximum matrix

calculateConsensus' :: [Int] -> [[Int]] -> String
calculateConsensus' (m:ms) (r:rs) = indexToBase maxIndex : calculateConsensus' ms rs
  where maybeIndex = findIndex (\v -> m == v) r
        maxIndex = fromMaybe 5 maybeIndex
calculateConsensus' _ _ = ""

indexToBase :: Int -> Char
indexToBase 0 = 'A'
indexToBase 1 = 'C'
indexToBase 2 = 'G'
indexToBase 3 = 'T'

extractStrings' :: [String] -> [String]
extractStrings' [] = []
extractStrings' content = section : extractStrings' rest
  where (section, rest) = readSection content

readSection [] = ([], [])
readSection (header:content)
  | fastaHeader header = ((concat $ takeWhile (not . fastaHeader) content), dropWhile (not . fastaHeader) content)
  | otherwise = ([], [])

fastaHeader ('>':_) = True
fastaHeader _ = False

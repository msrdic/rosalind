-- problem URL: http://rosalind.info/problems/gc/
module GCContent where

exampleData :: String
exampleData = "example.data"

realData :: String
realData = "rosalind_gc.txt"

-- process the file
process = do
  content <- readFile realData
  let ls = lines content
      resultPair = processLines ls
  print $ snd resultPair
  print $ 100 * fst resultPair


processLines :: [String] -> (Double, String)
processLines [] = (0.0, "")
processLines (l:[]) = (0.0, tail l)
processLines ls = maxGC $ processLines' ls

maxGC :: [(Double, String)] -> (Double, String)
maxGC [] = (0.0, "")
maxGC gs = maximum gs

processLines' :: [String] -> [(Double, String)]
processLines' [] = []
processLines' (l:ls)
  | head l == '>' = processSection (tail l) ls : processLines' ls
  | otherwise = processLines' ls

processSection rid (l:ls) = (gcContent dna, rid)
  where dna      = concat dnaLines
        dnaLines = takeWhile (\line -> head line /= '>') (l:ls)

-- Calculate the GC content of the DNA string
gcContent :: String -> Double
gcContent "" = 0
gcContent dnaString = if gcCount == 0 then 0 else fromIntegral gcCount / fromIntegral (length dnaString)
  where gcCount = length $ filter gc dnaString
        gc base = base == 'C' || base == 'G'

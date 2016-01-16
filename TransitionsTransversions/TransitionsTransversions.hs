-- problem URL: http://rosalind.info/problems/tran/
module TransitionsTransversions where

exampleData :: String
exampleData = "example.data"

realData :: String
realData = "rosalind_tran.txt"

  -- process the file
process = do
    content <- readFile realData
    let result = fromIntegral transitions / fromIntegral transversions
        (transitions, transversions) = count s1 s2
        (s1, s2) = extractStrings content
    print s1
    print s2
    print transitions
    print transversions
    print result

extractStrings :: String -> (String, String)
extractStrings content = (s1, s2)
  where (s1:s2:_) = extractStrings' $ lines content

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

count :: String -> String -> (Int, Int)
count = count' (0, 0)

count' counts _ [] = counts
count' counts [] _ = counts
count' (c1, c2) (s1:s1s) (s2:s2s)
  | transition s1 s2 = count' (c1 + 1, c2) s1s s2s
  | transversion s1 s2 = count' (c1, c2 + 1) s1s s2s
  | otherwise = count' (c1, c2) s1s s2s

transition c1 c2 =
  (c1 == 'A' && c2 == 'G') ||
  (c1 == 'G' && c2 == 'A') ||
  (c1 == 'C' && c2 == 'T') ||
  (c1 == 'T' && c2 == 'C')
transversion c1 c2 =
  (c1 == 'C' && c2 == 'A') ||
  (c1 == 'A' && c2 == 'C') ||
  (c1 == 'A' && c2 == 'T') ||
  (c1 == 'T' && c2 == 'A') ||
  (c1 == 'G' && c2 == 'C') ||
  (c1 == 'C' && c2 == 'G') ||
  (c1 == 'G' && c2 == 'T') ||
  (c1 == 'T' && c2 == 'G')

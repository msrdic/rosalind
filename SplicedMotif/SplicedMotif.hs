-- seek for a single subsequence
module SplicedMotif where

sampleData :: String
sampleData = "sample.data"

realData :: String
realData = "rosalind_sseq.txt"

extractDNAStrings :: [String] -> (String, String)
extractDNAStrings dna = (first, second)
  where first = (concat . takeWhile (\l -> head l /= '>') . tail) dna
        second = (concat . tail . dropWhile (\l -> head l /= '>') . tail) dna

splicedMotif :: String -> String -> [Int]
splicedMotif = splicedMotif' 0 []

splicedMotif' :: Int -> [Int] -> String -> String -> [Int]
-- if no more nucleotides left in second string, return
splicedMotif' _ intermediate _ [] = intermediate
-- if the first dna string has been traversed, but there's more in the second
-- then it means there's no solution
splicedMotif' _ _ [] (_:_) = []
splicedMotif' index intermediate (f:fs) (s:ss) | f == s = splicedMotif' (index + 1) (index:intermediate) fs ss
                                               | otherwise = splicedMotif' (index + 1) intermediate fs (s:ss)

main = do
  content <- readFile realData
  let ls = lines content
  let (f, s) = extractDNAStrings ls
  -- result is a list of indices in a reverse order, 0 based
  let result = splicedMotif f s
  let indices = (map (+1) . reverse) result
  mapM_ (putStr . (++ " ") . show) indices
  putStrLn ""

-- problem URL: http://rosalind.info/problems/grph/
module OverlapGraphs where

import           Data.List (nub)

exampleData :: String
exampleData = "sample.data"

realData :: String
realData = "rosalind_grph.txt"

-- process the file
process = do
  content <- readFile realData
  let ls = lines content
      result = map showRes $ nub $ filter (\o -> o /= ("","")) $ overlap $ toO3 $ toTriples $ preprocessLines ls
  mapM_ print $ result

showRes :: (String, String) -> String
showRes (f, s) = f ++ " " ++ s

overlap :: [[String]] -> [(String, String)]
overlap triples = overlap' (length triples) triples

overlap' :: Int -> [[String]] -> [(String, String)]
overlap' count (t:triples)
  | count == 0 = []
  | otherwise = (checkOverlap t triples) ++ (overlap' (count - 1) (triples ++ [t]))

checkOverlap :: [String] -> [[String]] -> [(String, String)]
checkOverlap triple triples = checkPrefix triple triples ++ checkSuffix triple triples
checkPrefix triple triples = map (\t -> if (triple !! 1) == (t !! 2) then ((t !! 0), (triple !! 0)) else ("","")) triples
checkSuffix triple triples = map (\t -> if (triple !! 2) == (t !! 1) then ((triple !! 0), (t !! 0)) else ("","")) triples

toO3 :: [[String]] -> [[String]]
toO3 triples = map toO3' triples

toO3' :: [String] -> [String]
toO3' (l1:l2:l3:[]) = [l1, take 3 l2, reverse $ take 3 $ reverse l3]
to03' _ = []

-- trim sections
toTriples :: [[String]] -> [[String]]
toTriples sections = map toTriple sections

toTriple :: [String] -> [String]
toTriple [] = []
toTriple (l:[]) = []
toTriple (l1:l2:[]) = [l1, l2, l2]
toTriple (l1:l2:ls) = [l1, l2, last ls]

-- lines -> sections
preprocessLines :: [String] -> [[String]]
preprocessLines [] = []
-- the first line is an ID
preprocessLines (l:ls) = [(tail l) : takeWhile (\line -> head line /= '>') ls] ++ preprocessLines (dropWhile (\line -> head line /= '>') ls)

-- problem URL: http://rosalind.info/problems/mrna/
module MRNAFromProtein where

process :: String -> Int
process string = foldr1 mulModMillion $ toIntString string

mulModMillion :: Int -> Int -> Int
mulModMillion f s = (f * s) `mod` 1000000

toIntString :: String -> [Int]
toIntString proteinString = map mrna (proteinString ++ "-")

mrna :: Char -> Int
mrna 'F' = 2
mrna 'L' = 6
mrna 'I' = 3
mrna 'V' = 4
mrna 'G' = 4
mrna 'E' = 2
mrna 'D' = 2
mrna 'A' = 4
mrna 'S' = 6
mrna 'K' = 2
mrna 'N' = 2
mrna 'M' = 1
mrna 'R' = 6
mrna 'Q' = 2
mrna 'H' = 2
mrna 'Y' = 2
mrna 'W' = 1
mrna 'C' = 2
mrna 'T' = 4
mrna 'P' = 4
mrna '-' = 3
mrna _ = 1

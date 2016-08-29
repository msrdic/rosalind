module HAMM where

-- | Problem


-- Figure 2. The Hamming distance between these two
-- strings is 7. Mismatched symbols are colored red.

-- Given two strings s and t of equal length, the
-- Hamming distance between s and t, denoted dH(s,t),
-- is the number of corresponding symbols that differ
-- in s and t. See Figure 2.

-- Given: Two DNA strings s and t of equal length
-- (not exceeding 1 kbp).

-- Return: The Hamming distance dH(s,t).

-- Sample Dataset

-- GAGCCTACTAACGGGAT
-- CATCGTAATGACGGCCT

-- Sample Output

-- 7

import           System.IO

baseDiff b1 b2
    | compare b1 b2 == EQ   = 0
    | otherwise             = 1

hamming :: String -> String -> Int
hamming s t = sum $ zipWith baseDiff s t

main = do
    handle <- openFile "rosalind_hamm.txt" ReadMode
    contents <- hGetContents handle
    let [s, t] = lines contents
    return $ hamming s t

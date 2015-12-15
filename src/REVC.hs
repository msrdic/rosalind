module REVC where

-- | Problem

-- In DNA strings, symbols 'A' and 'T' are complements of
-- each other, as are 'C' and 'G'.

-- The reverse complement of a DNA string s is the string
-- sc formed by reversing the symbols of s, then taking
-- the complement of each symbol (e.g., the reverse
-- complement of "GTCA" is "TGAC").

-- Given: A DNA string s of length at most 1000 bp.

-- Return: The reverse complement sc of s.

-- Sample Dataset

-- AAAACCCGGT

-- Sample Output

-- ACCGGGTTTT

import System.IO

baseComplement :: Char -> Char
baseComplement 'A' = 'T'
baseComplement 'T' = 'A'
baseComplement 'G' = 'C'
baseComplement 'C' = 'G'
baseComeplement _ = error "Invalid input"

main = do
    handle <- openFile "rosalind_revc.txt" ReadMode
    contents <- hGetContents handle
    return $ reverse $ map baseComplement contents
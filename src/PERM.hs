module PERM where

-- | Problem

-- A permutation of length n is an ordering of the
-- positive integers {1,2,…,n}. For example,
-- π=(5,3,2,1,4) is a permutation of length 5.

-- Given: A positive integer n≤7.

-- Return: The total number of permutations of length
-- n, followed by a list of all such permutations
-- (in any order).

-- Sample Dataset

-- 3

-- Sample Output

-- 6
-- 1 2 3
-- 1 3 2
-- 2 1 3
-- 2 3 1
-- 3 1 2
-- 3 2 1

import           Data.List
import           System.IO

showIntList :: [Int] -> String
showIntList [] = ""
showIntList [i] = show i
showIntList (i:is) = show i ++ " " ++ showIntList is

main = withFile "rosalind_perm.txt" ReadMode (\handle -> do
    contents <- hGetContents handle
    let (l:_) = lines contents
        n = read l :: Int
        perms = permutations [1..n]
    putStrLn $ show $ length perms
    mapM_ (putStrLn.showIntList) perms)

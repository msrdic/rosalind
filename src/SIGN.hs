module SIGN where

-- | Problem

-- A signed permutation of length n is some ordering of
-- the positive integers {1,2,…,n} in which each integer
-- is then provided with either a positive or negative
-- sign (for the sake of simplicity, we omit the positive
-- sign). For example, π=(5,−3,−2,1,4) is a signed
-- permutation of length 5.

-- Given: A positive integer n≤6.

-- Return: The total number of signed permutations of length n,
-- followed by a list of all such permutations (you may list
-- the signed permutations in any order).

-- Sample Dataset

-- 2

--Sample Output

-- 8
-- -1 -2
-- -1 2
-- 1 -2
-- 1 2
-- -2 -1
-- -2 1
-- 2 -1
-- 2 1

import           Data.List
import           System.IO

showPerm :: [Int] -> String
showPerm [] = ""
showPerm (x:xs) = show x ++ " " ++ showPerm xs

signedPerms :: Int -> [[Int]]
signedPerms size = signed $ permutations [1..size]

signed :: [[Int]] -> [[Int]]
signed = undefined

main = do
    handle <- openFile "rosalind_sign.txt" ReadMode
    hout   <- openFile "sign_out.txt" WriteMode
    contents <- hGetContents handle
    let ls = lines contents
        count = (read $ head ls) :: Int
        perms = signedPerms count
    -- mapM_ ((hPutStrLn hout).showPerm) perms
    mapM_ (putStrLn.showPerm) perms
    hClose handle
    hClose hout

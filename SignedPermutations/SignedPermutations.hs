-- problem URL: http://rosalind.info/problems/sign/
module SignedPermutations where

import           Data.List (permutations)

n :: Int
n = 5

process = do
  let result = signedPermutations $ permutations [1..n]
  print $ permutations [1..n]
  print $ length result
  mapM_ printPermutation result

printPermutation :: [Int] -> IO ()
printPermutation perm =
  mapM_ (putStr . (++ " ") . show) perm >> putStrLn ""

signedPermutations :: [[Int]] -> [[Int]]
signedPermutations = concatMap signPermutation

signPermutation :: [Int] -> [[Int]]
signPermutation perm = map (signPermutation' perm) [0 .. (2 ^ n) - 1]

signPermutation' :: [Int] -> Int -> [Int]
signPermutation' perm signs = zipWith (*) perm bits
  where bits = reverse $ toBits n signs

toBits :: Int -> Int -> [Int]
toBits size signs = bits ++ replicate times 1
  where bits = toBits' signs
        times = size - length bits

toBits' :: Int -> [Int]
toBits' 0 = []
toBits' a = (bit a) : toBits' (a `div` 2)

bit a
  | a `mod` 2 == 1 = -1
  | otherwise = 1

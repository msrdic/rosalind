-- problem URL: http://rosalind.info/problems/fib/
module FibPairs where

rabbits generation k = head $ offspring generation k

offspring :: Integer -> Integer -> [Integer]
offspring 1 _ = [1]
offspring 2 _ = [1, 1]
offspring generation k = mature * k + immature : previous
  where previous = offspring (generation - 1) k
        mature = previous !! 1
        immature = head previous

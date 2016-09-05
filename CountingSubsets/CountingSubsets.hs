module CountingSubsets where

countSupersetsModulo :: Int -> Int -> Int
countSupersetsModulo = pow 2

pow :: Int -> Int -> Int -> Int
pow _ 0 _ = 1
pow base 1 _ = base
pow base e m = (pow base (e - 1) m * base) `mod` m

main = print $ countSupersetsModulo 930 1000000

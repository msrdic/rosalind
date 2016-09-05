module FibonacciNumbers where

fibs :: [Integer]
fibs = 0 : 1 : nextFib fibs

nextFib :: [Integer] -> [Integer]
nextFib (f:s:ns) = f + s : nextFib (s:ns)

fib :: Int -> Integer
fib n = fibs !! n

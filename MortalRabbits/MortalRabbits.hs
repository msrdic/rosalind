-- problem URL: http://rosalind.info/problems/fibd/
module MortalRabbits where

alive generations lifespan = kill lifespan $ offspring generations 1 lifespan

kill :: Int -> [Integer] -> [Integer]
kill lifespan rabbits
  | lifespan > length rabbits = rabbits
  | otherwise = aliveR ++ zeroes
  where aliveR = take aliveGenerations rabbits
        zeroes = replicate deadGenerations 0
        aliveGenerations = (length rabbits) - lifespan
        deadGenerations = lifespan

offspring :: Integer -> Integer -> Int -> [Integer]
offspring 1 _ lifespan = kill lifespan [1]
offspring 2 _ lifespan = kill lifespan [1, 1]
offspring generation k lifespan = mature * k + immature : kill lifespan previous
      where previous = offspring (generation - 1) k lifespan
            mature = previous !! 1
            immature = head previous

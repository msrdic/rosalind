module BinarySearch where

import           Data.Vector (Vector, fromList, (!))
import           Debug.Trace

sampleData :: String
sampleData = "sample.data"

realData :: String
realData = "rosalind_bins.txt"

binarySearch :: Vector Int -> Int -> Int -> Int
binarySearch vect = binarySearch' vect 0

binarySearch' :: Vector Int -> Int -> Int -> Int -> Int
binarySearch' vect lower upper value | lower == upper && vect ! lower == value = lower
                                     | lower == upper && vect ! lower /= value = -2
                                     | otherwise = checkAndContinue vect lower upper value

checkAndContinue :: Vector Int -> Int -> Int -> Int -> Int
checkAndContinue vect lower upper value | midElem == value = midIndex
                                        | midElem > value = binarySearch' vect lower midIndex value
                                        | otherwise = binarySearch' vect (midIndex + 1) upper value
  where midIndex = lower + ((upper - lower) `div` 2)
        midElem = vect ! midIndex


main = do
  content <- readFile realData
  let ls = lines content
  let arrayLen = (read . head) ls :: Int
  let elemLen = (read . head . tail) ls :: Int
  let sortedArray = (map read . words . head . tail . tail) ls :: [Int]
  let elemArray = (map read . words . head . tail . tail . tail) ls :: [Int]

  let vect = fromList sortedArray
  -- indices are 0-based, so map a +1 to each
  mapM_ (putStr . (++ " ") . show . (+1) . binarySearch vect arrayLen) elemArray

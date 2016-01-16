-- problem URL: http://rosalind.info/problems/pper/
module PartialPermutations where

process :: IO ()
process = do
  let lowerTerms' = lowerTerms 8
      upperTerms' = upperTerms 80 8
      lowerTerms'' = foldr1 (*) lowerTerms'
      upperTerms'' = foldr1 (*) upperTerms'
      result = upperTerms'' `div` lowerTerms'' `mod` 1000000
  print $ show lowerTerms'
  print $ show upperTerms'
  print $ show lowerTerms''
  print $ show upperTerms''
  print $ result

lowerTerms :: Integer -> [Integer]
lowerTerms k = [1 .. k]

upperTerms :: Integer -> Integer -> [Integer]
upperTerms n k = map (upperTerm n) [1,2 .. k]

upperTerm :: Integer -> Integer -> Integer
upperTerm n i = n + 1 - i

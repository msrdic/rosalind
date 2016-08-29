-- problem URL: http://rosalind.info/problems/lexv/
module VariableLexKMers where

  import           Data.List


  -- type input here from the file
  -- S L O G N Z V K H P
  data Alpha = S | L | O | G | N | Z | V | K | H | P
      deriving (Eq, Show, Ord, Bounded, Enum)

  newtype AWord a = AWord { alphas :: [a] }
      deriving (Eq, Ord)

  instance (Show a) => Show (AWord a) where
      show (AWord []) = ""
      show (AWord (a:as)) = show a ++ show (AWord as)

  instance Monoid (AWord a) where
      mempty = AWord []
      mappend (AWord []) _ = AWord []
      mappend _ (AWord []) = AWord []
      mappend (AWord alphas1) (AWord alphas2) = AWord (alphas1 ++ alphas2)

  wordLength :: Int
  wordLength = 4

  letters = [minBound :: Alpha .. maxBound :: Alpha]
  oneLetterWords = map AWord $ group letters

  generateWordsUpTo :: Int -> [AWord Alpha]
  generateWordsUpTo 0 = []
  generateWordsUpTo len = generateWords len ++ generateWordsUpTo (len - 1)

  generateWords :: Int -> [AWord Alpha]
  generateWords 0 = []
  generateWords length = generateWords' oneLetterWords (length - 1)

  generateWords' ws 0 = ws
  generateWords' ws length = generateWords' (mappend <$> ws <*> oneLetterWords) (length - 1)

  main = do
      let words = sort $ generateWordsUpTo wordLength
      mapM_ (putStrLn.show) words
      putStrLn $ show $ length words

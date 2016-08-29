module RestrictionSites where

data DNABase = A | T | G | C | N
     deriving (Eq, Show)
type DNAString = [DNABase]

c2b :: Char -> DNABase
c2b 'A' = A
c2b 'C' = C
c2b 'G' = G
c2b 'T' = T
c2b _   = N

s2dna :: String -> DNAString
s2dna = map c2b

baseComplement :: DNABase -> DNABase
baseComplement A = T
baseComplement T = A
baseComplement G = C
baseComplement C = G
baseComplement N = N

dnaComplement :: DNAString -> DNAString
dnaComplement = map baseComplement

reverseDNAComplement :: DNAString -> DNAString
reverseDNAComplement = reverse . dnaComplement

chainMinLen, chainMaxLen :: Int
chainMinLen = 4
chainMaxLen = 12

data Candidate = Start
               | Candidate { start   :: Int -- absolute start index
                           , len     :: Int -- length of segment
                           , segment :: DNAString -- segment itself
                           }
               | End
               deriving (Show, Eq)

nextCandidate :: DNAString -> Candidate -> Candidate
nextCandidate dnaString Start = Candidate 0 chainMinLen (take chainMinLen dnaString)
nextCandidate _ End = End
nextCandidate dnaString (Candidate st l _)
  | l == chainMaxLen = Candidate (st + 1) chainMinLen ((take chainMinLen . drop (st + 1)) dnaString)
  | st + chainMinLen >= length dnaString = End
  | st + l >= length dnaString = Candidate (st + 1) chainMinLen ((take chainMinLen . drop (st + 1)) dnaString)
  | otherwise = Candidate st (l + 2) ((take (l + 2) . drop st) dnaString)

isPalindromic :: Candidate -> Bool
isPalindromic (Candidate _ _ seg) = seg == reverseDNAComplement seg
isPalindromic Start = False
isPalindromic End = False

startAndLen :: Candidate -> (Int, Int)
startAndLen (Candidate s l _) = (s+1, l)
startAndLen _ = (-1, -1)

printResult :: (Int, Int) -> IO ()
printResult (s, l) = do
  putStr $ show s
  putStr " "
  print l

sampleData :: String
sampleData = "sample.data"

realData :: String
realData = "rosalind_revp.txt"

main = do
  content <- readFile realData
  let dna = concat $ lines content
  let chain = s2dna dna
  let notEnd candidate = candidate /= End
  let candidates = takeWhile notEnd $ iterate (nextCandidate chain) Start
  let palindromic = filter isPalindromic candidates
  let positions = map startAndLen palindromic
  mapM_ printResult positions

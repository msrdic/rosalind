-- problem URL: http://rosalind.info/problems/orf/
module OpenReadingFrames where

import           Data.List (inits, isPrefixOf, isSuffixOf)

sampleData :: String
sampleData = "sample.data"

realData :: String
realData = ""

process = do
  content <- readFile sampleData
  let ls = lines content
      fasta = concat $ tail ls
      reverseFasta = reverse $ complement fasta
      orfs = findORFs fasta
      reverseOrfs = findORFs reverseFasta
      result = proteins (orfs ++ reverseOrfs)
  print fasta
  print reverseFasta
  print orfs
  print reverseOrfs
  print result

findORFs :: String -> [String]
findORFs fasta = findORFs' [] $ toRNA fasta

findORFs' orfs [] = orfs
findORFs' orfs rna@(_:bs)
  | "AUG" `isPrefixOf` rna = findORFs' (orf : orfs) rest
  | otherwise = findORFs' orfs bs
  where (orf, rest) = findORF rna

findORF :: String -> (String, String)
findORF rna = (orf, rest)
  where orf = findORF' $ inits rna
        rest = drop (length orf) rna

findORF' :: [String] -> String
findORF' [] = ""
findORF' (c:cs)
  | hasStopCodon c = c
  | otherwise = findORF' cs

-- UAA, UAG, UGA
hasStopCodon :: String -> Bool
hasStopCodon s = "UAA" `isSuffixOf` s || "UAG" `isSuffixOf` s || "UGA" `isSuffixOf` s

complement :: String -> String
complement = map baseComplement
baseComplement b
  | b == 'A' = 'T'
  | b == 'T' = 'A'
  | b == 'G' = 'C'
  | b == 'C' = 'G'

proteins :: [String] -> [String]
proteins rnaCandidates = map toProteinPartial rnaCandidates

toRNA :: String -> String
toRNA = map (\b -> if b == 'T' then 'U' else b)

toProteinPartial :: String -> String
toProteinPartial = toProteinPartial' []

toProteinPartial' :: String -> String -> String
toProteinPartial' acc rna
  | null rna = acc
  | length rna < 3 = acc
  | protein == 'M' = toProteinPartial' [protein] rest
  | protein == '-' = acc
  | otherwise = toProteinPartial' (acc ++ [protein]) rest
  where protein = toProtein (take 3 rna)
        rest = drop 3 rna

toProtein :: String -> Char
toProtein str
  | str == "UUU" = 'F'
  | str == "UUC" = 'F'
  | str == "UUA" = 'L'
  | str == "UUG" = 'L'
  | str == "UCU" = 'S'
  | str == "UCC" = 'S'
  | str == "UCA" = 'S'
  | str == "UCG" = 'S'
  | str == "UAU" = 'Y'
  | str == "UAC" = 'Y'
  | str == "UAA" = '-'
  | str == "UAG" = '-'
  | str == "UGU" = 'C'
  | str == "UGC" = 'C'
  | str == "UGA" = '-'
  | str == "UGG" = 'W'
  | str == "CUU" = 'L'
  | str == "CUC" = 'L'
  | str == "CUA" = 'L'
  | str == "CUG" = 'L'
  | str == "CCU" = 'P'
  | str == "CCC" = 'P'
  | str == "CCA" = 'P'
  | str == "CCG" = 'P'
  | str == "CAU" = 'H'
  | str == "CAC" = 'H'
  | str == "CAA" = 'Q'
  | str == "CAG" = 'Q'
  | str == "CGU" = 'R'
  | str == "CGC" = 'R'
  | str == "CGA" = 'R'
  | str == "CGG" = 'R'
  | str == "AUU" = 'I'
  | str == "AUC" = 'I'
  | str == "AUA" = 'I'
  | str == "AUG" = 'M'
  | str == "ACU" = 'T'
  | str == "ACC" = 'T'
  | str == "ACA" = 'T'
  | str == "ACG" = 'T'
  | str == "AAU" = 'N'
  | str == "AAC" = 'N'
  | str == "AAA" = 'K'
  | str == "AAG" = 'K'
  | str == "AGU" = 'S'
  | str == "AGC" = 'S'
  | str == "AGA" = 'R'
  | str == "AGG" = 'R'
  | str == "GUU" = 'V'
  | str == "GUC" = 'V'
  | str == "GUA" = 'V'
  | str == "GUG" = 'V'
  | str == "GCU" = 'A'
  | str == "GCC" = 'A'
  | str == "GCA" = 'A'
  | str == "GCG" = 'A'
  | str == "GAU" = 'D'
  | str == "GAC" = 'D'
  | str == "GAA" = 'E'
  | str == "GAG" = 'E'
  | str == "GGU" = 'G'
  | str == "GGC" = 'G'
  | str == "GGA" = 'G'
  | str == "GGG" = 'G'

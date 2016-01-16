-- problem URL: http://rosalind.info/problems/splc/
{-# LANGUAGE OverloadedStrings #-}
module RNASplicing where

import qualified Data.Text as T

{-
  UUU F      CUU L      AUU I      GUU V
  UUC F      CUC L      AUC I      GUC V
  UUA L      CUA L      AUA I      GUA V
  UUG L      CUG L      AUG M      GUG V
  UCU S      CCU P      ACU T      GCU A
  UCC S      CCC P      ACC T      GCC A
  UCA S      CCA P      ACA T      GCA A
  UCG S      CCG P      ACG T      GCG A
  UAU Y      CAU H      AAU N      GAU D
  UAC Y      CAC H      AAC N      GAC D
  UAA Stop   CAA Q      AAA K      GAA E
  UAG Stop   CAG Q      AAG K      GAG E
  UGU C      CGU R      AGU S      GGU G
  UGC C      CGC R      AGC S      GGC G
  UGA Stop   CGA R      AGA R      GGA G
  UGG W      CGG R      AGG R      GGG G
-}

sampleData :: String
sampleData = "sample.data"

realData :: String
realData = "rosalind_splc.txt"

process = do
  content <- readFile realData
  let ls = lines content
      result = processLines ls
  print result

processLines :: [String] -> T.Text
processLines lines = translate $ removeIntrons $ map toRNA $ sectionsToStrings $ preprocessLines lines

translate :: T.Text -> T.Text
translate rna = T.pack $ map toProtein $ map T.unpack $ T.chunksOf 3 rna

removeIntrons :: [T.Text] -> T.Text
-- head is original RNA string, tail are introns
removeIntrons [] = T.pack ""
removeIntrons (s:[]) = s
removeIntrons (s:ss) = removeIntrons' s ss

removeIntrons' :: T.Text -> [T.Text] -> T.Text
removeIntrons' rna [] = rna
removeIntrons' rna (i:introns) = removeIntrons' (T.concat $ T.splitOn i rna) introns

toRNA :: T.Text -> T.Text
toRNA t = T.pack $ map (\b -> if b == 'T' then 'U' else b) $ T.unpack t

sectionsToStrings :: [[T.Text]] -> [T.Text]
sectionsToStrings = map T.concat

-- lines -> sections
preprocessLines :: [String] -> [[T.Text]]
preprocessLines [] = []
-- the first line is an ID
preprocessLines (l:ls) = map (map T.pack) [takeWhile (\line -> head line /= '>') ls] ++ preprocessLines (dropWhile (\line -> head line /= '>') ls)

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

-- well, this took some time.

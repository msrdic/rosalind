module ProteinMotif where

import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Lazy.Char8 as C8
import           Network.HTTP.Client

motifIDs :: [String]
-- motifIDs = ["A2Z669", "B5ZC00", "P07204_TRBM_HUMAN", "P20840_SAG1_YEAST"]
motifIDs = ["Q07287_ZPB_PIG",
 "Q9V730",
 "Q13VE3",
 "A9QYN2",
 "P02790_HEMO_HUMAN",
 "A4SQX2",
 "P81428_FA10_TROCA",
 "P02725_GLP_PIG",
 "P21809_PGS1_BOVIN",
 "P00749_UROK_HUMAN",
 "P07585_PGS2_HUMAN",
 "P42098_ZP3_PIG",
 "P13473_LMP2_HUMAN",
 "Q81QB7" ]

toUniprotURL id = "http://www.uniprot.org/uniprot/" ++ id ++ ".fasta"

data Candidate = Candidate { start :: Int
                           , str   :: String
                           } deriving (Eq, Show)

-- N-glycosylation motif
-- N{P}[ST]{P}
-- N, then anything but P, then S or T, then anything but P
nglycoMotif :: Candidate -> Bool
nglycoMotif (Candidate s [first, second, third, fourth]) =
  and [first == 'N', second /= 'P', third == 'S' || third == 'T', fourth /= 'P']

generateCandidates :: String -> [Candidate]
generateCandidates = generateCandidates' 1 4

generateCandidates' pos len prot | length prot >= len = Candidate pos (take len prot) : generateCandidates' (pos+1) len (tail prot)
                                 | otherwise = []

checkProtForMotifs :: Manager -> String -> IO ()
checkProtForMotifs manager motifId = do
  request <- parseUrl $ toUniprotURL motifId
  response <- httpLbs request manager
  let body = responseBody response
  let prot = (C8.filter (/= '\n') . C8.tail . C8.dropWhile (/= '\n')) body
  let candidates = generateCandidates $ C8.unpack prot
  let matched = filter nglycoMotif candidates
  if null matched
    then return ()
    else printMotifs motifId matched

printMotifs :: String -> [Candidate] -> IO ()
printMotifs motifId matched = do
  putStrLn motifId
  mapM_ (putStr . (++ " ") . show . start) matched
  putStrLn ""

main = do
  manager <- newManager defaultManagerSettings
  results <- mapM (checkProtForMotifs manager) motifIDs
  return ()

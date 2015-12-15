{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module LEXF where

-- | Problem

-- Assume that an alphabet 𝒜 has a predetermined order;
-- that is, we write the alphabet as a permutation
-- 𝒜=(a1,a2,…,ak), where a1<a2<⋯<ak. For instance,
-- the English alphabet is organized as (A,B,…,Z).

-- Given two strings s and t having the same length n,
-- we say that s precedes t in the lexicographic order
-- (and write s<Lext) if the first symbol s[j] that
-- doesn't match t[j] satisfies sj<tj in 𝒜.

-- Given: A collection of at most 10 symbols defining
-- an ordered alphabet, and a positive integer n (n≤10).

-- Return: All strings of length n that can be formed
-- from the alphabet, ordered lexicographically.

-- Sample Dataset

-- T A G C
-- 2

-- Sample Output

-- TT
-- TA
-- TG
-- TC
-- AT
-- AA
-- AG
-- AC
-- GT
-- GA
-- GG
-- GC
-- CT
-- CA
-- CG
-- CC

import System.IO
import Data.List
import Data.Monoid
import Control.Applicative

-- type input here from the file
data Alpha = U | Q | I | Z | N | S | R | W | E | Y
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
wordLength = 3

letters = [minBound :: Alpha .. maxBound :: Alpha]
oneLetterWords = map AWord $ group letters

generateWords :: Int -> [AWord Alpha]
generateWords 0 = []
generateWords length = generateWords' oneLetterWords (length - 1)

generateWords' ws 0 = ws
generateWords' ws length = generateWords' (mappend <$> ws <*> oneLetterWords) (length - 1)

main = do
    handle <- openFile "lexf_out.txt" WriteMode -- results
    let words = generateWords wordLength
    -- mapM_ ((hPutStrLn handle).showWord) words
    mapM_ (putStrLn.show) words
    putStrLn $ show $ length words
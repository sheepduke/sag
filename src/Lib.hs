module Lib (readWordsSet, generateAbbreviations) where

import Data.Set (Set)
import qualified Data.Set as Set

combinations :: [a] -> [[a]]
combinations [] = error "Invalid condition"
combinations [x] = [[x]]
combinations (x : xs) =
  let restResult = combinations xs
   in [[x]] ++ restResult ++ map (x :) restResult

wordCombinationsAllowEmpty :: String -> [String]
wordCombinationsAllowEmpty word = [] : combinations word

wordCombinationsKeepingFirstChar :: String -> [String]
wordCombinationsKeepingFirstChar word = map (head word :) $ combinations $ tail word

wordCombinations :: String -> [String]
wordCombinations = combinations

linesToSet :: [String] -> Set String
linesToSet = Set.fromList

readWordsSet :: String -> IO (Set String)
readWordsSet fileName = do
  content <- readFile fileName
  return . linesToSet . lines $ content

generateAbbreviations :: String -> Set String -> [String]
generateAbbreviations expression wordsSet = dedup . filter (`Set.member` wordsSet) . map concat . mapM combinations . words $ expression
  where
    dedup = Set.toList . Set.fromList

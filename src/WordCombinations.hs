{-# LANGUAGE NamedFieldPuns #-}

module WordCombinations where

import qualified Data.List as List
import System.IO (IOMode (ReadMode, WriteMode), hPutStr, hPutStrLn, withFile)
import WordFilter (WordFilter)

combinations :: [a] -> [[a]]
combinations [] = error "Invalid condition"
combinations [x] = [[x]]
combinations (x : xs) =
  let restResult = combinations xs
   in [[x]] ++ restResult ++ map (x :) restResult

data WordCombinationPolicy = WordCombinationPolicy
  { minCharCountFromEachWord :: Int,
    maxCharCountFromEachWord :: Int,
    maxCombinationLength :: Int,
    mustTakeFirstChar :: Bool
  }
  deriving (Show)

defaultWordCombinationPolicy =
  WordCombinationPolicy
    { minCharCountFromEachWord = 1,
      maxCharCountFromEachWord = 3,
      maxCombinationLength = 5,
      mustTakeFirstChar = False
    }

generateWordCombinations :: [String] -> WordCombinationPolicy -> [String]
generateWordCombinations words policy =
  filter (\word -> length word <= maxCombinationLength policy) . map concat . mapM wordCombinations $ words
  where
    wordCombinations word =
      filter isWordLengthValid $
        if mustTakeFirstChar policy
          then [head word] : (map (head word :) . combinations $ tail word)
          else combinations word
    isWordLengthValid word =
      let listLength = length word
       in listLength >= minCharCountFromEachWord policy && listLength <= maxCharCountFromEachWord policy

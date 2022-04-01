module AbbrGen where

import qualified Data.Set as Set
import WordCombinations (WordCombinationsPolicy, generateWordCombinations)
import WordFilter

generateAbbreviations ::
  (WordFilter a) =>
  [String] ->
  WordCombinationsPolicy ->
  a ->
  [String]
generateAbbreviations words policy wordFilter = dedup . filter (isValidWord wordFilter) $ generateWordCombinations words policy
  where
    dedup = Set.toList . Set.fromList

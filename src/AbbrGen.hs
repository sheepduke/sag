module AbbrGen where

import qualified Data.Set as Set
import WordCombinations (WordCombinationPolicy, generateWordCombinations)
import WordFilter

generateAbbreviations :: [String] -> WordCombinationPolicy -> WordFilter -> [String]
generateAbbreviations words policy wordFilter =
  dedup . filter wordFilter $
    generateWordCombinations words policy
  where
    dedup = Set.toList . Set.fromList

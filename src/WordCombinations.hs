module WordCombinations where

import WordFilter (WordFilter)

combinations :: [a] -> [[a]]
combinations [] = error "Invalid condition"
combinations [x] = [[x]]
combinations (x : xs) =
  let restResult = combinations xs
   in [[x]] ++ restResult ++ map (x :) restResult

data WordCombinationsPolicy
  = MustTakeOneChar
  | MustKeepFirstChar
  | MayOmitWord

generateWordCombinations :: [String] -> WordCombinationsPolicy -> [String]
generateWordCombinations words policy =
  map concat . mapM wordCombinations $ words
  where
    wordCombinations word =
      let result = case policy of
            MustTakeOneChar -> combinations word
            MustKeepFirstChar -> map (head word :) $ combinations $ tail word
            MayOmitWord -> [] : combinations word
       in result

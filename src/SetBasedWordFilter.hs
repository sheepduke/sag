{-# LANGUAGE FlexibleInstances #-}

module SetBasedWordFilter where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import WordFilter

new :: [String] -> WordFilter
new validWords =
  let dict = Set.fromList validWords
   in (`Set.member` dict)

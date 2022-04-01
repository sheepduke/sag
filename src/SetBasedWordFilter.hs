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

-- newtype SetBasedWordFilter a = SetBasedWordFilter
--   { dict :: Set a
--   }

-- new :: (Ord a) => [a] -> SetBasedWordFilter a
-- new validWords =
--   SetBasedWordFilter
--     { dict = Set.fromList validWords
--     }

-- instance WordFilter (SetBasedWordFilter String) where
--   isValidWord wordFilter word = Set.member word $ dict wordFilter

-- instance WordFilter (SetBasedWordFilter Text) where
--   isValidWord wordFilter word = Set.member (Text.pack word) $ dict wordFilter

module WordFilter where

class WordFilter a where
  isValidWord :: a -> String -> Bool

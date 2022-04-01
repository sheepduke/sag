module AlwaysValidWordFilter where

import WordFilter

data AlwaysValidWordFilter = AlwaysValidWordFilter

instance WordFilter AlwaysValidWordFilter where
  isValidWord wordFilter word = True

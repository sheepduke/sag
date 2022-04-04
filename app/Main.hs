{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import AbbrGen
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import qualified EnglishDict
import qualified SetBasedWordFilter
import System.Console.CmdArgs
import WordCombinations
import WordFilter

data Sag = Sag
  { dict :: Maybe String,
    no_dict :: Bool,
    min_char_count :: Maybe Int,
    max_char_count :: Maybe Int,
    max_abbr_length :: Maybe Int,
    keep_first_char :: Bool,
    names :: [String]
  }
  deriving (Data, Show, Typeable)

cliArgDef =
  Sag
    { dict = def &= typ "DICT_FILE" &= help "Dictionary file, defaults to a built-in English dictionary",
      no_dict = def &= name "n" &= help "Ignore dictionary",
      min_char_count = def &= name "i" &= help "Minimum number of chars taken from each word",
      max_char_count = def &= name "a" &= help "Maximum number of chars taken from each word",
      max_abbr_length = def &= name "l" &= help "Maximum length of generated abbreviation",
      keep_first_char = def &= name "k" &= help "Keep first char of each word",
      names = def &= args &= typ "WORDS"
    }
    &= summary "sag - Sane Abbreviations Generator 1.0"
    &= help "Generate sane abbreviations against dictionary."
    &= details
      [ "Generate abbreviations against given dictionary file. It contains a built-in English dictionary that has 370,000 words."
      ]

main :: IO ()
main = do
  cliArgs <- cmdArgs cliArgDef
  wordFilter <- newWordFilter (dict cliArgs) (no_dict cliArgs)
  let result =
        generateAbbreviations
          (names cliArgs)
          WordCombinationPolicy
            { minCharCountFromEachWord = fromMaybe 1 $ min_char_count cliArgs,
              maxCharCountFromEachWord = fromMaybe 3 $ max_char_count cliArgs,
              maxCombinationLength = fromMaybe 5 $ max_abbr_length cliArgs,
              mustTakeFirstChar = keep_first_char cliArgs
            }
          wordFilter
  for_ result putStrLn
  where
    newWordFilter :: Maybe String -> Bool -> IO WordFilter
    newWordFilter _ True = do return $ pure True
    newWordFilter Nothing _ = do return $ SetBasedWordFilter.new EnglishDict.dict
    newWordFilter (Just dictFile) _ = SetBasedWordFilter.new . lines <$> readFile dictFile

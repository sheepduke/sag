{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import AbbrGen
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import qualified SetBasedWordFilter
import System.Console.CmdArgs
import qualified System.Console.CmdArgs as CmdArgs
import WordCombinations

data CliArg = CliArg
  { dict :: String,
    min_char_count :: Maybe Int,
    max_char_count :: Maybe Int,
    max_abbr_length :: Maybe Int,
    must_keep_first_char :: Bool,
    names :: [String],
    test_me :: String
  }
  deriving (Data, Show, Typeable)

cliArgDef =
  CliArg
    { dict = def &= typ "DICT_FILE" &= help "Dictionary file",
      min_char_count = def &= name "i" &= help "minimum number of chars taken from each word",
      max_char_count = def &= name "a" &= help "maximum number of chars taken from each word",
      max_abbr_length = def &= name "l" &= help "maximum length of generated abbreviation",
      must_keep_first_char = def &= name "k" &= help "keep first char of each word",
      test_me = def &= help "test" &= opt "hello",
      names = def &= args &= typ "WORDS"
    }
    &= summary "sag - Sane Abbreviations Generator"
    &= help "Generate sane abbreviations from against dictionary"
    &= details
      [ "By default, it takes one character from each word (of given name) and",
        "generates valid abbreviations that were defined in the dictionary file."
      ]

main :: IO ()
main = do
  cliArgs <- cmdArgs cliArgDef
  wordFilter <- newWordFilter $ dict cliArgs
  let result =
        generateAbbreviations
          (names cliArgs)
          WordCombinationPolicy
            { minCharCountFromEachWord = fromMaybe 1 $ min_char_count cliArgs,
              maxCharCountFromEachWord = fromMaybe 3 $ max_char_count cliArgs,
              maxCombinationLength = fromMaybe 5 $ max_abbr_length cliArgs,
              mustTakeFirstChar = must_keep_first_char cliArgs
            }
          wordFilter
  for_ result putStrLn
  where
    readDict dictFile = do
      content <- readFile dictFile
      return . lines $ content

    newWordFilter dictFile = case dictFile of
      [] -> do return $ pure True
      _ -> do
        content <- readFile dictFile
        return . SetBasedWordFilter.new . lines $ content

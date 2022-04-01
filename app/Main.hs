{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import AbbrGen
import AlwaysValidWordFilter
import Control.Monad
import Data.Foldable (for_)
import qualified Data.HashSet as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified SetBasedWordFilter
import System.Console.CmdArgs hiding (name)
import WordCombinations

data CliArg = CliArg
  { dict :: String,
    names :: [String]
  }
  deriving (Data, Show)

cliArgDef =
  CliArg
    { dict = def &= help "Dictionary file",
      names =
        def &= args &= typ "LONG_NAME"
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
  validWords <- readDict $ dict cliArgs
  let wordFilter = SetBasedWordFilter.new validWords
  let result = generateAbbreviations (names cliArgs) MustTakeOneChar wordFilter
  for_ result putStrLn
  where
    readDict dictFile = do
      content <- readFile dictFile
      return . lines $ content

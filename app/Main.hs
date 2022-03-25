{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.Foldable (for_)
import qualified Data.Foldable as Set
import Lib
import System.Console.CmdArgs hiding (name)

data CliArg = CliArg
  { dict :: String,
    name :: String
  }
  deriving (Data, Show)

cliArgDef =
  CliArg
    { dict = def &= help "Dictionary file",
      name =
        def &= argPos 0 &= typ "LONG_NAME"
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
  wordsSet <- readWordsSet $ dict cliArgs
  let result = generateAbbreviations (name cliArgs) wordsSet
  for_ result putStrLn

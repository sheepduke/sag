{-# LANGUAGE TemplateHaskell #-}

module EnglishDict where

import Data.FileEmbed (embedFile)
import Data.ByteString (ByteString)

import qualified Data.ByteString.Char8 as Char8

rawDictContent :: ByteString
rawDictContent = $(embedFile "resources/english_words.txt")

dict :: [String]
dict = lines $ Char8.unpack rawDictContent

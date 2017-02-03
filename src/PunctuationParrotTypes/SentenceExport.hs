{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveGeneric, RecordWildCards  #-}

module PunctuationParrotTypes.SentenceExport where 

import Data.Aeson as AE
import GHC.Generics as G
import PunctuationParrotTypes.DatabaseTypes
import qualified Data.Text as T

data SentenceExport = SentenceExport {
    text :: String,
    studentId :: Int,
    sentenceId :: Int,
    studentLevel :: Int
} deriving (Show, G.Generic)

instance ToJSON SentenceExport where
  toEncoding = genericToEncoding defaultOptions

sentenceToExport :: SentenceField -> Int -> Int -> SentenceExport
sentenceToExport (SentenceField sid sentenceText _ _ _ _) stId level = 
    SentenceExport {
        text = T.unpack sentenceText,
        studentId = stId,
        sentenceId = sid,
        studentLevel = level
    }
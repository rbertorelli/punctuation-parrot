{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveGeneric, RecordWildCards  #-}

module PunctuationParrotTypes.ResultsExport where 

import Data.Aeson as AE
import GHC.Generics as G
import PunctuationParrotTypes.DatabaseTypes
import qualified Data.Text as T

data ResultsExport = ResultsExport {
    text :: [String]
} deriving (Show, G.Generic)

instance ToJSON ResultsExport where
  toEncoding = genericToEncoding defaultOptions

{-
resultToExport :: ResultsField -> ResultsExport
resultToExport (ResultsExport errorType errorCount) = 
    ResultsExport {
        text = T.unpack ,
    }
-}

resultToExport :: [ResultsField] -> ResultsExport
resultToExport results =
    ResultsExport {
        text = map resultToString results
    }


resultToString :: ResultsField -> String
resultToString (ResultsField errorType errorCount) = (T.unpack errorType) ++ ": " ++ (show errorCount)
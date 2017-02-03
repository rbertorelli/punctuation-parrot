{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, RecordWildCards  #-}

module PunctuationParrotTypes.FrontEndPost where

import Data.Aeson as AE

data FrontEndPost = FrontEndPost {
    sentenceId :: Int,
    studentId :: Int,
    attemptStatus :: Bool,
    caps :: Int,
    period :: Int,
    quotes :: Int,
    spacing :: Int,
    misc :: Int,
    apostrophe :: Int
} deriving (Show)


instance FromJSON FrontEndPost where
    parseJSON = withObject "frontendpost" $ \o -> do
        sentenceId <- o .: "sentenceId"
        studentId <- o .: "studentId"
        attemptStatus <- o .: "attemptStatus"
        caps <- o .: "caps"
        period <- o .: "period"
        quotes <- o .: "quotes"
        spacing <- o .: "spacing"
        misc <- o .: "misc"
        apostrophe <- o .: "apostrophe"
        return FrontEndPost{..}
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module DatabaseAccess where

import PunctuationParrotTypes.DatabaseTypes
import PunctuationParrotTypes.SentenceExport
import qualified PunctuationParrotTypes.FrontEndPost as FPE
import Control.Applicative
import Data.Time
import qualified Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Control.Monad (forM_)
import System.Random

textFieldUnpack :: SingleTextField -> T.Text
textFieldUnpack (SingleTextField x) = x

intFieldUnpack :: SingleIntField -> Int
intFieldUnpack (SingleIntField x) = x :: Int


{-
So we have a Maybe FrontEndPost (Should this just be a FrontEndPost?)
Need to create a sentence attempt based off of this with the boolean value.
Get the id from this.

If any errors exist, we need to insert into student_sentence_errors

If everything succeeded, return IO 200, otherwise IO 500
-}

sentenceUpdate :: Maybe FPE.FrontEndPost -> IO Int
sentenceUpdate Nothing = return 500
sentenceUpdate (Just post) = do
  conn <- open "../db/db.sqlite"
  execute conn "INSERT INTO sentence_attempts (sentence_id, student_id, correct, created_at) \
              \VALUES (?, ?, ?, datetime())" [show (FPE.sentenceId post), show (FPE.studentId post), show (FPE.attemptStatus post)]
              -- The entire array needs to have the same type

  let errorCount = getErrorCount post
  if errorCount > 0 then do
    rowId <- lastInsertRowId conn

    -- need to look up types
    insertError (fromIntegral rowId) 1 (FPE.period post)
    insertError (fromIntegral rowId) 2 (FPE.caps post)
    insertError (fromIntegral rowId) 3 (FPE.apostrophe post)
    insertError (fromIntegral rowId) 4 (FPE.quotes post)
    insertError (fromIntegral rowId) 5 (FPE.misc post)
    insertError (fromIntegral rowId) 6 (FPE.spacing post)

    return 200
  else
    return 200


getErrorCount :: FPE.FrontEndPost -> Int
getErrorCount post = (FPE.caps post) + (FPE.period post) + (FPE.quotes post) + (FPE.spacing post) + (FPE.misc post) + (FPE.apostrophe post)

-- sentenceAttemptId, errorId, count
insertError :: Int -> Int -> Int -> IO ()
insertError attemptId errorId count
  | count <= 0 = return ()
  | otherwise = do
    conn <- open "../db/db.sqlite"
    execute conn "INSERT INTO student_sentence_errors (sentence_attempt_id, error_type_id) VALUES (?, ?)" [attemptId, errorId]
    insertError attemptId errorId (count - 1)


--fetchUserSentence :: Database.SQLite.Simple.ToField.ToField v => v -> IO SentenceField
fetchUserSentence studentId = do
  conn <- open "../db/db.sqlite"

  -- get level for student id
  studentLevelRows <- (queryNamed conn "SELECT \
      \max_level \
    \FROM \
      \students s \
      \INNER JOIN classroom_students sc ON s.id = sc.student_id \
      \INNER JOIN classrooms c ON sc.classroom_id = c.id \
    \WHERE \
      \s.id = :id" [":id" := studentId]) :: IO [SingleIntField]

  studentLevel <- getIntRowFromResults studentLevelRows 0

  -- get number of sentences completed
  sentenceCountResult <- (queryNamed conn "SELECT COUNT(*) from sentence_attempts where student_id = :id" [":id" := studentId]) :: IO [SingleIntField]
  sentenceCount <- getIntRowFromResults sentenceCountResult 0

  -- Pick a random sentence from those not done yet within max level
  rows <- (queryNamed conn "SELECT \
      \s.* \
    \FROM \
      \sentences s \
      \LEFT JOIN sentence_attempts sa ON s.id = sa.sentence_id AND sa.student_id = :studentId \
    \WHERE \
      \sa.id IS NULL \
      \AND \
      \s.level <= :level" [":studentId" := studentId, ":level" := (studentLevel::Int)]) :: IO [SentenceField]

  choice <- pickRandom rows
  return $ sentenceToExport choice studentId studentLevel


pickRandom :: [a] -> IO a
pickRandom xs = fmap (xs !!) $ randomRIO (0, length xs - 1)

getStringRowFromResults :: Monad m => [SingleTextField] -> Int -> m String
getStringRowFromResults rows x = 
  if (length rows > 0) then do
    let item = rows !! x
    return $ T.unpack (textFieldUnpack item)
  else
    return ""


getIntRowFromResults :: Monad m => [SingleIntField] -> Int -> m Int
getIntRowFromResults rows x = 
  if (length rows > 0) then do
    let item = rows !! x
    return $ intFieldUnpack item
  else
    return 0
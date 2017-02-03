{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Data.Time
import qualified Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Control.Monad.IO.Class (liftIO)
--import DatabaseTypes

main :: IO ()
main = do
  conn <- open "db.sqlite"
  execute_ conn "CREATE TABLE IF NOT EXISTS students (id INTEGER PRIMARY KEY, name TEXT, email TEXT UNIQUE, created_at DATETIME)"
  execute_ conn "CREATE TABLE IF NOT EXISTS teachers (id INTEGER PRIMARY KEY, name TEXT, email TEXT UNIQUE, created_at DATETIME)"
  execute_ conn "CREATE TABLE IF NOT EXISTS classroom_students (classroom_id INTEGER PRIMARY KEY, student_id INTEGER UNIQUE)"
  
  execute_ conn "CREATE TABLE IF NOT EXISTS sentences (id INTEGER PRIMARY KEY, sentence_text TEXT, status BOOL, teacher_id INTEGER, level INTEGER, created_at DATETIME)"
  execute_ conn "CREATE INDEX teacher_status_level ON sentences (teacher_id, status, level)"
  execute_ conn "CREATE INDEX level ON sentences (level)"
  
  execute_ conn "CREATE TABLE IF NOT EXISTS sentence_attempts (id INTEGER PRIMARY KEY, sentence_id INTEGER, modified_sentence TEXT, student_id INTEGER, correct BOOL, created_at DATETIME)"
  execute_ conn "CREATE INDEX sentence ON sentence_attempts (sentence_id)"
  execute_ conn "CREATE INDEX student ON sentence_attempts (student_id)"

  execute_ conn "CREATE TABLE IF NOT EXISTS classrooms (id INTEGER PRIMARY KEY, teacher_id INTEGER, max_level UNSIGNED, use_others_sentences BOOL)"
  execute_ conn "CREATE INDEX teacher ON classrooms (teacher_id)"

  execute_ conn "CREATE TABLE IF NOT EXISTS error_types (id INTEGER PRIMARY KEY, name TEXT)"

  execute_ conn "CREATE TABLE IF NOT EXISTS sentence_potential_errors (sentence_id INTEGER, error_type_id INTEGER, PRIMARY KEY (sentence_id, error_type_id))"

  execute_ conn "CREATE TABLE IF NOT EXISTS student_sentence_errors (id INTEGER PRIMARY KEY, sentence_attempt_id INTEGER, error_type_id INTEGER)"
  execute_ conn "CREATE INDEX sentence_attempt_id ON student_sentence_errors (sentence_attempt_id)"

  -- Base inserts
  execute conn "INSERT INTO teachers (name, email, created_at) VALUES (?, ?, date('now'))" ["Admin" :: String, "admin@punctuation-parrot.com" :: String]
  execute conn "INSERT INTO students (name, email, created_at) VALUES (?, ?, date('now'))" ["TestStudent" :: String, "test_student@punctuation-parrot.com" :: String]
  execute conn "INSERT INTO classrooms (teacher_id, max_level, use_others_sentences) VALUES (?, ?, ?)" ["1"::String, "4"::String, "1"::String]
  execute conn "INSERT INTO classroom_students (classroom_id, student_id) VALUES (?, ?)" ["1"::String, "1"::String]

  execute conn "INSERT INTO error_types (name) VALUES (?), (?), (?), (?), (?)" ["Period" :: String, "Capitals" :: String, "Apostrophes" :: String, "Quotes" :: String, "Miscellaneous Punctuation" :: String, "Spacing" :: String]

  -- Might want to have these added via manual insert
  execute conn "INSERT INTO sentences (sentence_text, status, teacher_id, level, created_at) VALUES (?, ?, ?, ?, date('now'))" ["\"How now, brown cow,\" said George. He was not happy that day."::String, "1"::String, "1"::String, "10"::String]
  execute conn "INSERT INTO sentences (sentence_text, status, teacher_id, level, created_at) VALUES (?, ?, ?, ?, date('now'))" ["The boy wished that he could fly."::String, "1"::String, "1"::String, "1"::String]
  execute conn "INSERT INTO sentences (sentence_text, status, teacher_id, level, created_at) VALUES (?, ?, ?, ?, date('now'))" ["After school we're going to play."::String, "1"::String, "1"::String, "2"::String]
  execute conn "INSERT INTO sentences (sentence_text, status, teacher_id, level, created_at) VALUES (?, ?, ?, ?, date('now'))" ["It's going to be a lovely day."::String, "1"::String, "1"::String, "2"::String]
  execute conn "INSERT INTO sentences (sentence_text, status, teacher_id, level, created_at) VALUES (?, ?, ?, ?, date('now'))" ["When are we going to eat? I'm hungry."::String, "1"::String, "1"::String, "3"::String]
  execute conn "INSERT INTO sentences (sentence_text, status, teacher_id, level, created_at) VALUES (?, ?, ?, ?, date('now'))" ["Good diet and exercise make for a healthy, strong person."::String, "1"::String, "1"::String, "2"::String]
  execute conn "INSERT INTO sentences (sentence_text, status, teacher_id, level, created_at) VALUES (?, ?, ?, ?, date('now'))" ["Mr. Jones came back from the store."::String, "1"::String, "1"::String, "1"::String]
  execute conn "INSERT INTO sentences (sentence_text, status, teacher_id, level, created_at) VALUES (?, ?, ?, ?, date('now'))" ["Do you swear to tell the truth, the whole truth, and nothing but the truth?"::String, "1"::String, "1"::String, "3"::String]

  close conn
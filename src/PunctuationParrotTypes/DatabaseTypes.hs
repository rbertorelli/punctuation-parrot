{-# LANGUAGE OverloadedStrings #-}

module PunctuationParrotTypes.DatabaseTypes where

import Control.Applicative
import Data.Time
import qualified Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow


data SingleTextField = SingleTextField T.Text deriving (Show)

data SingleIntField = SingleIntField Int deriving (Show)

-- id, name, email, created_at
data StudentField = StudentField Int T.Text T.Text T.Text deriving (Show)
data TeacherField = TeacherField Int T.Text T.Text T.Text deriving (Show)

-- teacher_id, student_id
data TeacherStudentField = TeacherStudentField Int Int deriving (Show)

-- id, sentence text, teacher_id, status, level, created_at
data SentenceField = SentenceField Int T.Text Int Int Int T.Text deriving (Show)

-- id, sentence_id, modified_sentence, student_id, correct, created_at
data SentenceAttemptField = SentenceAttemptField Int Int T.Text Int Int T.Text deriving (Show)


-- Single field Instances (mostly for select results)
instance FromRow SingleTextField where
  fromRow = SingleTextField <$> field

instance FromRow SingleIntField where
  fromRow = SingleIntField <$> field


-- Teacher Instances
instance FromRow TeacherField where
  fromRow = TeacherField <$> field <*> field <*> field <*> field

instance ToRow TeacherField where
  toRow (TeacherField id_ name email createdAt) = toRow (id_, name, email, createdAt)

-- Student instances
instance FromRow StudentField where
  fromRow = StudentField <$> field <*> field <*> field <*> field

instance ToRow StudentField where
  toRow (StudentField id_ name email createdAt) = toRow (id_, name, email, createdAt)

-- Sentence instances
instance FromRow SentenceField where
  fromRow = SentenceField <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow SentenceField where
  toRow (SentenceField id_ sentence_text status teacherId level createdAt) = toRow (id_, sentence_text, status, teacherId, level, createdAt)
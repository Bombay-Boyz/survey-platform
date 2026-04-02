{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}
module Model where

import qualified Data.Text as T
import Data.Text           (Text)
import Data.Time           (UTCTime)
import Text.Read           (readMaybe)
import Database.Persist.TH

import Types.Survey (Answer (..))

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
SurveyRecord sql=survey_record
  externalId  Text
  title       Text
  schemaJson  Text
  version     Int
  createdAt   UTCTime
  UniqueExternalSurveyId externalId
  deriving Show Eq

SubmissionRecord sql=submission_record
  surveyId     SurveyRecordId
  externalId   Text
  submittedAt  UTCTime
  respondentId Text Maybe
  UniqueExternalSubmissionId externalId
  deriving Show Eq

AnswerRecord sql=answer_record
  submissionId SubmissionRecordId
  questionId   Text
  answerType   Text
  answerValue  Text
  deriving Show Eq
|]

encodeAnswer :: Answer -> (Text, Text)
encodeAnswer (AText   t) = ("text",   t)
encodeAnswer (AChoice c) = ("choice", c)
encodeAnswer (ARating r) = ("rating", T.pack (show r))
encodeAnswer (ANumber n) = ("number", T.pack (show n))

decodeAnswer :: Text -> Text -> Maybe Answer
decodeAnswer "text"   v = Just (AText v)
decodeAnswer "choice" v = Just (AChoice v)
decodeAnswer "rating" v = ARating <$> readMaybe (T.unpack v)
decodeAnswer "number" v = ANumber <$> readMaybe (T.unpack v)
decodeAnswer _        _ = Nothing

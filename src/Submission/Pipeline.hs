{-# LANGUAGE OverloadedStrings #-}
module Submission.Pipeline
  ( SubmissionInput(..)
  , SubmissionOutput(..)
  , PipelineError(..)
  , buildSubmission
  , encodeAnswers
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text       as T
import Data.Map.Strict (Map)
import Data.Text       (Text)
import Data.Time       (UTCTime)

import Types.Core      (SurveyId(..), QuestionId(..))
import Types.Survey    (SubmissionAnswers(..))
import Model           (SubmissionRecordId, SubmissionRecord(..), AnswerRecord(..), encodeAnswer)
import Utils.Conversion (surveyIdToRecordId, placeholderSubmissionRecordId)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

data SubmissionInput = SubmissionInput
  { siSurveyDbId   :: SurveyId        -- Domain type
  , siExternalId   :: Text
  , siSubmittedAt  :: UTCTime
  , siRespondentId :: Maybe Text
  , siAnswers      :: SubmissionAnswers
  } deriving (Show)

data SubmissionOutput = SubmissionOutput
  { soSubmission :: SubmissionRecord
  , soAnswers    :: [AnswerRecord]
  } deriving (Show, Eq)

data PipelineError
  = EmptyAnswers
  | DuplicateExternalId Text
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Pipeline — pure, no IO
--
-- Build DB records from a validated submission input.
-- Caller inserts soSubmission, gets back a Key, then
-- replaces the placeholder key in soAnswers before inserting them.
-- ---------------------------------------------------------------------------

buildSubmission :: SubmissionInput -> Either PipelineError SubmissionOutput
buildSubmission si
  | Map.null (unAnswers (siAnswers si)) = Left EmptyAnswers
  | otherwise = Right SubmissionOutput
      { soSubmission = SubmissionRecord
          { submissionRecordSurveyId     = surveyIdToRecordId (siSurveyDbId si)
          , submissionRecordExternalId   = siExternalId si
          , submissionRecordSubmittedAt  = siSubmittedAt si
          , submissionRecordRespondentId = siRespondentId si
          }
      , soAnswers = encodeAnswers (siAnswers si)
      }

-- | Convert SubmissionAnswers to AnswerRecord skeletons.
-- submissionId is set to a placeholder key — caller replaces it after inserting the SubmissionRecord.
encodeAnswers :: SubmissionAnswers -> [AnswerRecord]
encodeAnswers (SubmissionAnswers m) =
  [ let (aType, aVal) = encodeAnswer ans
        qidText       = T.pack (show uuid)
    in AnswerRecord
          { answerRecordSubmissionId = placeholderSubmissionRecordId
          , answerRecordQuestionId   = qidText
          , answerRecordAnswerType   = aType
          , answerRecordAnswerValue  = aVal
          }
  | (QuestionId uuid, ans) <- Map.toList m
  ]
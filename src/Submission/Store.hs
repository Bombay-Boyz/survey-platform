module Submission.Store
  ( persistSubmission
  , loadAnswersForSurvey
  ) where

import qualified Data.Map.Strict     as Map
import qualified Data.UUID           as UUID
import Database.Persist.Sql        (Entity(..), SqlBackend,
                                    fromSqlKey, insert, insertMany_,
                                    selectList, (==.))
import Control.Monad.Reader        (ReaderT)
import Control.Monad.IO.Class      (MonadIO)

import Model
import Types.Core                  (QuestionId(..), SubmissionId(..), SurveyId(..))
import Types.Survey                (SubmissionAnswers(..))
import Submission.Pipeline         (SubmissionOutput(..))
import Utils.Conversion            (surveyIdToRecordId)

-- ---------------------------------------------------------------------------
-- Persist a submission in one transaction.
-- Step 1: insert SubmissionRecord → get the real DB key.
-- Step 2: attach that key to every AnswerRecord, then batch-insert.
-- ---------------------------------------------------------------------------

persistSubmission :: MonadIO m => SubmissionOutput -> ReaderT SqlBackend m ()
persistSubmission output = do
  sid <- insert (soSubmission output)
  insertMany_ (map (\ar -> ar { answerRecordSubmissionId = sid }) (soAnswers output))

-- ---------------------------------------------------------------------------
-- Load all answers for a survey, decoded back into domain types.
-- The questionId is stored in the DB as the UUID's text representation.
-- Rows with unparseable question IDs are silently dropped.
-- ---------------------------------------------------------------------------

loadAnswersForSurvey
  :: MonadIO m
  => SurveyId
  -> ReaderT SqlBackend m [(SubmissionId, SubmissionAnswers)]
loadAnswersForSurvey surveyId = do
  let surveyDbId = surveyIdToRecordId surveyId
  subs <- selectList [SubmissionRecordSurveyId ==. surveyDbId] []
  mapM loadOne subs
  where
    loadOne (Entity sid _) = do
      rows <- selectList [AnswerRecordSubmissionId ==. sid] []
      let pairs  = concatMap (toPair . entityVal) rows
          domSid = mkSubmissionId sid
      pure (domSid, SubmissionAnswers (Map.fromList pairs))

    toPair ar =
      case ( UUID.fromText (answerRecordQuestionId ar)
           , decodeAnswer (answerRecordAnswerType ar)
                          (answerRecordAnswerValue ar)
           ) of
        (Just uid, Just ans) -> [(QuestionId uid, ans)]
        _                    -> []

    -- Convert DB Int64 key to a deterministic SubmissionId UUID.
    mkSubmissionId sid =
      let n = fromIntegral (fromSqlKey sid) :: Int
      in SubmissionId (UUID.fromWords 0 0 0 (fromIntegral n))
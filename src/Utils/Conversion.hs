module Utils.Conversion
  ( surveyIdToRecordId
  , surveyRecordIdToId
  , placeholderSubmissionRecordId
  ) where

import Database.Persist.Sql (toSqlKey, fromSqlKey)
import qualified Data.UUID as UUID
import Data.Int (Int64)
import Model (SurveyRecordId, SubmissionRecordId)
import Types.Core (SurveyId(..))

-- | Convert a domain SurveyId to a Persistent DB key
surveyIdToRecordId :: SurveyId -> SurveyRecordId
surveyIdToRecordId (SurveyId uuid) = toSqlKey (uuidToInt64 uuid)

-- | Convert a Persistent SurveyRecordId back to domain SurveyId
--   Deterministic mapping: fromSqlKey → UUID
surveyRecordIdToId :: SurveyRecordId -> SurveyId
surveyRecordIdToId dbKey =
  let n = fromIntegral (fromSqlKey dbKey) :: Int
      uuid = UUID.fromWords 0 0 0 (fromIntegral n)
  in SurveyId uuid

-- | Deterministic UUID -> Int64 mapping
uuidToInt64 :: UUID.UUID -> Int64
uuidToInt64 uuid =
  let (_, _, _, w4) = UUID.toWords uuid
  in fromIntegral w4

-- | Placeholder SubmissionRecordId for encoding answers
--   Use in Submission.Pipeline.encodeAnswers before inserting real DB key
placeholderSubmissionRecordId :: SubmissionRecordId
placeholderSubmissionRecordId = toSqlKey 0
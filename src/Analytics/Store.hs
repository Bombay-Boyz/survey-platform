module Analytics.Store
  ( fetchAnalytics
  ) where

import Database.Persist.Sql        (SqlBackend)
import Control.Monad.Reader        (ReaderT)
import Control.Monad.IO.Class      (MonadIO)

import Types.Core                  (SurveyId(..))
import Analytics.Engine            (Filter(..), normalize)
import Analytics.Response          (AnalyticsResponse, buildResponse)
import Submission.Store            (loadAnswersForSurvey)

-- ---------------------------------------------------------------------------
-- Load answers from DB and run the full analytics pipeline.
--
-- This is the single integration point between the DB layer and the
-- analytics engine. Everything below this function is pure.
--
-- Algorithm:
--   1. Load (SubmissionId, SubmissionAnswers) pairs from DB
--   2. normalize → [NormalizedAnswer]
--   3. buildResponse FAll → AnalyticsResponse
-- ---------------------------------------------------------------------------

fetchAnalytics :: MonadIO m => SurveyId -> ReaderT SqlBackend m AnalyticsResponse
fetchAnalytics surveyId = do
    -- loadAnswersForSurvey is now polymorphic over MonadIO
    submissions <- loadAnswersForSurvey surveyId
    let answers = normalize submissions
    pure (buildResponse FAll answers)
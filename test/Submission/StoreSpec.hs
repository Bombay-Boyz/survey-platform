{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Submission.StoreSpec (spec) where

import qualified Data.Map.Strict       as Map
import Control.Monad.IO.Class           (liftIO)
import Control.Monad.Logger             (runNoLoggingT)
import Control.Monad.Trans.Reader       (ReaderT)
import Data.Time                        (UTCTime(..), fromGregorian, secondsToDiffTime)
import Data.UUID                        (fromWords)
import Database.Persist.Sqlite          (withSqliteConn, runSqlConn, runMigration, SqlBackend)
import Database.Persist                  (insert)
import Database.Persist.Sql             (fromSqlKey)  -- ✅ FIX ADDED

import Test.Hspec

import Types.Core
import Types.Survey                     (Answer(..), SubmissionAnswers(..))
import Model
import Submission.Pipeline              (SubmissionInput(..), SubmissionOutput(..), buildSubmission)
import Submission.Store                 (persistSubmission, loadAnswersForSurvey)
import Utils.Conversion                 (surveyIdToRecordId)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

epoch :: UTCTime
epoch = UTCTime (fromGregorian 2024 1 1) (secondsToDiffTime 0)

qA, qB :: QuestionId
qA = QuestionId (fromWords 0 0 0 1)
qB = QuestionId (fromWords 0 0 0 2)

-- Run an action in an in-memory SQLite DB with the survey schema.
withTestDb :: ReaderT SqlBackend IO a -> IO a
withTestDb action = runNoLoggingT $
  withSqliteConn ":memory:" $ \conn ->
    liftIO $ runSqlConn (runMigration migrateAll >> action) conn

-- A minimal survey record to satisfy the FK.
testSurveyRecord :: SurveyRecord
testSurveyRecord = SurveyRecord
  { surveyRecordExternalId = "survey-001"
  , surveyRecordTitle      = "Test Survey"
  , surveyRecordSchemaJson = "{}"
  , surveyRecordVersion    = 1
  , surveyRecordCreatedAt  = epoch
  }

-- ✅ FIX: Proper mapping from DB key → domain SurveyId
recordIdToSurveyId :: SurveyRecordId -> SurveyId
recordIdToSurveyId rid =
  SurveyId (fromWords 0 0 0 (fromIntegral (fromSqlKey rid)))

mkInput :: SurveyId -> SubmissionAnswers -> SubmissionInput
mkInput sid ans = SubmissionInput
  { siSurveyDbId   = sid
  , siExternalId   = "sub-001"
  , siSubmittedAt  = epoch
  , siRespondentId = Nothing
  , siAnswers      = ans
  }

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "Submission.Store" $ do

  describe "persistSubmission + loadAnswersForSurvey" $ do

    it "persists and reloads one answer" $ do
      result <- withTestDb $ do
        surveyRecordKey <- insert testSurveyRecord
        let domainSurveyId = recordIdToSurveyId surveyRecordKey
        let answers = SubmissionAnswers (Map.singleton qA (AText "hello"))
        case buildSubmission (mkInput domainSurveyId answers) of
          Left e -> error (show e)
          Right o -> do
            persistSubmission o
            loadAnswersForSurvey domainSurveyId
      length result `shouldBe` 1
      let (_, SubmissionAnswers m) = head result
      Map.size m `shouldBe` 1

    it "persists and reloads two answers" $ do
      result <- withTestDb $ do
        surveyRecordKey <- insert testSurveyRecord
        let domainSurveyId = recordIdToSurveyId surveyRecordKey
        let answers = SubmissionAnswers
                        (Map.fromList [(qA, AText "yes"), (qB, ARating 5)])
        case buildSubmission (mkInput domainSurveyId answers) of
          Left e -> error (show e)
          Right o -> do
            persistSubmission o
            loadAnswersForSurvey domainSurveyId
      let (_, SubmissionAnswers m) = head result
      Map.size m `shouldBe` 2

    it "correctly round-trips AText" $ do
      result <- withTestDb $ do
        surveyRecordKey <- insert testSurveyRecord
        let domainSurveyId = recordIdToSurveyId surveyRecordKey
        let answers = SubmissionAnswers (Map.singleton qA (AText "roundtrip"))
        case buildSubmission (mkInput domainSurveyId answers) of
          Left e -> error (show e)
          Right o -> do
            persistSubmission o
            loadAnswersForSurvey domainSurveyId
      let (_, SubmissionAnswers m) = head result
      Map.elems m `shouldContain` [AText "roundtrip"]

    it "correctly round-trips ARating" $ do
      result <- withTestDb $ do
        surveyRecordKey <- insert testSurveyRecord
        let domainSurveyId = recordIdToSurveyId surveyRecordKey
        let answers = SubmissionAnswers (Map.singleton qB (ARating 8))
        case buildSubmission (mkInput domainSurveyId answers) of
          Left e -> error (show e)
          Right o -> do
            persistSubmission o
            loadAnswersForSurvey domainSurveyId
      let (_, SubmissionAnswers m) = head result
      Map.elems m `shouldContain` [ARating 8]

    it "returns empty list when no submissions exist" $ do
      result <- withTestDb $ do
        surveyRecordKey <- insert testSurveyRecord
        let domainSurveyId = recordIdToSurveyId surveyRecordKey
        loadAnswersForSurvey domainSurveyId
      result `shouldBe` []

    it "loads submissions from the correct survey only" $ do
      result <- withTestDb $ do
        sid1Key <- insert testSurveyRecord
        sid2Key <- insert (testSurveyRecord { surveyRecordExternalId = "survey-002" })

        let sid1 = recordIdToSurveyId sid1Key
            sid2 = recordIdToSurveyId sid2Key

        let ans1 = SubmissionAnswers (Map.singleton qA (AText "survey1"))
            ans2 = SubmissionAnswers (Map.singleton qA (AText "survey2"))

        case buildSubmission (mkInput sid1 ans1) of
          Left e -> error (show e)
          Right o -> persistSubmission o

        case buildSubmission ((mkInput sid2 ans2) { siExternalId = "sub-002" }) of
          Left e -> error (show e)
          Right o -> persistSubmission o

        loadAnswersForSurvey sid1

      length result `shouldBe` 1
      let (_, SubmissionAnswers m) = head result
      Map.elems m `shouldContain` [AText "survey1"]
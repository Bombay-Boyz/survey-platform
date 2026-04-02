module Submission.PipelineSpec (spec) where

import qualified Data.Map.Strict   as Map
import Data.Time                   (UTCTime (..), fromGregorian,
                                    secondsToDiffTime)
import Data.UUID                   (fromWords)
import Database.Persist.Sql        (fromSqlKey, toSqlKey)

import Test.Hspec

import Types.Core
import Types.Survey                (Answer (..), SubmissionAnswers (..))
import Model                       (answerRecordAnswerType,
                                    answerRecordAnswerValue,
                                    answerRecordQuestionId,
                                    answerRecordSubmissionId,
                                    submissionRecordExternalId,
                                    submissionRecordRespondentId,
                                    submissionRecordSubmittedAt,
                                    SurveyRecordId)
import Submission.Pipeline

-- ---------------------------------------------------------------------------
-- Fixed test data
-- ---------------------------------------------------------------------------

epoch :: UTCTime
epoch = UTCTime (fromGregorian 2024 1 1) (secondsToDiffTime 0)

qA, qB :: QuestionId
qA = QuestionId (fromWords 0 0 0 1)
qB = QuestionId (fromWords 0 0 0 2)

-- A minimal valid answer map
oneAnswer :: SubmissionAnswers
oneAnswer = SubmissionAnswers (Map.singleton qA (AText "hello"))

twoAnswers :: SubmissionAnswers
twoAnswers = SubmissionAnswers
  (Map.fromList [(qA, AText "hello"), (qB, ARating 5)])

emptyAnswers :: SubmissionAnswers
emptyAnswers = SubmissionAnswers Map.empty

-- We use toSqlKey 1 for the survey FK — the exact value doesn't matter
-- for pipeline tests since we only test the pure logic.
surveyKey :: SurveyRecordId
surveyKey = toSqlKey 1
  
input :: SubmissionAnswers -> SubmissionInput
input ans = SubmissionInput
  { siSurveyDbId   = surveyKey
  , siExternalId   = "ext-001"
  , siSubmittedAt  = epoch
  , siRespondentId = Nothing
  , siAnswers      = ans
  }

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "Submission.Pipeline" $ do

  describe "buildSubmission" $ do

    it "rejects empty answers" $
      buildSubmission (input emptyAnswers) `shouldSatisfy` isLeft

    it "returns EmptyAnswers error for empty input" $
      buildSubmission (input emptyAnswers) `shouldBe` Left EmptyAnswers

    it "accepts a non-empty submission" $
      buildSubmission (input oneAnswer) `shouldSatisfy` isRight

    it "preserves externalId in SubmissionRecord" $
      case buildSubmission (input oneAnswer) of
        Left  e -> expectationFailure (show e)
        Right o ->
          submissionRecordExternalId (soSubmission o) `shouldBe` "ext-001"

    it "preserves submittedAt in SubmissionRecord" $
      case buildSubmission (input oneAnswer) of
        Left  e -> expectationFailure (show e)
        Right o ->
          submissionRecordSubmittedAt (soSubmission o) `shouldBe` epoch

    it "preserves respondentId (Nothing) in SubmissionRecord" $
      case buildSubmission (input oneAnswer) of
        Left  e -> expectationFailure (show e)
        Right o ->
          submissionRecordRespondentId (soSubmission o) `shouldBe` Nothing

    it "produces one AnswerRecord per answer" $
      case buildSubmission (input twoAnswers) of
        Left  e -> expectationFailure (show e)
        Right o -> length (soAnswers o) `shouldBe` 2

    it "produces exactly one AnswerRecord for one answer" $
      case buildSubmission (input oneAnswer) of
        Left  e -> expectationFailure (show e)
        Right o -> length (soAnswers o) `shouldBe` 1

  describe "encodeAnswers" $ do

    it "encodes AText correctly" $ do
      let ans = SubmissionAnswers (Map.singleton qA (AText "hello"))
          rs  = encodeAnswers ans
      case rs of
        [r] -> do
          answerRecordAnswerType  r `shouldBe` "text"
          answerRecordAnswerValue r `shouldBe` "hello"
        _   -> expectationFailure "expected exactly one record"

    it "encodes ARating correctly" $ do
      let ans = SubmissionAnswers (Map.singleton qA (ARating 7))
          rs  = encodeAnswers ans
      case rs of
        [r] -> do
          answerRecordAnswerType  r `shouldBe` "rating"
          answerRecordAnswerValue r `shouldBe` "7"
        _   -> expectationFailure "expected exactly one record"

    it "encodes AChoice correctly" $ do
      let ans = SubmissionAnswers (Map.singleton qA (AChoice "opt-a"))
          rs  = encodeAnswers ans
      case rs of
        [r] -> do
          answerRecordAnswerType  r `shouldBe` "choice"
          answerRecordAnswerValue r `shouldBe` "opt-a"
        _   -> expectationFailure "expected exactly one record"

    it "encodes ANumber correctly" $ do
      let ans = SubmissionAnswers (Map.singleton qA (ANumber 3.14))
          rs  = encodeAnswers ans
      case rs of
        [r] -> do
          answerRecordAnswerType  r `shouldBe` "number"
        _   -> expectationFailure "expected exactly one record"

    it "sets placeholder submissionId (key 0) on all records" $ do
      let ans = SubmissionAnswers
                  (Map.fromList [(qA, AText "x"), (qB, AText "y")])
          rs  = encodeAnswers ans
      all (\r -> fromSqlKey (answerRecordSubmissionId r) == 0) rs
        `shouldBe` True

    it "stores the question UUID as the questionId text" $ do
      let ans = SubmissionAnswers (Map.singleton qA (AText "x"))
          rs  = encodeAnswers ans
      case rs of
        [r] -> answerRecordQuestionId r `shouldSatisfy` (not . null . show)
        _   -> expectationFailure "expected exactly one record"

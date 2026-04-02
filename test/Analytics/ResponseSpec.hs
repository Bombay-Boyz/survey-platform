{-# LANGUAGE OverloadedStrings #-}

module Analytics.ResponseSpec (spec) where

import qualified Data.Map.Strict   as Map
import qualified Data.Aeson        as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text         as T
import Data.UUID                   (fromWords)

import Test.Hspec

import Types.Core
import Types.Survey        (Answer (..), SubmissionAnswers (..))
import Analytics.Engine    (Filter (..), normalize, NormalizedAnswer)
import Analytics.Response

-- ---------------------------------------------------------------------------
-- Fixed test data
-- ---------------------------------------------------------------------------

sid1, sid2, sid3 :: SubmissionId
sid1 = SubmissionId (fromWords 1 0 0 0)
sid2 = SubmissionId (fromWords 2 0 0 0)
sid3 = SubmissionId (fromWords 3 0 0 0)

qA, qB :: QuestionId
qA = QuestionId (fromWords 0 0 0 1)
qB = QuestionId (fromWords 0 0 0 2)

submissions :: [(SubmissionId, SubmissionAnswers)]
submissions =
  [ (sid1, SubmissionAnswers (Map.fromList [(qA, AChoice "yes"), (qB, ARating 8)]))
  , (sid2, SubmissionAnswers (Map.fromList [(qA, AChoice "yes"), (qB, ARating 6)]))
  , (sid3, SubmissionAnswers (Map.fromList [(qA, AChoice "no"),  (qB, ARating 7)]))
  ]

answers :: [NormalizedAnswer]
answers = normalize submissions

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

questionIdText :: QuestionId -> T.Text
questionIdText (QuestionId u) = T.pack (show u)

findQuestion :: QuestionId -> [QuestionResult] -> Maybe QuestionResult
findQuestion qid =
  let target = questionIdText qid
  in  foldr (\q acc -> if qrQuestionId q == target then Just q else acc) Nothing

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "Analytics.Response" $ do

  -- -------------------------------------------------------------------------
  -- buildResponse
  -- -------------------------------------------------------------------------

  describe "buildResponse" $ do

    it "returns one QuestionResult per distinct question" $ do
      let r = buildResponse FAll answers
      length (arQuestions r) `shouldBe` 2

    it "each QuestionResult has a non-empty questionId" $ do
      let r = buildResponse FAll answers
      all (not . T.null . qrQuestionId) (arQuestions r)
        `shouldBe` True

    it "counts are correct for qA (3 answers)" $ do
      let r = buildResponse FAll answers
      case findQuestion qA (arQuestions r) of
        Just q  -> qrCount q `shouldBe` 3
        Nothing -> expectationFailure "qA not found"

    it "counts are correct for qB (3 answers)" $ do
      let r = buildResponse FAll answers
      case findQuestion qB (arQuestions r) of
        Just q  -> qrCount q `shouldBe` 3
        Nothing -> expectationFailure "qB not found"

    it "mean is present for numeric question (qB)" $ do
      let r = buildResponse FAll answers
      case findQuestion qB (arQuestions r) of
        Just q  -> qrMean q `shouldBe` Just 7.0
        Nothing -> expectationFailure "qB not found"

    it "mean is Nothing for text question (qA)" $ do
      let r = buildResponse FAll answers
      case findQuestion qA (arQuestions r) of
        Just q  -> qrMean q `shouldBe` Nothing
        Nothing -> expectationFailure "qA not found"

    it "empty answers produces empty response" $ do
      let r = buildResponse FAll []
      arQuestions r `shouldBe` []

    it "FNone filter produces empty response" $ do
      let r = buildResponse FNone answers
      arQuestions r `shouldBe` []

  -- -------------------------------------------------------------------------
  -- JSON
  -- -------------------------------------------------------------------------

  describe "ToJSON instance" $ do

    it "serialises to a JSON object with 'questions' key" $ do
      let r   = buildResponse FAll answers
          val = Aeson.toJSON r
      case val of
        Aeson.Object obj ->
          KM.member "questions" obj `shouldBe` True
        _ -> expectationFailure "expected JSON object"

    it "each question has required keys" $ do
      let r = buildResponse FAll answers
      case arQuestions r of
        (q:_) ->
          case Aeson.toJSON q of
            Aeson.Object obj -> do
              KM.member "question_id" obj `shouldBe` True
              KM.member "count"       obj `shouldBe` True
              KM.member "frequency"   obj `shouldBe` True
            _ -> expectationFailure "expected JSON object"
        [] -> expectationFailure "expected at least one question"

  -- -------------------------------------------------------------------------
  -- renderValue behavior (via frequency)
  -- -------------------------------------------------------------------------

  describe "renderValue" $ do

    it "round-trips through buildResponse frequency" $ do
      let r = buildResponse (FQuestion qA) answers
      case arQuestions r of
        [q] -> do
          let vals = map fst (qrFrequency q)
          "yes" `elem` vals `shouldBe` True
          "no"  `elem` vals `shouldBe` True
        _ -> expectationFailure "expected exactly one question"
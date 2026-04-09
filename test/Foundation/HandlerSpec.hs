{-# LANGUAGE OverloadedStrings #-}
module Foundation.HandlerSpec (spec) where

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict      as Map
import qualified Data.Set             as Set
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
import qualified Data.UUID.V4         as UUID4
import Data.List.NonEmpty             (fromList, toList)
import qualified Data.List.NonEmpty   as NE
import Data.Text                      (Text)
import Data.UUID                      (fromWords, toText)

import Test.Hspec

import Types.Core
import Types.Survey
import Types.Logic                    (mkRule, Rule, AnyAction (..), Action (..),
                                       Condition (..), Operator (..))
import Logic.Engine                   (defaultEvalState, evalRules, visibleQuestions)
import Analytics.Engine               (answerToValue)

-- ---------------------------------------------------------------------------
-- Fixed test IDs
-- ---------------------------------------------------------------------------

surveyUuid :: SurveyId
surveyUuid = SurveyId (fromWords 0 0 0 1)

pageUuid :: PageId
pageUuid = PageId (fromWords 0 0 1 0)

qA, qB, qC :: QuestionId
qA = QuestionId (fromWords 0 0 0 1)
qB = QuestionId (fromWords 0 0 0 2)
qC = QuestionId (fromWords 0 0 0 3)

-- ---------------------------------------------------------------------------
-- Test survey
-- ---------------------------------------------------------------------------

testSurvey :: Survey
testSurvey = Survey
  { surveyId    = surveyUuid
  , surveyTitle = "Customer Feedback"
  , surveyPages = fromList
      [ Page
          { pageId        = pageUuid
          , pageTitle     = Just "Page 1"
          , pageQuestions = fromList
              [ Question qA "How did you hear about us?"
                  (AnyQT (QChoice (fromList ["social", "friend", "ad"]))) True
              , Question qB "Rate your experience (1-10)"
                  (AnyQT (QRating 10)) True
              , Question qC "Any additional comments?"
                  (AnyQT QText) False
              ]
          }
      ]
  , surveyRules = []
  }

-- ---------------------------------------------------------------------------
-- Helpers mirroring Foundation logic (pure, no Yesod needed)
-- ---------------------------------------------------------------------------

decodeSurvey :: Text -> Either String Survey
decodeSurvey = Aeson.eitherDecodeStrict . TE.encodeUtf8

encodeSurvey :: Survey -> Text
encodeSurvey = TE.decodeUtf8 . BL.toStrict . Aeson.encode

-- | Mirror of the answer-filtering logic in postSurveyR.
-- Runs the logic engine and strips answers for hidden questions.
filterAnswers :: Survey -> SubmissionAnswers -> SubmissionAnswers
filterAnswers survey (SubmissionAnswers m) =
  let qids     = map questionId (allQuestions survey)
      valueMap = Map.map answerToValue m
      evalSt   = evalRules valueMap (surveyRules survey) (defaultEvalState qids)
      visible  = visibleQuestions evalSt
  in  SubmissionAnswers (Map.filterWithKey (\qid _ -> Set.member qid visible) m)

safeRule :: [Condition] -> [AnyAction] -> Int -> Rule
safeRule conds acts pri =
  case mkRule conds acts pri of
    Right r -> r
    Left  e -> error ("HandlerSpec.safeRule: " ++ show e)

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "Foundation (Phase 10)" $ do

  -- -------------------------------------------------------------------------
  -- Survey JSON codec
  -- -------------------------------------------------------------------------

  describe "Survey JSON codec" $ do

    it "encodes a survey to non-empty JSON" $
      T.null (encodeSurvey testSurvey) `shouldBe` False

    it "decodes well-formed survey JSON without error" $
      decodeSurvey (encodeSurvey testSurvey) `shouldSatisfy` isRight

    it "round-trips surveyTitle" $ do
      case decodeSurvey (encodeSurvey testSurvey) of
        Left  e -> expectationFailure e
        Right s -> surveyTitle s `shouldBe` "Customer Feedback"

    it "round-trips page count" $ do
      case decodeSurvey (encodeSurvey testSurvey) of
        Left  e -> expectationFailure e
        Right s -> length (toList (surveyPages s)) `shouldBe` 1

    it "round-trips question count on page 1" $ do
      case decodeSurvey (encodeSurvey testSurvey) of
        Left  e -> expectationFailure e
        Right s ->
          length (toList (pageQuestions (NE.head (surveyPages s)))) `shouldBe` 3

    it "round-trips QChoice options" $ do
      case decodeSurvey (encodeSurvey testSurvey) of
        Left  e -> expectationFailure e
        Right s ->
          questionType (head (allQuestions s))
            `shouldBe` AnyQT (QChoice (fromList ["social", "friend", "ad"]))

    it "round-trips QRating max" $ do
      case decodeSurvey (encodeSurvey testSurvey) of
        Left  e -> expectationFailure e
        Right s ->
          questionType (allQuestions s !! 1) `shouldBe` AnyQT (QRating 10)

    it "round-trips QText" $ do
      case decodeSurvey (encodeSurvey testSurvey) of
        Left  e -> expectationFailure e
        Right s ->
          questionType (allQuestions s !! 2) `shouldBe` AnyQT QText

    it "round-trips required flags" $ do
      case decodeSurvey (encodeSurvey testSurvey) of
        Left  e -> expectationFailure e
        Right s ->
          map questionRequired (allQuestions s) `shouldBe` [True, True, False]

    it "round-trips page title Just" $ do
      case decodeSurvey (encodeSurvey testSurvey) of
        Left  e -> expectationFailure e
        Right s ->
          pageTitle (NE.head (surveyPages s)) `shouldBe` Just "Page 1"

    it "round-trips page title Nothing" $ do
      let noTitle = testSurvey
            { surveyPages = fromList
                [ (NE.head (surveyPages testSurvey)) { pageTitle = Nothing } ]
            }
      case decodeSurvey (encodeSurvey noTitle) of
        Left  e -> expectationFailure e
        Right s -> pageTitle (NE.head (surveyPages s)) `shouldBe` Nothing

    it "surveyRules defaults to empty list on decode" $ do
      case decodeSurvey (encodeSurvey testSurvey) of
        Left  e -> expectationFailure e
        Right s -> surveyRules s `shouldBe` []

    it "fails on invalid JSON" $
      decodeSurvey "not-json" `shouldSatisfy` isLeft

    it "fails when 'pages' key is missing" $
      decodeSurvey
        "{\"id\":\"00000000-0000-0000-0000-000000000001\",\"title\":\"T\"}"
        `shouldSatisfy` isLeft

    it "fails when pages array is empty" $
      decodeSurvey
        "{\"id\":\"00000000-0000-0000-0000-000000000001\",\"title\":\"T\",\"pages\":[]}"
        `shouldSatisfy` isLeft

    it "fails when a question has an unknown type tag" $
      decodeSurvey
        ( T.replace "\"text\"" "\"unknown_tag\""
          (encodeSurvey testSurvey) )
        `shouldSatisfy` isLeft

  -- -------------------------------------------------------------------------
  -- allQuestions
  -- -------------------------------------------------------------------------

  describe "allQuestions" $ do

    it "returns all questions from a single-page survey" $
      length (allQuestions testSurvey) `shouldBe` 3

    it "returns all questions from a multi-page survey" $ do
      let page2 = Page
            { pageId        = PageId (fromWords 0 0 2 0)
            , pageTitle     = Nothing
            , pageQuestions = fromList [Question qC "Extra" (AnyQT QText) False]
            }
          multi = testSurvey
            { surveyPages = fromList
                [ NE.head (surveyPages testSurvey), page2 ] }
      length (allQuestions multi) `shouldBe` 4

    it "preserves question order within pages" $
      map questionId (allQuestions testSurvey) `shouldBe` [qA, qB, qC]

    it "concatenates questions across pages in page order" $ do
      let page2 = Page
            { pageId        = PageId (fromWords 0 0 2 0)
            , pageTitle     = Nothing
            , pageQuestions = fromList [Question qC "Extra" (AnyQT QText) False]
            }
          multi = testSurvey
            { surveyPages = fromList
                [ NE.head (surveyPages testSurvey), page2 ] }
      -- qA, qB come from page 1; qC from page 2
      map questionId (allQuestions multi) `shouldBe` [qA, qB, qC, qC]

  -- -------------------------------------------------------------------------
  -- Logic-engine answer filtering (mirrors postSurveyR)
  -- -------------------------------------------------------------------------

  describe "answer filtering via logic engine" $ do

    it "passes all answers through when there are no rules" $ do
      let answers = SubmissionAnswers
            (Map.fromList [(qA, AChoice "social"), (qB, ARating 8), (qC, AText "great")])
      Map.size (unAnswers (filterAnswers testSurvey answers)) `shouldBe` 3

    it "strips hidden question answer when its rule fires" $ do
      let rule    = safeRule [Condition qA OpEq (VText "social")]
                             [AnyAction (HideQuestion qC)] 0
          survey  = testSurvey { surveyRules = [rule] }
          answers = SubmissionAnswers
            (Map.fromList [(qA, AChoice "social"), (qB, ARating 8), (qC, AText "n/a")])
          filtered = filterAnswers survey answers
      Map.member qC (unAnswers filtered) `shouldBe` False

    it "keeps answers for questions that remain visible" $ do
      let rule    = safeRule [Condition qA OpEq (VText "social")]
                             [AnyAction (HideQuestion qC)] 0
          survey  = testSurvey { surveyRules = [rule] }
          answers = SubmissionAnswers
            (Map.fromList [(qA, AChoice "social"), (qB, ARating 8), (qC, AText "n/a")])
          filtered = filterAnswers survey answers
      Map.member qA (unAnswers filtered) `shouldBe` True
      Map.member qB (unAnswers filtered) `shouldBe` True

    it "does not strip answers when the hide rule condition is unmet" $ do
      let rule    = safeRule [Condition qA OpEq (VText "social")]
                             [AnyAction (HideQuestion qC)] 0
          survey  = testSurvey { surveyRules = [rule] }
          answers = SubmissionAnswers
            (Map.fromList [(qA, AChoice "friend"), (qB, ARating 7), (qC, AText "nice")])
      Map.size (unAnswers (filterAnswers survey answers)) `shouldBe` 3

    it "handles multiple rules: only fired rules affect visibility" $ do
      let rule1   = safeRule [Condition qB OpGt (VNumber 5)]
                             [AnyAction (HideQuestion qC)] 0
          rule2   = safeRule [Condition qA OpEq (VText "ad")]
                             [AnyAction (HideQuestion qB)] 1
          survey  = testSurvey { surveyRules = [rule1, rule2] }
          -- qB = 8 > 5 → rule1 fires, qC hidden
          -- qA = "social" ≠ "ad" → rule2 does not fire, qB stays
          answers = SubmissionAnswers
            (Map.fromList [(qA, AChoice "social"), (qB, ARating 8), (qC, AText "bye")])
          filtered = filterAnswers survey answers
      Map.member qC (unAnswers filtered) `shouldBe` False
      Map.member qB (unAnswers filtered) `shouldBe` True

    it "empty answers produce empty filtered answers" $ do
      let answers = SubmissionAnswers Map.empty
      Map.size (unAnswers (filterAnswers testSurvey answers)) `shouldBe` 0

  -- -------------------------------------------------------------------------
  -- Value JSON codec
  -- -------------------------------------------------------------------------

  describe "Value JSON codec" $ do

    let rt v = Aeson.eitherDecodeStrict (BL.toStrict (Aeson.encode v)) :: Either String Value

    it "round-trips VText"              $ rt (VText "hello") `shouldBe` Right (VText "hello")
    it "round-trips VText empty string" $ rt (VText "")      `shouldBe` Right (VText "")
    it "round-trips VNumber integer"    $ rt (VNumber 42)    `shouldBe` Right (VNumber 42)
    it "round-trips VNumber float"      $ rt (VNumber 3.14)  `shouldBe` Right (VNumber 3.14)
    it "round-trips VNumber negative"   $ rt (VNumber (-7))  `shouldBe` Right (VNumber (-7))
    it "round-trips VBool True"         $ rt (VBool True)    `shouldBe` Right (VBool True)
    it "round-trips VBool False"        $ rt (VBool False)   `shouldBe` Right (VBool False)
    it "round-trips VNull"              $ rt VNull           `shouldBe` Right VNull

  -- -------------------------------------------------------------------------
  -- QuestionId / SurveyId / PageId / SubmissionId UUID codec
  -- -------------------------------------------------------------------------

  describe "UUID newtype JSON codecs" $ do

    let rtQ qid = Aeson.eitherDecodeStrict (BL.toStrict (Aeson.encode qid)) :: Either String QuestionId
    let rtS sid = Aeson.eitherDecodeStrict (BL.toStrict (Aeson.encode sid)) :: Either String SurveyId
    let rtP pid = Aeson.eitherDecodeStrict (BL.toStrict (Aeson.encode pid)) :: Either String PageId

    it "round-trips QuestionId"   $ rtQ qA `shouldBe` Right qA
    it "round-trips SurveyId"     $ rtS surveyUuid `shouldBe` Right surveyUuid
    it "round-trips PageId"       $ rtP pageUuid `shouldBe` Right pageUuid

    it "fails to decode an invalid UUID string for QuestionId" $ do
      let bad = Aeson.String "not-a-uuid"
      (Aeson.fromJSON bad :: Aeson.Result QuestionId) `shouldSatisfy` isError

  -- -------------------------------------------------------------------------
  -- Submission external ID generation
  -- -------------------------------------------------------------------------

  describe "submission external ID generation" $ do

    it "generates IDs with 'sub-' prefix" $ do
      uuid <- UUID4.nextRandom
      let extId = "sub-" <> toText uuid
      T.isPrefixOf "sub-" extId `shouldBe` True

    it "two generated IDs are different" $ do
      id1 <- ("sub-" <>) . toText <$> UUID4.nextRandom
      id2 <- ("sub-" <>) . toText <$> UUID4.nextRandom
      id1 `shouldNotBe` id2

    it "generated ID has correct total length (sub- + 36 UUID chars)" $ do
      uuid <- UUID4.nextRandom
      let extId = "sub-" <> toText uuid
      T.length extId `shouldBe` 40

-- ---------------------------------------------------------------------------
-- Helper
-- ---------------------------------------------------------------------------

isError :: Aeson.Result a -> Bool
isError (Aeson.Error _) = True
isError _               = False

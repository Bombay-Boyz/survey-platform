-- test/Model/SurveySpec.hs
module Model.SurveySpec (spec) where

import Data.List.NonEmpty           (fromList)
import Data.Text (Text)
import Data.UUID                    (fromWords)

import Test.Hspec
import Test.Hspec.Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

import Model        (encodeAnswer, decodeAnswer)
import Types.Core
import Types.Survey

-- Generators ----------------------------------------------------------------

genAnswer :: Gen Answer
genAnswer = Gen.choice
  [ AText   <$> Gen.text (Range.linear 0 50) Gen.alphaNum
  , AChoice <$> Gen.text (Range.linear 1 50) Gen.alphaNum
  , ARating <$> Gen.int  (Range.linear 1 10)
  , ANumber <$> Gen.double (Range.linearFrac (-1e6) 1e6)
  ]

-- Spec ----------------------------------------------------------------------

spec :: Spec
spec = describe "Survey domain model" $ do

  describe "validateSurvey" $ do

    it "rejects an empty title" $
      validateSurvey (makeSurvey "") `shouldSatisfy` isLeft

    it "accepts a valid survey" $ do
      let s = makeSurvey "Customer Feedback"
      validateSurvey s `shouldBe` Right s

    it "rejects duplicate page IDs" $ do
      let pid  = PageId (fromWords 0 0 0 1)
          page = makePage pid
          s    = (makeSurvey "T") { surveyPages = fromList [page, page] }
      validateSurvey s `shouldSatisfy` isLeft

    it "rejects duplicate question IDs" $ do
      let qid  = QuestionId (fromWords 0 0 0 99)
          q    = makeQuestion qid
          page = Page
                   (PageId (fromWords 0 0 0 1))
                   Nothing
                   (fromList [q, q])
          s    = (makeSurvey "T") { surveyPages = fromList [page] }
      validateSurvey s `shouldSatisfy` isLeft

  describe "Answer codec" $
    it "encodeAnswer / decodeAnswer round-trip" $ hedgehog $ do
      ans <- forAll genAnswer
      let (typ, val) = encodeAnswer ans
      decodeAnswer typ val === Just ans

-- Helpers -------------------------------------------------------------------

makeSurvey :: Text -> Survey
makeSurvey t = Survey
  { surveyId    = SurveyId    (fromWords 0 0 0 0)
  , surveyTitle = t
  , surveyPages = fromList [makePage (PageId (fromWords 0 0 0 1))]
  , surveyRules = []
  }

makePage :: PageId -> Page
makePage pid =
  Page pid Nothing
    (fromList [makeQuestion (QuestionId (fromWords 0 0 0 42))])

makeQuestion :: QuestionId -> Question
makeQuestion qid = Question
  { questionId       = qid
  , questionLabel    = "Sample"
  , questionType     = AnyQT QText
  , questionRequired = True
  }

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False
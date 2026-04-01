module Analytics.QuerySpec (spec) where

import qualified Data.Map.Strict as Map
import Data.UUID                 (fromWords)

import Test.Hspec

import Types.Core
import Types.Survey              (Answer (..), SubmissionAnswers (..))
import Analytics.Engine          (normalize, Filter (..))
import Analytics.Query

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

spec :: Spec
spec = describe "Analytics.Query" $ do

  describe "summariseQuestion" $ do
    it "counts total responses correctly" $
      qsCount (summariseQuestion FAll qA answers) `shouldBe` 3
    it "builds correct frequency map" $ do
      let freq = qsFrequency (summariseQuestion FAll qA answers)
      Map.lookup (VText "yes") freq `shouldBe` Just 2
      Map.lookup (VText "no")  freq `shouldBe` Just 1
    it "computes mean for numeric answers" $
      qsMean (summariseQuestion FAll qB answers) `shouldBe` Just 7.0
    it "computes median for numeric answers" $
      qsMedian (summariseQuestion FAll qB answers) `shouldBe` Just 7.0
    it "returns Nothing for mean when no numeric answers" $
      qsMean (summariseQuestion FAll qA answers) `shouldBe` Nothing
    it "returns Nothing for median when no numeric answers" $
      qsMedian (summariseQuestion FAll qA answers) `shouldBe` Nothing

  describe "summariseAll" $ do
    it "produces a summary for each distinct question" $
      Map.size (summariseAll FAll answers) `shouldBe` 2
    it "each summary has the correct question ID" $ do
      let m = summariseAll FAll answers
      Map.member qA m `shouldBe` True
      Map.member qB m `shouldBe` True
    it "results are consistent with summariseQuestion" $ do
      let m = summariseAll FAll answers
      Map.lookup qA m `shouldBe` Just (summariseQuestion FAll qA answers)
      Map.lookup qB m `shouldBe` Just (summariseQuestion FAll qB answers)

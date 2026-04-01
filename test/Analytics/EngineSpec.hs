module Analytics.EngineSpec (spec) where

import qualified Data.Map.Strict as Map
import Data.UUID                 (fromWords)

import Test.Hspec

import Types.Core
import Types.Survey              (Answer (..), SubmissionAnswers (..))
import Analytics.Engine

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

extractFrequency :: AggResult -> [(Value, Int)]
extractFrequency (FrequencyResult ps) = ps
extractFrequency other = error $ "expected FrequencyResult, got: " ++ show other

extractMean :: AggResult -> Maybe Double
extractMean (MeanResult v) = v
extractMean other = error $ "expected MeanResult, got: " ++ show other

extractMedian :: AggResult -> Maybe Double
extractMedian (MedianResult v) = v
extractMedian other = error $ "expected MedianResult, got: " ++ show other

extractCrossTab :: AggResult -> Map.Map (Value, Value) Int
extractCrossTab (CrossTabResult m) = m
extractCrossTab other = error $ "expected CrossTabResult, got: " ++ show other

spec :: Spec
spec = describe "Analytics.Engine" $ do

  describe "normalize" $ do
    it "produces one NormalizedAnswer per (submission, question) pair" $
      length answers `shouldBe` 6
    it "converts AChoice to VText" $
      answers `shouldContain` [NormalizedAnswer sid1 qA (VText "yes")]
    it "converts ARating to VNumber" $
      answers `shouldContain` [NormalizedAnswer sid1 qB (VNumber 8)]

  describe "evalFilter" $ do
    it "FAll admits every answer" $
      filter (evalFilter FAll) answers `shouldBe` answers
    it "FNone admits no answer" $
      filter (evalFilter FNone) answers `shouldBe` []
    it "FQuestion filters to one question" $
      length (filter (evalFilter (FQuestion qA)) answers) `shouldBe` 3
    it "FValue filters to matching value" $
      length (filter (evalFilter (FValue (VText "yes"))) answers) `shouldBe` 2
    it "FAnd is conjunction" $
      length (filter (evalFilter (FAnd (FQuestion qA) (FValue (VText "yes")))) answers)
        `shouldBe` 2
    it "FOr is disjunction" $
      length (filter (evalFilter (FOr (FValue (VText "yes")) (FValue (VText "no")))) answers)
        `shouldBe` 3
    it "FNot inverts FAll to FNone" $
      filter (evalFilter (FNot FAll)) answers `shouldBe` []
    it "double negation: FNot (FNot f) == f" $ do
      let f  = FQuestion qA
          xs = filter (evalFilter f)               answers
          ys = filter (evalFilter (FNot (FNot f))) answers
      xs `shouldBe` ys
    it "De Morgan: FAnd f g == FNot (FOr (FNot f) (FNot g))" $ do
      let f   = FQuestion qA
          g   = FValue (VText "yes")
          lhs = filter (evalFilter (FAnd f g))                     answers
          rhs = filter (evalFilter (FNot (FOr (FNot f) (FNot g)))) answers
      lhs `shouldBe` rhs

  describe "Frequency aggregation" $ do
    it "counts each distinct value" $ do
      let pairs = extractFrequency (runAggregation (FQuestion qA) Frequency answers)
      pairs `shouldContain` [(VText "yes", 2)]
      pairs `shouldContain` [(VText "no",  1)]
    it "orders results descending by count" $ do
      let pairs = extractFrequency (runAggregation (FQuestion qA) Frequency answers)
      map snd pairs `shouldSatisfy` isDescending

  describe "Mean aggregation" $ do
    it "computes arithmetic mean of numeric answers" $
      extractMean (runAggregation (FQuestion qB) Mean answers) `shouldBe` Just 7.0
    it "returns Nothing when no numeric answers" $
      extractMean (runAggregation (FQuestion qA) Mean answers) `shouldBe` Nothing

  describe "Median aggregation" $ do
    it "computes median of odd-length sorted list" $
      extractMedian (runAggregation (FQuestion qB) Median answers) `shouldBe` Just 7.0
    it "returns Nothing when no numeric answers" $
      extractMedian (runAggregation (FQuestion qA) Median answers) `shouldBe` Nothing

  describe "CrossTab aggregation" $ do
    it "counts co-occurrences between two questions" $ do
      let m = extractCrossTab (runAggregation FAll (CrossTab qA qB) answers)
      Map.lookup (VText "yes", VNumber 8) m `shouldBe` Just 1
      Map.lookup (VText "yes", VNumber 6) m `shouldBe` Just 1
      Map.lookup (VText "no",  VNumber 7) m `shouldBe` Just 1

isDescending :: Ord a => [a] -> Bool
isDescending []       = True
isDescending [_]      = True
isDescending (x:y:zs) = x >= y && isDescending (y:zs)

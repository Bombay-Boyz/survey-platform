module Analytics.CSVSpec (spec) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict      as Map
import Data.UUID                      (fromWords)

import Test.Hspec

import Types.Core
import Types.Survey   (Answer (..), SubmissionAnswers (..))
import Analytics.CSV

-- ---------------------------------------------------------------------------
-- Fixed IDs
-- ---------------------------------------------------------------------------

sid1, sid2 :: SubmissionId
sid1 = SubmissionId (fromWords 1 0 0 0)
sid2 = SubmissionId (fromWords 2 0 0 0)

qA, qB :: QuestionId
qA = QuestionId (fromWords 0 0 0 1)
qB = QuestionId (fromWords 0 0 0 2)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

csvLines :: BL.ByteString -> [String]
csvLines = lines . filter (/= '\r') . map (toEnum . fromEnum) . BL.unpack

-- Safe: returns Nothing on empty output instead of throwing
firstLine :: BL.ByteString -> Maybe String
firstLine bs = case csvLines bs of
  (l:_) -> Just l
  []    -> Nothing

-- ---------------------------------------------------------------------------
-- Specs
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "Analytics.CSV" $ do

  describe "exportCSV" $ do

    it "produces a header row as the first line" $ do
      let s1 = SubmissionAnswers (Map.singleton qA (AText "hello"))
      firstLine (exportCSV [(sid1, s1)])
        `shouldSatisfy` maybe False ("submission_id" `isInfixOf`)

    it "produces one data row per submission" $ do
      let s1 = SubmissionAnswers (Map.singleton qA (AText "hello"))
          s2 = SubmissionAnswers (Map.singleton qA (AText "world"))
          ls = csvLines (exportCSV [(sid1, s1), (sid2, s2)])
      -- header + 2 data rows = 3 lines
      length ls `shouldBe` 3

    it "empty field for a missing answer in a column" $ do
      -- s1 answers both qA and qB; s2 answers only qA
      -- the qB column for s2 must be an empty trailing field
      let s1 = SubmissionAnswers (Map.fromList [(qA, AText "x"), (qB, AText "y")])
          s2 = SubmissionAnswers (Map.singleton qA (AText "z"))
          ls = csvLines (exportCSV [(sid1, s1), (sid2, s2)])
      case drop 2 ls of
        (s2Row:_) -> s2Row `shouldSatisfy` ("," `isSuffixOf`)
        []        -> expectationFailure "expected at least 3 lines"

    it "column order is stable across calls" $ do
      let s1   = SubmissionAnswers (Map.fromList [(qA, AText "a"), (qB, AText "b")])
          hdr1 = firstLine (exportCSV [(sid1, s1)])
          hdr2 = firstLine (exportCSV [(sid1, s1)])
      hdr1 `shouldBe` hdr2

  describe "answersToField" $ do
    it "encodes AText"   $ answersToField (AText   "hi")  `shouldBe` "hi"
    it "encodes AChoice" $ answersToField (AChoice "opt") `shouldBe` "opt"
    it "encodes ARating" $ answersToField (ARating 3)     `shouldBe` "3"
    it "encodes ANumber" $ answersToField (ANumber 1.5)   `shouldBe` "1.5"

-- ---------------------------------------------------------------------------
-- Local string predicates — no partial functions
-- ---------------------------------------------------------------------------

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack =
  any (isPrefixOf needle) (tails haystack)

isSuffixOf :: String -> String -> Bool
isSuffixOf suffix str =
  drop (length str - length suffix) str == suffix

isPrefixOf :: String -> String -> Bool
isPrefixOf []     _      = True
isPrefixOf _      []     = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

tails :: [a] -> [[a]]
tails []          = [[]]
tails xs@(_:rest) = xs : tails rest

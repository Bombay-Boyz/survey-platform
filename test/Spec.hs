module Main (main) where

import Test.Hspec
import qualified Model.SurveySpec  as Survey
import qualified Logic.EngineSpec  as Logic
import qualified Analytics.CSVSpec as CSV

main :: IO ()
main = hspec $ do
  Survey.spec
  Logic.spec
  CSV.spec

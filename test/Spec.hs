module Main (main) where

import Test.Hspec
import qualified Model.SurveySpec     as Survey
import qualified Logic.EngineSpec     as Logic
import qualified Analytics.CSVSpec    as CSV
import qualified Analytics.EngineSpec as Engine

main :: IO ()
main = hspec $ do
  Survey.spec
  Logic.spec
  CSV.spec
  Engine.spec

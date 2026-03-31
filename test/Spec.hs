-- test/Spec.hs
module Main (main) where

import Test.Hspec
import qualified Model.SurveySpec as Survey

main :: IO ()
main = hspec Survey.spec
module Main (main) where

import Test.Hspec
import qualified Model.SurveySpec          as Survey
import qualified Logic.EngineSpec          as Logic
import qualified Analytics.CSVSpec         as CSV
import qualified Analytics.EngineSpec      as Engine
import qualified Analytics.QuerySpec       as Query
import qualified Analytics.ResponseSpec    as Response
import qualified Distribution.CampaignSpec as Campaign
import qualified Cache.CacheSpec           as Cache
import qualified Submission.PipelineSpec   as Pipeline
import qualified Submission.StoreSpec      as Store

main :: IO ()
main = hspec $ do
  Survey.spec
  Logic.spec
  CSV.spec
  Engine.spec
  Query.spec
  Response.spec
  Campaign.spec
  Cache.spec
  Pipeline.spec
  Store.spec

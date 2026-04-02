module Analytics.Response
  ( AnalyticsResponse (..)
  , QuestionResult (..)
  , buildResponse
  ) where

import qualified Data.Map.Strict  as Map
import Data.Map.Strict            (Map)
import Data.Aeson                 (ToJSON (..), object, (.=))
import Data.Text                  (Text)
import qualified Data.Text        as T

import Types.Core                 (QuestionId (..), Value (..))
import Analytics.Engine           (NormalizedAnswer, Filter (..))
import Analytics.Query            (QuestionSummary (..), summariseAll)

-- ---------------------------------------------------------------------------
-- Response types with ToJSON instances
-- ---------------------------------------------------------------------------

data QuestionResult = QuestionResult
  { qrQuestionId :: Text
  , qrCount      :: Int
  , qrFrequency  :: [(Text, Int)]   -- (value as text, count)
  , qrMean       :: Maybe Double
  , qrMedian     :: Maybe Double
  } deriving (Eq, Show)

instance ToJSON QuestionResult where
  toJSON qr = object
    [ "question_id" .= qrQuestionId qr
    , "count"       .= qrCount      qr
    , "frequency"   .= map pairToObj (qrFrequency qr)
    , "mean"        .= qrMean       qr
    , "median"      .= qrMedian     qr
    ]
    where
      pairToObj (v, n) = object ["value" .= v, "count" .= n]

newtype AnalyticsResponse = AnalyticsResponse
  { arQuestions :: [QuestionResult]
  } deriving (Eq, Show)

instance ToJSON AnalyticsResponse where
  toJSON ar = object ["questions" .= arQuestions ar]

-- ---------------------------------------------------------------------------
-- Builder — pure, no IO
--
-- Algorithm:
--   1. summariseAll computes all question summaries in one pass
--   2. For each summary, convert to QuestionResult:
--      a. Render QuestionId UUID as Text
--      b. Render each Value in the frequency map as Text
--      c. Pass through count, mean, median directly
-- ---------------------------------------------------------------------------

buildResponse :: Filter -> [NormalizedAnswer] -> AnalyticsResponse
buildResponse flt answers =
  AnalyticsResponse
    { arQuestions = map toResult (Map.toList (summariseAll flt answers))
    }

toResult :: (QuestionId, QuestionSummary) -> QuestionResult
toResult (QuestionId uuid, qs) = QuestionResult
  { qrQuestionId = T.pack (show uuid)
  , qrCount      = qsCount    qs
  , qrFrequency  = map renderPair (Map.toList (qsFrequency qs))
  , qrMean       = qsMean     qs
  , qrMedian     = qsMedian   qs
  }
  where
    renderPair (val, n) = (renderValue val, n)

renderValue :: Value -> Text
renderValue (VText   t) = t
renderValue (VNumber n) = T.pack (show n)
renderValue (VBool   b) = if b then "true" else "false"
renderValue VNull       = "null"

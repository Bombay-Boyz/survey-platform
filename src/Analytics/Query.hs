module Analytics.Query
  ( QuestionSummary (..)
  , summariseQuestion
  , summariseAll
  ) where

import qualified Data.Map.Strict as Map
import Data.List                 (foldl', sort)
import Data.Map.Strict           (Map)

import Types.Core                (QuestionId, Value (..))
import Analytics.Engine          (NormalizedAnswer (..), Filter, evalFilter)

data QuestionSummary = QuestionSummary
  { qsQuestionId :: QuestionId
  , qsCount      :: Int
  , qsFrequency  :: Map Value Int
  , qsMean       :: Maybe Double
  , qsMedian     :: Maybe Double
  } deriving (Eq, Show)

data Acc = Acc
  { accCount :: !Int
  , accFreq  :: !(Map Value Int)
  , accNums  :: ![Double]
  }

emptyAcc :: Acc
emptyAcc = Acc 0 Map.empty []

summariseQuestion
  :: Filter -> QuestionId -> [NormalizedAnswer] -> QuestionSummary
summariseQuestion flt qid answers =
  QuestionSummary
    { qsQuestionId = qid
    , qsCount      = accCount acc
    , qsFrequency  = accFreq  acc
    , qsMean       = computeMean   (accNums acc)
    , qsMedian     = computeMedian (accNums acc)
    }
  where
    relevant = filter (\na -> evalFilter flt na && naQuestionId na == qid) answers
    acc      = foldl' step emptyAcc relevant
    step (Acc c freq nums) na =
      Acc (c + 1)
          (Map.insertWith (+) (naValue na) 1 freq)
          (case naValue na of
             VNumber n -> n : nums
             _         -> nums)

summariseAll :: Filter -> [NormalizedAnswer] -> Map QuestionId QuestionSummary
summariseAll flt answers =
  Map.mapWithKey (\qid nas -> summariseQuestion flt qid nas) grouped
  where
    grouped = foldl' step Map.empty (filter (evalFilter flt) answers)
    step m na = Map.insertWith (++) (naQuestionId na) [na] m

computeMean :: [Double] -> Maybe Double
computeMean [] = Nothing
computeMean vs = Just (sum vs / fromIntegral (length vs))

computeMedian :: [Double] -> Maybe Double
computeMedian [] = Nothing
computeMedian vs =
  let sorted = sort vs
      n      = length sorted
      mid    = n `div` 2
  in  Just $ if odd n
               then sorted !! mid
               else (sorted !! (mid - 1) + sorted !! mid) / 2.0

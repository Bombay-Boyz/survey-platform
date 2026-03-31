module Analytics.Engine
  ( -- * Normalized answer
    NormalizedAnswer (..)
  , normalize

    -- * Filter DSL
  , Filter (..)
  , evalFilter

    -- * Aggregations
  , Aggregation (..)
  , AggResult (..)
  , runAggregation
  ) where

import qualified Data.Map.Strict as Map
import Data.List                 (group, sort, sortOn)
import Data.Map.Strict           (Map)
import Data.Ord                  (Down (..))

import Types.Core
import Types.Survey (Answer (..), SubmissionAnswers (..))

-- ---------------------------------------------------------------------------
-- Normalized answer
-- ---------------------------------------------------------------------------

data NormalizedAnswer = NormalizedAnswer
  { naSubmissionId :: SubmissionId
  , naQuestionId   :: QuestionId
  , naValue        :: Value
  } deriving (Eq, Show)

-- | Flatten submissions into one NormalizedAnswer per (submission, question).
-- Complexity: O(S * Q)
normalize :: [(SubmissionId, SubmissionAnswers)] -> [NormalizedAnswer]
normalize submissions =
  [ NormalizedAnswer sid qid (answerToValue ans)
  | (sid, SubmissionAnswers m) <- submissions
  , (qid, ans)                 <- Map.toList m
  ]

answerToValue :: Answer -> Value
answerToValue (AText   t) = VText   t
answerToValue (AChoice c) = VText   c
answerToValue (ARating r) = VNumber (fromIntegral r)
answerToValue (ANumber n) = VNumber n

-- ---------------------------------------------------------------------------
-- Filter DSL — boolean algebra over NormalizedAnswer
--
-- Laws satisfied:
--   identity:         evalFilter FAll  a = True
--                     evalFilter FNone a = False
--   double negation:  evalFilter (FNot (FNot f)) = evalFilter f
--   De Morgan:        FAnd f g = FNot (FOr (FNot f) (FNot g))
--   commutativity:    FAnd f g ≡ FAnd g f
-- ---------------------------------------------------------------------------

data Filter
  = FAll                           -- ^ admits everything
  | FNone                          -- ^ admits nothing
  | FQuestion QuestionId           -- ^ admits answers for this question only
  | FValue    Value                -- ^ admits answers with exactly this value
  | FAnd      Filter Filter        -- ^ conjunction
  | FOr       Filter Filter        -- ^ disjunction
  | FNot      Filter               -- ^ negation
  deriving (Eq, Show)

-- | Evaluate a filter against a single NormalizedAnswer.
-- Complexity: O(depth of filter tree)
evalFilter :: Filter -> NormalizedAnswer -> Bool
evalFilter FAll            _  = True
evalFilter FNone           _  = False
evalFilter (FQuestion qid) na = naQuestionId na == qid
evalFilter (FValue    val) na = naValue      na == val
evalFilter (FAnd      f g) na = evalFilter f na && evalFilter g na
evalFilter (FOr       f g) na = evalFilter f na || evalFilter g na
evalFilter (FNot      f)   na = not (evalFilter f na)

-- ---------------------------------------------------------------------------
-- Aggregations
-- ---------------------------------------------------------------------------

data Aggregation
  = Frequency
  | Mean
  | Median
  | CrossTab QuestionId QuestionId
  deriving (Eq, Show)

data AggResult
  = FrequencyResult  [(Value, Int)]
  | MeanResult       (Maybe Double)
  | MedianResult     (Maybe Double)
  | CrossTabResult   (Map (Value, Value) Int)
  deriving (Eq, Show)

-- | Apply filter then compute aggregation.
runAggregation :: Filter -> Aggregation -> [NormalizedAnswer] -> AggResult
runAggregation flt agg = compute agg . filter (evalFilter flt)

-- ---------------------------------------------------------------------------
-- Aggregation implementations
-- ---------------------------------------------------------------------------

compute :: Aggregation -> [NormalizedAnswer] -> AggResult

-- Frequency: O(N log N)
-- group . sort guarantees each sublist is non-empty, so we pattern match safely.
compute Frequency answers =
  FrequencyResult
    . sortOn (Down . snd)
    . map countGroup
    . group
    . sort
    $ map naValue answers
  where
    countGroup []     = error "impossible: group never produces empty lists"
    countGroup (v:vs) = (v, 1 + length vs)

-- Mean: O(N)
compute Mean answers =
  MeanResult $ case numericValues answers of
    [] -> Nothing
    vs -> Just (sum vs / fromIntegral (length vs))

-- Median: O(N log N)
-- Even-length: average of the two middle elements.
-- Odd-length:  the single middle element.
compute Median answers =
  MedianResult $ case sort (numericValues answers) of
    [] -> Nothing
    vs ->
      let n   = length vs
          mid = n `div` 2
      in  Just $ if odd n
                   then vs !! mid
                   else (vs !! (mid - 1) + vs !! mid) / 2.0

-- CrossTab: O(N log N)
-- Groups answers by submission, then pairs the values of qA and qB.
compute (CrossTab qidA qidB) answers =
  CrossTabResult $ foldl insertPair Map.empty pairs
  where
    bySubmission :: Map SubmissionId [NormalizedAnswer]
    bySubmission = foldl step Map.empty answers
      where
        step m na = Map.insertWith (++) (naSubmissionId na) [na] m

    pairs :: [(Value, Value)]
    pairs =
      [ (vA, vB)
      | nas        <- Map.elems bySubmission
      , Just vA    <- [lookupQ qidA nas]
      , Just vB    <- [lookupQ qidB nas]
      ]

    lookupQ :: QuestionId -> [NormalizedAnswer] -> Maybe Value
    lookupQ qid nas =
      case filter (\na -> naQuestionId na == qid) nas of
        (na:_) -> Just (naValue na)
        []     -> Nothing

    insertPair :: Map (Value, Value) Int -> (Value, Value) -> Map (Value, Value) Int
    insertPair m pair = Map.insertWith (+) pair 1 m

-- ---------------------------------------------------------------------------
-- Helper
-- ---------------------------------------------------------------------------

numericValues :: [NormalizedAnswer] -> [Double]
numericValues nas = [ n | NormalizedAnswer _ _ (VNumber n) <- nas ]

module Types.Core
  ( SurveyId (..)
  , PageId (..)
  , QuestionId (..)
  , SubmissionId (..)
  , Value (..)
  , ValidationError (..)
  , Validated
  , validate
  ) where

import Data.Text (Text)
import Data.UUID (UUID)

newtype SurveyId     = SurveyId     UUID deriving (Eq, Ord, Show)
newtype PageId       = PageId       UUID deriving (Eq, Ord, Show)
newtype QuestionId   = QuestionId   UUID deriving (Eq, Ord, Show)
newtype SubmissionId = SubmissionId UUID deriving (Eq, Ord, Show)

data Value
  = VText   Text
  | VNumber Double
  | VBool   Bool
  | VNull
  deriving (Eq, Ord, Show)

newtype ValidationError = ValidationError Text
  deriving (Eq, Show)

type Validated a = Either [ValidationError] a

validate :: [ValidationError] -> a -> Validated a
validate []   x = Right x
validate errs _ = Left errs
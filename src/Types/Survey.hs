module Types.Survey
  ( QTag (..)
  , QuestionType (..)
  , AnyQuestionType (..)
  , Question (..)
  , Page (..)
  , Survey (..)
  , Answer (..)
  , SubmissionAnswers (..)
  , validateSurvey
  ) where

import qualified Data.Set           as Set
import qualified Data.Text          as T
import Data.List.NonEmpty           (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict              (Map)
import Data.Text                    (Text)

import Types.Core
import Types.Logic (Rule)

data QTag = TText | TChoice | TRating | TNumber

data QuestionType (t :: QTag) where
  QText   ::                  QuestionType 'TText
  QChoice :: NonEmpty Text -> QuestionType 'TChoice
  QRating :: Int           -> QuestionType 'TRating
  QNumber ::                  QuestionType 'TNumber

-- Existential wrapper — Show/Eq defined manually
data AnyQuestionType = forall t. AnyQT (QuestionType t)

instance Show AnyQuestionType where
  show (AnyQT QText)       = "QText"
  show (AnyQT (QChoice _)) = "QChoice"
  show (AnyQT (QRating n)) = "QRating " ++ show n
  show (AnyQT QNumber)     = "QNumber"

instance Eq AnyQuestionType where
  AnyQT QText       == AnyQT QText       = True
  AnyQT (QChoice a) == AnyQT (QChoice b) = a == b
  AnyQT (QRating a) == AnyQT (QRating b) = a == b
  AnyQT QNumber     == AnyQT QNumber     = True
  _                 == _                 = False

data Question = Question
  { questionId       :: QuestionId
  , questionLabel    :: Text
  , questionType     :: AnyQuestionType
  , questionRequired :: Bool
  } deriving (Eq, Show)

data Page = Page
  { pageId        :: PageId
  , pageTitle     :: Maybe Text
  , pageQuestions :: NonEmpty Question
  } deriving (Eq, Show)

data Survey = Survey
  { surveyId    :: SurveyId
  , surveyTitle :: Text
  , surveyPages :: NonEmpty Page
  , surveyRules :: [Rule]
  } deriving (Eq, Show)

data Answer
  = AText   Text
  | AChoice Text
  | ARating Int
  | ANumber Double
  deriving (Eq, Show)

newtype SubmissionAnswers = SubmissionAnswers
  { unAnswers :: Map QuestionId Answer }
  deriving (Eq, Show)

-- Validation ----------------------------------------------------------------

validateSurvey :: Survey -> Validated Survey
validateSurvey s@Survey{..} =
  validate (concat
    [ checkTitle       surveyTitle
    , checkPageIds     (NE.toList surveyPages)
    , checkQuestionIds (allQuestions s)
    ]) s

allQuestions :: Survey -> [Question]
allQuestions Survey{..} =
  concatMap (NE.toList . pageQuestions) (NE.toList surveyPages)

checkTitle :: Text -> [ValidationError]
checkTitle t
  | T.null t  = [ValidationError "Survey title must not be empty"]
  | otherwise = []

checkPageIds :: [Page] -> [ValidationError]
checkPageIds pages
  | allUnique (map pageId pages) = []
  | otherwise = [ValidationError "Duplicate page IDs"]

checkQuestionIds :: [Question] -> [ValidationError]
checkQuestionIds qs
  | allUnique (map questionId qs) = []
  | otherwise = [ValidationError "Duplicate question IDs"]

allUnique :: Ord a => [a] -> Bool
allUnique xs = length xs == Set.size (Set.fromList xs)

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
  , allQuestions
  ) where

import qualified Data.Set           as Set
import qualified Data.Text          as T
import Data.List.NonEmpty           (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict              (Map)
import Data.Text                    (Text)
import Data.Aeson                   (ToJSON (..), FromJSON (..), (.=), (.:), (.:?),
                                     withObject, object, withText)
import Data.Aeson.Types             (Parser)
import qualified Data.Aeson         as Aeson

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

instance ToJSON AnyQuestionType where
  toJSON (AnyQT QText)        = object ["tag" .= ("text"   :: Text)]
  toJSON (AnyQT (QChoice os)) = object ["tag" .= ("choice" :: Text), "options" .= NE.toList os]
  toJSON (AnyQT (QRating n))  = object ["tag" .= ("rating" :: Text), "max" .= n]
  toJSON (AnyQT QNumber)      = object ["tag" .= ("number" :: Text)]

instance FromJSON AnyQuestionType where
  parseJSON = withObject "AnyQuestionType" $ \o -> do
    tag <- o .: "tag" :: Parser Text
    case tag of
      "text"   -> pure (AnyQT QText)
      "choice" -> do
        opts <- o .: "options"
        case NE.nonEmpty opts of
          Nothing -> fail "QChoice requires at least one option"
          Just ne -> pure (AnyQT (QChoice ne))
      "rating" -> AnyQT . QRating <$> o .: "max"
      "number" -> pure (AnyQT QNumber)
      _        -> fail ("unknown question type tag: " ++ show tag)

data Question = Question
  { questionId       :: QuestionId
  , questionLabel    :: Text
  , questionType     :: AnyQuestionType
  , questionRequired :: Bool
  } deriving (Eq, Show)

instance ToJSON Question where
  toJSON q = object
    [ "id"       .= questionId       q
    , "label"    .= questionLabel    q
    , "type"     .= questionType     q
    , "required" .= questionRequired q
    ]

instance FromJSON Question where
  parseJSON = withObject "Question" $ \o ->
    Question
      <$> o .:  "id"
      <*> o .:  "label"
      <*> o .:  "type"
      <*> o .:  "required"

data Page = Page
  { pageId        :: PageId
  , pageTitle     :: Maybe Text
  , pageQuestions :: NonEmpty Question
  } deriving (Eq, Show)

instance ToJSON Page where
  toJSON p = object
    [ "id"        .= pageId        p
    , "title"     .= pageTitle     p
    , "questions" .= NE.toList (pageQuestions p)
    ]

instance FromJSON Page where
  parseJSON = withObject "Page" $ \o -> do
    pid  <- o .:  "id"
    ttl  <- o .:? "title"
    qs   <- o .:  "questions"
    case NE.nonEmpty qs of
      Nothing -> fail "Page requires at least one question"
      Just ne -> pure (Page pid ttl ne)

data Survey = Survey
  { surveyId    :: SurveyId
  , surveyTitle :: Text
  , surveyPages :: NonEmpty Page
  , surveyRules :: [Rule]
  } deriving (Eq, Show)

instance ToJSON Survey where
  toJSON s = object
    [ "id"    .= surveyId    s
    , "title" .= surveyTitle s
    , "pages" .= NE.toList (surveyPages s)
    -- rules omitted for now — not needed for GET/POST form flow
    ]

instance FromJSON Survey where
  parseJSON = withObject "Survey" $ \o -> do
    sid   <- o .: "id"
    title <- o .: "title"
    pages <- o .: "pages"
    case NE.nonEmpty pages of
      Nothing -> fail "Survey requires at least one page"
      Just ne -> pure (Survey sid title ne [])

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

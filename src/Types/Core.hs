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

import Data.Aeson        (ToJSON (..), FromJSON (..), (.=), (.:), withObject)
import Data.Aeson.Types  (Parser)
import qualified Data.Aeson as Aeson
import qualified Data.UUID  as UUID
import Data.Text         (Text)

newtype SurveyId     = SurveyId     UUID.UUID deriving (Eq, Ord, Show)
newtype PageId       = PageId       UUID.UUID deriving (Eq, Ord, Show)
newtype QuestionId   = QuestionId   UUID.UUID deriving (Eq, Ord, Show)
newtype SubmissionId = SubmissionId UUID.UUID deriving (Eq, Ord, Show)

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

-- ---------------------------------------------------------------------------
-- JSON instances
-- ---------------------------------------------------------------------------

uuidToJSON :: UUID.UUID -> Aeson.Value
uuidToJSON = toJSON . UUID.toText

uuidFromJSON :: Aeson.Value -> Parser UUID.UUID
uuidFromJSON v = do
  t <- parseJSON v
  maybe (fail "invalid UUID") pure (UUID.fromText t)

instance ToJSON   SurveyId     where toJSON (SurveyId     u) = uuidToJSON u
instance FromJSON SurveyId     where parseJSON v = SurveyId     <$> uuidFromJSON v
instance ToJSON   PageId       where toJSON (PageId       u) = uuidToJSON u
instance FromJSON PageId       where parseJSON v = PageId       <$> uuidFromJSON v
instance ToJSON   QuestionId   where toJSON (QuestionId   u) = uuidToJSON u
instance FromJSON QuestionId   where parseJSON v = QuestionId   <$> uuidFromJSON v
instance ToJSON   SubmissionId where toJSON (SubmissionId u) = uuidToJSON u
instance FromJSON SubmissionId where parseJSON v = SubmissionId <$> uuidFromJSON v

instance ToJSON Value where
  toJSON (VText   t) = Aeson.object ["type" .= ("text"   :: Text), "value" .= t]
  toJSON (VNumber n) = Aeson.object ["type" .= ("number" :: Text), "value" .= n]
  toJSON (VBool   b) = Aeson.object ["type" .= ("bool"   :: Text), "value" .= b]
  toJSON VNull       = Aeson.object ["type" .= ("null"   :: Text)]

instance FromJSON Value where
  parseJSON = withObject "Value" $ \o -> do
    t <- o .: "type" :: Parser Text
    case t of
      "text"   -> VText   <$> o .: "value"
      "number" -> VNumber <$> o .: "value"
      "bool"   -> VBool   <$> o .: "value"
      "null"   -> pure VNull
      _        -> fail ("unknown Value type: " ++ show t)

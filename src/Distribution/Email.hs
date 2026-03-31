module Distribution.Email
  ( EmailConfig (..)
  , EmailSender
  , SendResult (..)
  , buildInviteEmail
  , sendCampaign
  ) where

import Control.Exception        (SomeException, try)
import Data.Text                (Text)
import qualified Data.Text      as T
import qualified Data.Text.Lazy as LT
import Network.Mail.Mime        (Mail, Address (..))
import Network.Mail.SMTP        ( sendMailWithLogin
                                , simpleMail
                                , plainTextPart
                                , htmlPart
                                )

import Types.Campaign           (Contact (..), Invite (..))

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

data EmailConfig = EmailConfig
  { smtpHost      :: String
  , smtpUser      :: String
  , smtpPassword  :: String
  , fromAddress   :: Address
  , surveyBaseUrl :: Text
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- Pluggable sender
--
-- Injecting the sender as a parameter means:
--   - Tests pass in a no-op or capturing sender — no real SMTP needed
--   - Production passes smtpSender
--   - buildInviteEmail is pure, sendCampaign is IO only at the send boundary
-- ---------------------------------------------------------------------------

type EmailSender = Mail -> IO (Either String ())

-- | Production sender — authenticates and delivers via SMTP.
smtpSender :: EmailConfig -> EmailSender
smtpSender cfg mail = do
  result <- try (sendMailWithLogin
                   (smtpHost     cfg)
                   (smtpUser     cfg)
                   (smtpPassword cfg)
                   mail) :: IO (Either SomeException ())
  pure $ case result of
    Left  e -> Left (show e)
    Right _ -> Right ()

-- ---------------------------------------------------------------------------
-- Result type
-- ---------------------------------------------------------------------------

data SendResult = SendResult
  { srInvite :: Invite
  , srResult :: Either String ()
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- Email construction — pure, no IO
-- ---------------------------------------------------------------------------

-- | Build the invite email for one invite.
buildInviteEmail :: EmailConfig -> Invite -> Mail
buildInviteEmail cfg invite =
  simpleMail
    (fromAddress cfg)
    [toAddr]
    []
    []
    subject
    [ plainTextPart (LT.fromStrict plainBody)
    , htmlPart      (LT.fromStrict htmlBody)
    ]
  where
    contact    = inviteContact invite
    token      = inviteToken   invite
    surveyUrl  = surveyBaseUrl cfg <> "/survey/" <> token
    name       = maybe "there" id (contactName contact)

    toAddr = Address
      { addressName  = contactName contact
      , addressEmail = contactEmail contact
      }

    subject = "You have been invited to take a survey"

    plainBody = T.unlines
      [ "Hello " <> name <> ","
      , ""
      , "You have been invited to complete a survey."
      , "Please visit: " <> surveyUrl
      , ""
      , "This link is unique to you."
      ]

    htmlBody = T.unlines
      [ "<p>Hello " <> name <> ",</p>"
      , "<p>You have been invited to complete a survey.</p>"
      , "<p><a href=\"" <> surveyUrl <> "\">Click here to begin</a></p>"
      , "<p>This link is unique to you.</p>"
      ]

-- ---------------------------------------------------------------------------
-- Campaign sending
-- ---------------------------------------------------------------------------

-- | Send one email per invite, independently.
-- One failure does not stop the rest.
sendCampaign :: EmailSender -> EmailConfig -> [Invite] -> IO [SendResult]
sendCampaign sender cfg = mapM sendOne
  where
    sendOne invite = do
      result <- sender (buildInviteEmail cfg invite)
      pure (SendResult invite result)

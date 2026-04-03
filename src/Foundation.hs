{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
module Foundation where

import Data.Aeson                  (Value, object, (.=))
import Data.Text                   (Text)
import Data.Time                   (getCurrentTime)
import Database.Persist.Sql        (Entity(..), ConnectionPool, SqlBackend, runSqlPool,
                                    getBy)
import Yesod.Core
import Yesod.Form                  (FormMessage, defaultFormMessage)
import Yesod.Persist               (YesodPersist (..), YesodPersistBackend)

import Model
import Analytics.Store             (fetchAnalytics)
import Types.Core                  (SurveyId(..))
import Utils.Conversion            (surveyRecordIdToId)

data App = App
  { appConnPool :: ConnectionPool
  }

mkYesod "App" [parseRoutes|
/                    HomeR       GET
/survey/#Text        SurveyR     GET POST
/analytics/#Text     AnalyticsR  GET
|]

instance Yesod App

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB action = do
    app <- getYesod
    runSqlPool action (appConnPool app)

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|<p>Survey platform|]

getSurveyR :: Text -> Handler Html
getSurveyR sid =
  defaultLayout [whamlet|
    <h1>Survey #{sid}
    <p>Survey rendering will be wired in Phase 9.
  |]

postSurveyR :: Text -> Handler Html
postSurveyR sid = do
  now <- liftIO getCurrentTime
  defaultLayout [whamlet|
    <p>Submission received for survey #{sid} at #{show now}.
  |]

-- | Return analytics for a survey as JSON.
-- Looks up the survey by externalId, loads all answers, runs analytics.
-- Returns 404 if the survey is not found.
getAnalyticsR :: Text -> Handler Value
getAnalyticsR externalId = do
  mSurvey <- runDB (getBy (UniqueExternalSurveyId externalId))
  case mSurvey of
    Nothing -> notFound
    Just (Entity sid _) -> do
      -- convert the database SurveyRecordId to domain SurveyId
      let surveyId = surveyRecordIdToId sid
      result <- runDB (fetchAnalytics surveyId)
      returnJson result
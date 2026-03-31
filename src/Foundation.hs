{-# LANGUAGE QuasiQuotes   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies  #-}
module Foundation where

import Data.Text                   (Text)
import Data.Time                   (getCurrentTime)
import Database.Persist.Sql        (ConnectionPool, SqlBackend, runSqlPool)
import Yesod.Core
import Yesod.Form                  (FormMessage, defaultFormMessage)
import Yesod.Persist               (YesodPersist (..), YesodPersistBackend)

-- ---------------------------------------------------------------------------
-- Foundation type
-- ---------------------------------------------------------------------------

data App = App
  { appConnPool :: ConnectionPool
  }

mkYesod "App" [parseRoutes|
/              HomeR      GET
/survey/#Text  SurveyR    GET POST
/analytics     AnalyticsR GET
|]

-- ---------------------------------------------------------------------------
-- Instances
-- ---------------------------------------------------------------------------

instance Yesod App

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB action = do
    app <- getYesod
    runSqlPool action (appConnPool app)

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

-- ---------------------------------------------------------------------------
-- Handlers (stubs — fully wired in Phase 4)
-- ---------------------------------------------------------------------------

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|<p>Survey platform|]

getSurveyR :: Text -> Handler Html
getSurveyR sid =
  defaultLayout [whamlet|
    <h1>Survey #{sid}
    <p>Survey rendering will be wired in Phase 4.
  |]

postSurveyR :: Text -> Handler Html
postSurveyR sid = do
  now <- liftIO getCurrentTime
  defaultLayout [whamlet|
    <p>Submission received for survey #{sid} at #{show now}.
  |]

getAnalyticsR :: Handler Html
getAnalyticsR =
  defaultLayout [whamlet|
    <h1>Analytics
    <p>Analytics queries will be wired in Phase 4.
  |]

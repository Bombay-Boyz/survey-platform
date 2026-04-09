{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
module Foundation where

import qualified Data.Aeson           as Aeson
import qualified Data.Map.Strict      as Map
import qualified Data.Set             as Set
import qualified Data.Text.Encoding   as TE
import Data.Aeson                     (Value)
import Data.List.NonEmpty             (toList, head)
import Data.Text                      (Text)
import qualified Data.Text            as T
import Data.Time                      (getCurrentTime)
import Data.UUID                      (toText)
import qualified Data.UUID.V4         as UUID4
import Database.Persist.Sql           (Entity (..), ConnectionPool, SqlBackend,
                                       runSqlPool, getBy)
import Prelude                        hiding (head)
import Yesod.Core
import Yesod.Form                     (FormMessage, defaultFormMessage, runFormPost,
                                       FormResult (..), generateFormPost)
import Yesod.Persist                  (YesodPersist (..), YesodPersistBackend)

import Analytics.Engine               (answerToValue)
import Analytics.Store                (fetchAnalytics)
import Forms.Survey                   (SurveyFormResult (..), renderSurveyForm)
import Logic.Engine                   (defaultEvalState, evalRules, visibleQuestions)
import Model
import Submission.Pipeline            (SubmissionInput (..), buildSubmission)
import Submission.Store               (persistSubmission)
import Types.Core                     (QuestionId (..))
import Types.Survey                   (Survey (..), allQuestions, questionId,
                                       SubmissionAnswers (..))
import Utils.Conversion               (surveyRecordIdToId)

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

-- ---------------------------------------------------------------------------
-- Home
-- ---------------------------------------------------------------------------

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|<p>Survey platform|]

-- ---------------------------------------------------------------------------
-- GET /survey/:id
--
-- 1. Look up SurveyRecord by externalId → 404 if not found
-- 2. Decode the stored schemaJson as a Survey domain object
-- 3. Render page 1 via renderSurveyForm
-- ---------------------------------------------------------------------------

getSurveyR :: Text -> Handler Html
getSurveyR externalId = do
  mSurvey <- runDB (getBy (UniqueExternalSurveyId externalId))
  case mSurvey of
    Nothing -> notFound
    Just (Entity _ sr) ->
      case decodeSurvey (surveyRecordSchemaJson sr) of
        Left err -> invalidArgs ["Invalid survey schema: " <> T.pack err]
        Right survey -> do
          let firstPage = Data.List.NonEmpty.head (surveyPages survey)
          (widget, enctype) <- generateFormPost (renderSurveyForm firstPage)
          defaultLayout $ do
            setTitle (toHtml (surveyRecordTitle sr))
            [whamlet|
              <h1>#{surveyRecordTitle sr}
              <form method=post action=@{SurveyR externalId} enctype=#{enctype}>
                ^{widget}
                <button type=submit>Submit
            |]

-- ---------------------------------------------------------------------------
-- POST /survey/:id
--
-- 1. Look up SurveyRecord by externalId → 404 if not found
-- 2. Decode schema → Survey
-- 3. Parse the submitted form (page 1)
-- 4. Run the logic engine; filter out hidden question answers
-- 5. Build and persist SubmissionRecord + AnswerRecords via the pipeline
-- 6. Render confirmation or re-render with validation errors
-- ---------------------------------------------------------------------------

postSurveyR :: Text -> Handler Html
postSurveyR externalId = do
  mSurvey <- runDB (getBy (UniqueExternalSurveyId externalId))
  case mSurvey of
    Nothing -> notFound
    Just (Entity dbKey sr) ->
      case decodeSurvey (surveyRecordSchemaJson sr) of
        Left err -> invalidArgs ["Invalid survey schema: " <> T.pack err]
        Right survey -> do
          let firstPage = Data.List.NonEmpty.head (surveyPages survey)
          ((result, widget), enctype) <- runFormPost (renderSurveyForm firstPage)
          case result of
            FormMissing ->
              defaultLayout [whamlet|
                <p>No form data received.
                <a href=@{SurveyR externalId}>Go back
              |]
            FormFailure errs ->
              defaultLayout $ do
                setTitle (toHtml (surveyRecordTitle sr))
                [whamlet|
                  <h1>#{surveyRecordTitle sr}
                  <ul>
                    $forall e <- errs
                      <li>#{e}
                  <form method=post action=@{SurveyR externalId} enctype=#{enctype}>
                    ^{widget}
                    <button type=submit>Submit
                |]
            FormSuccess (SurveyFormResult answers) -> do
              -- Run logic engine to get visible question set
              -- evalRules needs Map QuestionId Value, so convert Answer → Value
              let qids      = map questionId (allQuestions survey)
                  rawMap    = unAnswers answers
                  valueMap  = Map.map answerToValue rawMap
                  evalSt    = evalRules valueMap (surveyRules survey)
                                        (defaultEvalState qids)
                  visible   = visibleQuestions evalSt
              -- Strip answers for hidden questions before persisting
              let filteredAnswers = SubmissionAnswers
                    (Map.filterWithKey (\qid _ -> Set.member qid visible) rawMap)
              now   <- liftIO getCurrentTime
              extId <- liftIO newSubmissionExternalId
              let surveyDomainId = surveyRecordIdToId dbKey
                  input = SubmissionInput
                    { siSurveyDbId   = surveyDomainId
                    , siExternalId   = extId
                    , siSubmittedAt  = now
                    , siRespondentId = Nothing
                    , siAnswers      = filteredAnswers
                    }
              case buildSubmission input of
                Left pipelineErr ->
                  invalidArgs ["Submission error: " <> T.pack (show pipelineErr)]
                Right output -> do
                  runDB (persistSubmission output)
                  defaultLayout [whamlet|
                    <h1>Thank you!
                    <p>Your response has been recorded.
                    <p>Reference: #{extId}
                  |]

-- ---------------------------------------------------------------------------
-- GET /analytics/:id
-- ---------------------------------------------------------------------------

getAnalyticsR :: Text -> Handler Value
getAnalyticsR externalId = do
  mSurvey <- runDB (getBy (UniqueExternalSurveyId externalId))
  case mSurvey of
    Nothing -> notFound
    Just (Entity sid _) -> do
      let surveyId = surveyRecordIdToId sid
      result <- runDB (fetchAnalytics surveyId)
      returnJson result

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Decode a Survey from its stored JSON schema text.
decodeSurvey :: Text -> Either String Survey
decodeSurvey = Aeson.eitherDecodeStrict . TE.encodeUtf8

-- | Generate a unique external ID for a new submission.
newSubmissionExternalId :: IO Text
newSubmissionExternalId = do
  uuid <- UUID4.nextRandom
  pure ("sub-" <> toText uuid)

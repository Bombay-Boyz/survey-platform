{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module Forms.Survey
  ( SurveyFormResult (..)
  , renderSurveyForm
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text       as T
import Data.Text                 (Text)
import Data.List.NonEmpty        (NonEmpty, toList)
import Yesod.Core
import Yesod.Form

import Types.Core
import Types.Survey

newtype SurveyFormResult = SurveyFormResult SubmissionAnswers

renderSurveyForm
  :: (RenderMessage site FormMessage, Yesod site)
  => Page
  -> Html
  -> MForm (HandlerFor site) (FormResult SurveyFormResult, WidgetFor site ())
renderSurveyForm page extra = do
  pairs <- mapM renderQuestion (toList (pageQuestions page))
  let results = map fst pairs
      views   = map snd pairs
      widget  = [whamlet|
        #{extra}
        $forall v <- views
          <div>
            <label>#{fvLabel v}
            ^{fvInput v}
            $maybe err <- fvErrors v
              <p .error>#{err}
      |]
  case sequenceA results of
    FormSuccess answers ->
      pure
        ( FormSuccess
            (SurveyFormResult
              (SubmissionAnswers (Map.fromList answers)))
        , widget
        )
    FormFailure errs -> pure (FormFailure errs, widget)
    FormMissing      -> pure (FormMissing,      widget)

renderQuestion
  :: (RenderMessage site FormMessage, Yesod site)
  => Question
  -> MForm (HandlerFor site) (FormResult (QuestionId, Answer), FieldView site)
renderQuestion q =
  case questionType q of
    AnyQT QText          -> renderTextField   q
    AnyQT (QChoice opts) -> renderChoiceField q opts
    AnyQT (QRating maxR) -> renderRatingField q maxR
    AnyQT QNumber        -> renderNumberField q

renderTextField
  :: (RenderMessage site FormMessage, Yesod site)
  => Question
  -> MForm (HandlerFor site) (FormResult (QuestionId, Answer), FieldView site)
renderTextField q = do
  (res, v) <- mreq textField (fsFor q) Nothing
  pure (fmap (\t -> (questionId q, AText t)) res, v)

-- Single-choice: selectField returns Text (the chosen option value)
renderChoiceField
  :: (RenderMessage site FormMessage, Yesod site)
  => Question
  -> NonEmpty Text
  -> MForm (HandlerFor site) (FormResult (QuestionId, Answer), FieldView site)
renderChoiceField q opts = do
  let pairs = map (\o -> (o, o)) (toList opts)
  (res, v) <- mreq (selectField (optionsPairs pairs)) (fsFor q) Nothing
  pure (fmap (\c -> (questionId q, AChoice c)) res, v)

-- Rating: selectField returns Int (the chosen rating value)
renderRatingField
  :: (RenderMessage site FormMessage, Yesod site)
  => Question
  -> Int
  -> MForm (HandlerFor site) (FormResult (QuestionId, Answer), FieldView site)
renderRatingField q maxR = do
  let pairs = map (\i -> (T.pack (show i), i)) [1 .. maxR]
  (res, v) <- mreq (selectField (optionsPairs pairs)) (fsFor q) Nothing
  pure (fmap (\r -> (questionId q, ARating r)) res, v)

renderNumberField
  :: (RenderMessage site FormMessage, Yesod site)
  => Question
  -> MForm (HandlerFor site) (FormResult (QuestionId, Answer), FieldView site)
renderNumberField q = do
  (res, v) <- mreq doubleField (fsFor q) Nothing
  pure (fmap (\n -> (questionId q, ANumber n)) res, v)

fsFor
  :: RenderMessage site FormMessage
  => Question
  -> FieldSettings site
fsFor q = FieldSettings
  { fsLabel   = SomeMessage (questionLabel q)
  , fsTooltip = Nothing
  , fsId      = Nothing
  , fsName    = Nothing
  , fsAttrs   = []
  }

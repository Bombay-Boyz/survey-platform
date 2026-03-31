module Analytics.CSV
  ( exportCSV
  , answersToField
  ) where

import qualified Data.ByteString.Lazy    as BL
import qualified Data.Map.Strict         as Map
import qualified Data.Set                as Set
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as TE
import Data.ByteString                   (ByteString)
import Data.Csv                          (Header, ToNamedRecord (..),
                                          encodeByName, header, namedRecord,
                                          (.=))
import Data.Map.Strict                   (Map)
import Data.Set                          (Set)

import Types.Core
import Types.Survey (Answer (..), SubmissionAnswers (..))

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Encode a list of submissions as a CSV lazy ByteString.
--
-- Algorithm:
--   1. Union all QuestionId keys across every submission → column universe U
--   2. Sort U to get a stable, deterministic column order
--   3. Build the cassava Header from sorted U
--   4. Encode each submission as a named record aligned to that header
--
-- Missing answers for a column are encoded as empty string.
--
-- Complexity: O(S * Q * log Q) where S = submissions, Q = unique questions.
exportCSV :: [(SubmissionId, SubmissionAnswers)] -> BL.ByteString
exportCSV submissions =
  encodeByName csvHeader rows
  where
    -- Step 1 & 2: sorted union of all question IDs
    allQids :: [QuestionId]
    allQids = Set.toAscList (foldMap (keySet . unAnswers . snd) submissions)

    -- Step 3: cassava header — each column name is the UUID text of the QID
    csvHeader :: Header
    csvHeader = header ("submission_id" : map qidToBytes allQids)

    -- Step 4: one CsvRow per submission
    rows :: [CsvRow]
    rows = map (uncurry (toCsvRow allQids)) submissions

    keySet :: Map QuestionId Answer -> Set QuestionId
    keySet = Map.keysSet

-- ---------------------------------------------------------------------------
-- Internal row type
-- ---------------------------------------------------------------------------

-- | Carries the ordered column list alongside the answer map so that
-- toNamedRecord can align fields deterministically.
data CsvRow = CsvRow
  { rowSubmissionId :: SubmissionId
  , rowColumnOrder  :: [QuestionId]   -- same list for every row
  , rowAnswers      :: Map QuestionId Answer
  }

toCsvRow
  :: [QuestionId]
  -> SubmissionId
  -> SubmissionAnswers
  -> CsvRow
toCsvRow cols sid (SubmissionAnswers m) = CsvRow sid cols m

instance ToNamedRecord CsvRow where
  toNamedRecord CsvRow{..} =
    namedRecord $
      ("submission_id" .= sidToText rowSubmissionId)
      : [ qidToBytes qid .= maybe "" answersToField (Map.lookup qid rowAnswers)
        | qid <- rowColumnOrder
        ]

-- ---------------------------------------------------------------------------
-- Encoding helpers
-- ---------------------------------------------------------------------------

-- | Render an Answer as a plain-text CSV field value.
answersToField :: Answer -> ByteString
answersToField (AText   t) = TE.encodeUtf8 t
answersToField (AChoice c) = TE.encodeUtf8 c
answersToField (ARating r) = TE.encodeUtf8 (T.pack (show r))
answersToField (ANumber n) = TE.encodeUtf8 (T.pack (show n))

-- | Stable column name: the QuestionId UUID rendered as UTF-8 bytes.
qidToBytes :: QuestionId -> ByteString
qidToBytes (QuestionId uuid) = TE.encodeUtf8 (T.pack (show uuid))

sidToText :: SubmissionId -> ByteString
sidToText (SubmissionId uuid) = TE.encodeUtf8 (T.pack (show uuid))

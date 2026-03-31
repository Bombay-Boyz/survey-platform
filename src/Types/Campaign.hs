module Types.Campaign
  ( -- * Invite status state machine
    InviteStatus (..)
  , validTransition
  , allStatuses

    -- * Domain types
  , Contact (..)
  , Campaign (..)
  , Invite (..)
  , mkInvite
  , transition
  , TransitionError (..)
  ) where

import Data.Text (Text)
import Data.Time (UTCTime)

import Types.Core (SurveyId, SubmissionId)

-- ---------------------------------------------------------------------------
-- Invite status — a finite state machine
--
-- Valid transitions (adjacency list):
--
--   Created  → Sent
--   Sent     → Opened
--   Sent     → Bounced
--   Opened   → Completed
--
-- Every other (from, to) pair is illegal.
-- The table is the single source of truth — no ad-hoc guards anywhere else.
-- ---------------------------------------------------------------------------

data InviteStatus
  = Created
  | Sent
  | Opened
  | Bounced
  | Completed
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | All possible statuses — used by QuickCheck to generate arbitrary values.
allStatuses :: [InviteStatus]
allStatuses = [minBound .. maxBound]

-- | The transition table as an explicit list of (from, to) pairs.
-- This is the ONLY place where valid transitions are defined.
transitionTable :: [(InviteStatus, InviteStatus)]
transitionTable =
  [ (Created, Sent)
  , (Sent,    Opened)
  , (Sent,    Bounced)
  , (Opened,  Completed)
  ]

-- | O(n) lookup in the transition table.
-- n is always 4 (the table never grows), so this is effectively O(1).
validTransition :: InviteStatus -> InviteStatus -> Bool
validTransition from to = (from, to) `elem` transitionTable

-- ---------------------------------------------------------------------------
-- Domain types
-- ---------------------------------------------------------------------------

data Contact = Contact
  { contactEmail :: Text
  , contactName  :: Maybe Text
  } deriving (Eq, Show)

data Campaign = Campaign
  { campaignId       :: Text
  , campaignSurveyId :: SurveyId
  , campaignContacts :: [Contact]
  } deriving (Eq, Show)

data Invite = Invite
  { inviteToken        :: Text
  , inviteContact      :: Contact
  , inviteStatus       :: InviteStatus
  , inviteCreatedAt    :: UTCTime
  , inviteSentAt       :: Maybe UTCTime
  , inviteOpenedAt     :: Maybe UTCTime
  , inviteCompletedAt  :: Maybe UTCTime
  , inviteSubmissionId :: Maybe SubmissionId
  } deriving (Eq, Show)

-- | Smart constructor — all invites start in Created status.
mkInvite :: Text -> Contact -> UTCTime -> Invite
mkInvite token contact now = Invite
  { inviteToken        = token
  , inviteContact      = contact
  , inviteStatus       = Created
  , inviteCreatedAt    = now
  , inviteSentAt       = Nothing
  , inviteOpenedAt     = Nothing
  , inviteCompletedAt  = Nothing
  , inviteSubmissionId = Nothing
  }

-- ---------------------------------------------------------------------------
-- State transition
-- ---------------------------------------------------------------------------

data TransitionError
  = InvalidTransition InviteStatus InviteStatus
  deriving (Eq, Show)

-- | Apply a status transition to an invite, updating timestamps.
-- Returns Left if the transition is not in the transition table.
transition
  :: UTCTime
  -> InviteStatus
  -> Invite
  -> Either TransitionError Invite
transition now to invite
  | not (validTransition (inviteStatus invite) to) =
      Left (InvalidTransition (inviteStatus invite) to)
  | otherwise =
      Right (applyTransition now to invite)

-- | Apply the timestamp update for each target status.
-- This function is only called after validTransition has passed.
applyTransition :: UTCTime -> InviteStatus -> Invite -> Invite
applyTransition now Sent      i = i { inviteStatus = Sent,      inviteSentAt      = Just now }
applyTransition now Opened    i = i { inviteStatus = Opened,    inviteOpenedAt    = Just now }
applyTransition now Completed i = i { inviteStatus = Completed, inviteCompletedAt = Just now }
applyTransition _   Bounced   i = i { inviteStatus = Bounced }
applyTransition _   Created   i = i { inviteStatus = Created }

module Types.Auth
  ( Role (..)
  , Permission (..)
  , hasPermission
  , roleFromText
  , roleToText
  ) where

import Data.Text (Text)

-- ---------------------------------------------------------------------------
-- Roles — ordered by privilege level (higher = more access)
-- ---------------------------------------------------------------------------

data Role
  = Respondent   -- ^ can submit surveys
  | Creator      -- ^ can create/manage surveys, view analytics
  | Admin        -- ^ full access including user management
  deriving (Eq, Ord, Show, Enum, Bounded)

-- ---------------------------------------------------------------------------
-- Permissions — what actions exist in the system
-- ---------------------------------------------------------------------------

data Permission
  = SubmitSurvey     -- ^ POST to /survey/:id
  | ViewSurvey       -- ^ GET  /survey/:id
  | ViewAnalytics    -- ^ GET  /analytics/:id
  | ManageSurveys    -- ^ create, edit, delete surveys
  | ManageUsers      -- ^ admin-only user management
  deriving (Eq, Show, Enum, Bounded)

-- ---------------------------------------------------------------------------
-- Authorisation table
--
-- This is the single source of truth for what each role can do.
-- Every permission check goes through this function — no ad-hoc guards.
--
-- Algorithm: each role has all permissions of roles below it (cumulative).
-- Admin ⊇ Creator ⊇ Respondent.
-- ---------------------------------------------------------------------------

hasPermission :: Role -> Permission -> Bool
hasPermission _         ViewSurvey    = True   -- public
hasPermission _         SubmitSurvey  = True   -- public
hasPermission Creator   ViewAnalytics = True
hasPermission Admin     ViewAnalytics = True
hasPermission Creator   ManageSurveys = True
hasPermission Admin     ManageSurveys = True
hasPermission Admin     ManageUsers   = True
hasPermission _         _             = False

-- ---------------------------------------------------------------------------
-- Text serialisation — for storing role in session / DB
-- ---------------------------------------------------------------------------

roleToText :: Role -> Text
roleToText Respondent = "respondent"
roleToText Creator    = "creator"
roleToText Admin      = "admin"

roleFromText :: Text -> Maybe Role
roleFromText "respondent" = Just Respondent
roleFromText "creator"    = Just Creator
roleFromText "admin"      = Just Admin
roleFromText _            = Nothing

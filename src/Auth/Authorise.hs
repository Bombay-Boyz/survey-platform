module Auth.Authorise
  ( authorise
  , AuthResult (..)
  , RouteAction (..)
  ) where

import Data.Text (Text)

import Types.Auth (Permission (..), Role, hasPermission)

-- ---------------------------------------------------------------------------
-- AuthResult mirrors Yesod's authorisation result
-- ---------------------------------------------------------------------------

data AuthResult
  = Authorised
  | NotAuthenticated   -- ^ no session — redirect to login
  | Forbidden Text     -- ^ authenticated but insufficient role
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Route descriptors — what the handler needs to authorise
-- ---------------------------------------------------------------------------

data RouteAction
  = GetSurvey
  | PostSurvey
  | GetAnalytics
  | Other            -- ^ no access control required
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Authorise a request.
--
-- Arguments:
--   maybeRole  — Nothing if the user is not authenticated
--   action     — which route/method is being accessed
--
-- Algorithm:
--   1. Map the action to the required Permission
--   2. If the permission is public (no role needed), return Authorised
--   3. If no role in session, return NotAuthenticated
--   4. Check hasPermission — return Authorised or Forbidden
-- ---------------------------------------------------------------------------

authorise :: Maybe Role -> RouteAction -> AuthResult
authorise _           Other        = Authorised
authorise _           GetSurvey    = Authorised   -- public
authorise _           PostSurvey   = Authorised   -- public
authorise Nothing     GetAnalytics = NotAuthenticated
authorise (Just role) GetAnalytics
  | hasPermission role ViewAnalytics = Authorised
  | otherwise = Forbidden "Creator or Admin role required"

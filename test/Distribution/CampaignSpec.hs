module Distribution.CampaignSpec (spec) where

import Data.Time              (UTCTime (..), fromGregorian, secondsToDiffTime)

import Test.Hspec
import Test.QuickCheck

import Types.Campaign

-- ---------------------------------------------------------------------------
-- Arbitrary instance — required for QuickCheck property tests
-- ---------------------------------------------------------------------------

instance Arbitrary InviteStatus where
  arbitrary = elements allStatuses

-- ---------------------------------------------------------------------------
-- Fixed test data
-- ---------------------------------------------------------------------------

epoch :: UTCTime
epoch = UTCTime (fromGregorian 2024 1 1) (secondsToDiffTime 0)

later :: UTCTime
later = UTCTime (fromGregorian 2024 1 2) (secondsToDiffTime 0)

testContact :: Contact
testContact = Contact
  { contactEmail = "test@example.com"
  , contactName  = Just "Test User"
  }

freshInvite :: Invite
freshInvite = mkInvite "token-abc" testContact epoch

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "Distribution.Campaign" $ do

  describe "validTransition (state machine table)" $ do

    it "Created → Sent is valid" $
      validTransition Created Sent `shouldBe` True

    it "Sent → Opened is valid" $
      validTransition Sent Opened `shouldBe` True

    it "Sent → Bounced is valid" $
      validTransition Sent Bounced `shouldBe` True

    it "Opened → Completed is valid" $
      validTransition Opened Completed `shouldBe` True

    it "Created → Completed is invalid" $
      validTransition Created Completed `shouldBe` False

    it "Created → Opened is invalid" $
      validTransition Created Opened `shouldBe` False

    it "Bounced → anything is invalid" $
      all (\s -> not (validTransition Bounced s)) allStatuses
        `shouldBe` True

    it "Completed → anything is invalid" $
      all (\s -> not (validTransition Completed s)) allStatuses
        `shouldBe` True

  describe "transition" $ do

    it "Created invite transitions to Sent" $ do
      let result = transition later Sent freshInvite
      result `shouldSatisfy` isRight
      case result of
        Right i -> inviteStatus i `shouldBe` Sent
        Left  _ -> pure ()

    it "Sent invite records sentAt timestamp" $ do
      case transition later Sent freshInvite of
        Right i -> inviteSentAt i `shouldBe` Just later
        Left  e -> expectationFailure (show e)

    it "Sent → Opened records openedAt timestamp" $ do
      case transition later Sent freshInvite of
        Left  e    -> expectationFailure (show e)
        Right sent ->
          case transition later Opened sent of
            Right i -> inviteOpenedAt i `shouldBe` Just later
            Left  e -> expectationFailure (show e)

    it "Opened → Completed records completedAt timestamp" $ do
      let path = do
            sent      <- transition later Sent    freshInvite
            opened    <- transition later Opened  sent
            completed <- transition later Completed opened
            pure completed
      case path of
        Right i -> inviteCompletedAt i `shouldBe` Just later
        Left  e -> expectationFailure (show e)

    it "invalid transition returns Left" $ do
      -- Created → Completed is not in the table
      transition later Completed freshInvite `shouldSatisfy` isLeft

    it "Bounced invite cannot transition further" $
      case transition later Sent freshInvite of
        Left  e    -> expectationFailure (show e)
        Right sent ->
          case transition later Bounced sent of
            Left  e       -> expectationFailure (show e)
            Right bounced ->
              all (\s -> isLeft (transition later s bounced)) allStatuses
                `shouldBe` True

  describe "mkInvite" $ do

    it "starts in Created status" $
      inviteStatus freshInvite `shouldBe` Created

    it "starts with all timestamps as Nothing except createdAt" $ do
      inviteSentAt      freshInvite `shouldBe` Nothing
      inviteOpenedAt    freshInvite `shouldBe` Nothing
      inviteCompletedAt freshInvite `shouldBe` Nothing

  describe "QuickCheck properties" $ do

    it "a status never transitions to itself" $
      property $ \s ->
        not (validTransition s s)

    it "terminal states have no valid outgoing transitions" $
      property $ \s ->
        (s `elem` [Bounced, Completed]) ==>
          all (\t -> not (validTransition s t)) allStatuses

    it "transition rejects all invalid (from, to) pairs" $
      property $ \from to ->
        not (validTransition from to) ==>
          isLeft (transition epoch to (freshInvite { inviteStatus = from }))

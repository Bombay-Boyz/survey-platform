module Auth.AuthoriseSpec (spec) where

import Test.Hspec

import Types.Auth
import Auth.Authorise
import Test.QuickCheck hiding ((==>))
-- -- ---------------------------------------------------------------------------
-- Arbitrary instances for QuickCheck
-- ---------------------------------------------------------------------------

instance Arbitrary Role where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary Permission where
  arbitrary = elements [minBound .. maxBound] ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "Auth.Authorise" $ do

  describe "hasPermission" $ do

    -- ViewSurvey and SubmitSurvey are public
    it "all roles can view a survey" $
      all (\r -> hasPermission r ViewSurvey) [minBound..maxBound]
        `shouldBe` True

    it "all roles can submit a survey" $
      all (\r -> hasPermission r SubmitSurvey) [minBound..maxBound]
        `shouldBe` True

    -- ViewAnalytics requires Creator or above
    it "Respondent cannot view analytics" $
      hasPermission Respondent ViewAnalytics `shouldBe` False

    it "Creator can view analytics" $
      hasPermission Creator ViewAnalytics `shouldBe` True

    it "Admin can view analytics" $
      hasPermission Admin ViewAnalytics `shouldBe` True

    -- ManageSurveys requires Creator or above
    it "Respondent cannot manage surveys" $
      hasPermission Respondent ManageSurveys `shouldBe` False

    it "Creator can manage surveys" $
      hasPermission Creator ManageSurveys `shouldBe` True

    it "Admin can manage surveys" $
      hasPermission Admin ManageSurveys `shouldBe` True

    -- ManageUsers requires Admin only
    it "Respondent cannot manage users" $
      hasPermission Respondent ManageUsers `shouldBe` False

    it "Creator cannot manage users" $
      hasPermission Creator ManageUsers `shouldBe` False

    it "Admin can manage users" $
      hasPermission Admin ManageUsers `shouldBe` True

    -- Role ordering: Admin ⊇ Creator ⊇ Respondent
    it "higher roles have at least the permissions of lower roles" $
      property $ \perm ->
        let respondentCan = hasPermission Respondent perm
            creatorCan    = hasPermission Creator    perm
            adminCan      = hasPermission Admin      perm
        in  (respondentCan ==> creatorCan) && (creatorCan ==> adminCan)

  describe "authorise" $ do

    it "unauthenticated user can view a survey" $
      authorise Nothing GetSurvey `shouldBe` Authorised

    it "unauthenticated user can submit a survey" $
      authorise Nothing PostSurvey `shouldBe` Authorised

    it "unauthenticated user cannot view analytics" $
      authorise Nothing GetAnalytics `shouldBe` NotAuthenticated

    it "Respondent cannot view analytics" $
      authorise (Just Respondent) GetAnalytics
        `shouldBe` Forbidden "Creator or Admin role required"

    it "Creator can view analytics" $
      authorise (Just Creator) GetAnalytics `shouldBe` Authorised

    it "Admin can view analytics" $
      authorise (Just Admin) GetAnalytics `shouldBe` Authorised

    it "Other routes are always authorised" $
      property $ \role ->
        authorise (Just role) Other == Authorised

  describe "roleFromText / roleToText round-trip" $ do

    it "round-trips all roles" $
      all (\r -> roleFromText (roleToText r) == Just r)
          [minBound..maxBound]
        `shouldBe` True

    it "returns Nothing for unknown role text" $
      roleFromText "superuser" `shouldBe` Nothing

-- ---------------------------------------------------------------------------
-- Helper — logical implication for QuickCheck
-- ---------------------------------------------------------------------------

(==>) :: Bool -> Bool -> Bool
False ==> _    = True
True  ==> True = True
True  ==> _    = False
infix 0 ==>

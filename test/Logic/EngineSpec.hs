module Logic.EngineSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import Data.UUID                 (fromWords)

import Test.Hspec

import Types.Core
import Types.Logic
import Logic.Engine

-- ---------------------------------------------------------------------------
-- Fixed test IDs — distinct fromWords values, never reused
-- ---------------------------------------------------------------------------

qA, qB, qC :: QuestionId
qA = QuestionId (fromWords 0 0 0 1)
qB = QuestionId (fromWords 0 0 0 2)
qC = QuestionId (fromWords 0 0 0 3)

pX :: PageId
pX = PageId (fromWords 0 0 1 0)

allQids :: [QuestionId]
allQids = [qA, qB, qC]

-- | Build a rule unsafely for tests — we control the inputs.
rule :: [Condition] -> [AnyAction] -> Int -> Rule
rule conds actions priority =
  case mkRule conds actions priority of
    Right r -> r
    Left  e -> error ("EngineSpec: bad rule: " ++ show e)

-- ---------------------------------------------------------------------------
-- Specs
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "Logic.Engine" $ do

  describe "defaultEvalState" $ do
    it "starts with all questions visible" $
      visibleQuestions (defaultEvalState allQids)
        `shouldBe` Set.fromList allQids
    it "starts with no overrides" $
      overriddenValues (defaultEvalState allQids)
        `shouldBe` Map.empty

  describe "evalOperator" $ do
    it "OpEq matches equal values" $
      evalOperator OpEq (VText "yes") (VText "yes") `shouldBe` True
    it "OpEq rejects unequal values" $
      evalOperator OpEq (VText "yes") (VText "no")  `shouldBe` False
    it "OpNeq rejects equal values" $
      evalOperator OpNeq (VNumber 1) (VNumber 1)    `shouldBe` False
    it "OpGt holds when left > right" $
      evalOperator OpGt  (VNumber 5) (VNumber 3)    `shouldBe` True
    it "OpGt fails when left <= right" $
      evalOperator OpGt  (VNumber 3) (VNumber 5)    `shouldBe` False
    it "OpLt holds when left < right" $
      evalOperator OpLt  (VNumber 2) (VNumber 9)    `shouldBe` True
    it "OpIn holds when value is in list" $
      evalOperator (OpIn [VText "a", VText "b"]) (VText "a") VNull
        `shouldBe` True
    it "OpIn fails when value is absent from list" $
      evalOperator (OpIn [VText "a", VText "b"]) (VText "c") VNull
        `shouldBe` False
    it "numeric operator on non-numbers returns False" $
      evalOperator OpGt (VText "x") (VText "y")     `shouldBe` False

  describe "evalCondition" $ do
    it "returns False for a missing answer (treated as VNull)" $
      let cond = Condition qA OpEq (VText "yes")
      in evalCondition Map.empty cond `shouldBe` False

    it "returns True when answer matches" $
      let answers = Map.singleton qA (VText "yes")
          cond    = Condition qA OpEq (VText "yes")
      in evalCondition answers cond `shouldBe` True

  describe "evalRules" $ do
    it "hides a question when its condition is met" $ do
      let answers = Map.singleton qA (VText "yes")
          cond    = Condition qA OpEq (VText "yes")
          act     = AnyAction (HideQuestion qB)
          r       = rule [cond] [act] 0
          initial = defaultEvalState allQids
          result  = evalRules answers [r] initial
      visibleQuestions result `shouldSatisfy` (Set.notMember qB)

    it "does not hide a question when condition is unmet" $ do
      let answers = Map.singleton qA (VText "no")
          cond    = Condition qA OpEq (VText "yes")
          act     = AnyAction (HideQuestion qB)
          r       = rule [cond] [act] 0
          initial = defaultEvalState allQids
          result  = evalRules answers [r] initial
      visibleQuestions result `shouldSatisfy` (Set.member qB)

    it "applies SetValue override" $ do
      let answers = Map.singleton qA (VNumber 10)
          cond    = Condition qA OpGt (VNumber 5)
          act     = AnyAction (SetValue qB (VText "high"))
          r       = rule [cond] [act] 0
          result  = evalRules answers [r] (defaultEvalState allQids)
      Map.lookup qB (overriddenValues result) `shouldBe` Just (VText "high")

    it "higher priority rule wins over lower priority on same question" $ do
      -- priority 0 hides qC, priority 1 shows qC → net result: visible
      let answers = Map.fromList [(qA, VText "x")]
          cond    = Condition qA OpEq (VText "x")
          r0      = rule [cond] [AnyAction (HideQuestion qC)] 0
          r1      = rule [cond] [AnyAction (ShowQuestion qC)] 1
          initial = defaultEvalState allQids
          result  = evalRules answers [r0, r1] initial
      visibleQuestions result `shouldSatisfy` (Set.member qC)

    it "no rules applied when no answers provided" $ do
      let cond    = Condition qA OpEq (VText "yes")
          act     = AnyAction (HideQuestion qB)
          r       = rule [cond] [act] 0
          initial = defaultEvalState allQids
          result  = evalRules Map.empty [r] initial
      result `shouldBe` initial

    it "JumpToPage does not alter visible questions or overrides" $ do
      let answers = Map.singleton qA (VText "yes")
          cond    = Condition qA OpEq (VText "yes")
          act     = AnyAction (JumpToPage pX)
          r       = rule [cond] [act] 0
          initial = defaultEvalState allQids
          result  = evalRules answers [r] initial
      result `shouldBe` initial

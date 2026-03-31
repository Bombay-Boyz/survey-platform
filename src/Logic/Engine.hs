module Logic.Engine
  ( EvalState (..)
  , defaultEvalState
  , evalRules
  , evalCondition
  , evalOperator
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import Data.List                 (sortOn)
import Data.Map.Strict           (Map)
import Data.Set                  (Set)

import Types.Core
import Types.Logic

-- ---------------------------------------------------------------------------
-- State produced by rule evaluation
-- ---------------------------------------------------------------------------

-- | The result of evaluating all rules against one set of answers.
--
-- visibleQuestions: the set of questions the respondent should see.
-- overriddenValues: values injected by SetValue actions.
--
-- Invariant: a question is either visible or not — it cannot be in
-- both states simultaneously. Actions are applied in priority order
-- so the last matching rule wins on conflict.
data EvalState = EvalState
  { visibleQuestions :: Set QuestionId
  , overriddenValues :: Map QuestionId Value
  } deriving (Eq, Show)

-- | All questions visible, no overrides — used as the starting state.
defaultEvalState :: [QuestionId] -> EvalState
defaultEvalState qids = EvalState
  { visibleQuestions = Set.fromList qids
  , overriddenValues = Map.empty
  }

-- ---------------------------------------------------------------------------
-- Rule evaluation
-- ---------------------------------------------------------------------------

-- | Evaluate all rules against the given answers.
--
-- Algorithm:
--   sort rules by priority (ascending)
--   fold over sorted rules:
--     if ALL conditions hold → apply ALL actions
--     otherwise             → leave state unchanged
--
-- Complexity: O(R * C * log Q) where
--   R = number of rules
--   C = max conditions per rule (≤ 3 by construction)
--   Q = number of questions
evalRules
  :: Map QuestionId Value  -- ^ current answers (possibly with overrides)
  -> [Rule]
  -> EvalState             -- ^ initial state (typically defaultEvalState)
  -> EvalState
evalRules answers rules initial =
  foldl applyRule initial (sortOn rulePriority rules)
  where
    applyRule state rule
      | all (evalCondition answers) (ruleConditions rule) =
          foldl applyAction state (ruleActions rule)
      | otherwise = state

-- ---------------------------------------------------------------------------
-- Action application
-- ---------------------------------------------------------------------------

applyAction :: EvalState -> AnyAction -> EvalState
applyAction state (AnyAction action) = case action of
  ShowQuestion qid ->
    state { visibleQuestions = Set.insert qid (visibleQuestions state) }
  HideQuestion qid ->
    state { visibleQuestions = Set.delete qid (visibleQuestions state) }
  JumpToPage _ ->
    -- JumpToPage is handled at the routing layer, not here.
    -- The engine records no state change for it.
    state
  SetValue qid val ->
    state { overriddenValues = Map.insert qid val (overriddenValues state) }

-- ---------------------------------------------------------------------------
-- Condition evaluation
-- ---------------------------------------------------------------------------

-- | A condition holds when the operator relation between the stored answer
-- and the condition's reference value is satisfied.
-- Missing answers are treated as VNull — they satisfy only OpEq VNull.
evalCondition :: Map QuestionId Value -> Condition -> Bool
evalCondition answers Condition{..} =
  evalOperator condOperator actual condValue
  where
    actual = Map.findWithDefault VNull condQuestion answers

-- | Pure relational operator.
--
-- Numeric comparisons (Gt, Lt, Gte, Lte) only make sense on VNumber.
-- Any other combination returns False rather than throwing.
--
-- OpIn ignores condValue — the candidate list is embedded in the operator.
evalOperator :: Operator -> Value -> Value -> Bool
evalOperator OpEq        a b = a == b
evalOperator OpNeq       a b = a /= b
evalOperator OpGt        (VNumber a) (VNumber b) = a >  b
evalOperator OpLt        (VNumber a) (VNumber b) = a <  b
evalOperator OpGte       (VNumber a) (VNumber b) = a >= b
evalOperator OpLte       (VNumber a) (VNumber b) = a <= b
evalOperator (OpIn vals) a _                     = a `elem` vals
evalOperator _           _ _                     = False

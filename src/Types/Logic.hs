module Types.Logic
  ( Operator (..)
  , Condition (..)
  , ActionTag (..)
  , Action (..)
  , AnyAction (..)
  , Rule
  , mkRule
  , ruleConditions
  , ruleActions
  , rulePriority
  , LogicError (..)
  , validateRules
  ) where

import qualified Data.Set as Set
import Types.Core

data Operator
  = OpEq
  | OpNeq
  | OpGt
  | OpLt
  | OpGte
  | OpLte
  | OpIn [Value]
  deriving (Eq, Show)

data Condition = Condition
  { condQuestion :: QuestionId
  , condOperator :: Operator
  , condValue    :: Value
  } deriving (Eq, Show)

data ActionTag = AShow | AHide | AJump | ASetVal

data Action (t :: ActionTag) where
  ShowQuestion :: QuestionId          -> Action 'AShow
  HideQuestion :: QuestionId          -> Action 'AHide
  JumpToPage   :: PageId              -> Action 'AJump
  SetValue     :: QuestionId -> Value -> Action 'ASetVal

data AnyAction = forall t. AnyAction (Action t)

data Rule = Rule
  { ruleConditions :: [Condition]
  , ruleActions    :: [AnyAction]
  , rulePriority   :: Int
  }

mkRule
  :: [Condition]
  -> [AnyAction]
  -> Int
  -> Either LogicError Rule
mkRule conds actions priority
  | length conds > 3 = Left (TooManyConditions (length conds))
  | null actions     = Left EmptyActions
  | otherwise        = Right (Rule conds actions priority)

data LogicError
  = TooManyConditions Int
  | EmptyActions
  | CyclicDependency  [QuestionId]
  | UnknownQuestion   QuestionId
  deriving (Eq, Show)

validateRules :: [QuestionId] -> [Rule] -> [LogicError]
validateRules knownIds rules =
  unknownRefs knownIds rules ++ detectCycles rules

unknownRefs :: [QuestionId] -> [Rule] -> [LogicError]
unknownRefs known rules =
  [ UnknownQuestion (condQuestion c)
  | rule <- rules
  , c    <- ruleConditions rule
  , condQuestion c `Set.notMember` Set.fromList known
  ]

detectCycles :: [Rule] -> [LogicError]
detectCycles rules =
  [ CyclicDependency [qid]
  | qid <- condPivots
  , isShownBy  qid rules
  , isHiddenBy qid rules
  ]
  where
    condPivots   = map condQuestion (concatMap ruleConditions rules)
    isShownBy  q = any (anyAction q isShow)
    isHiddenBy q = any (anyAction q isHide)
    anyAction q p rule = any (p q) (ruleActions rule)
    isShow :: QuestionId -> AnyAction -> Bool
    isShow q (AnyAction (ShowQuestion t)) = t == q
    isShow _ _                            = False
    isHide :: QuestionId -> AnyAction -> Bool
    isHide q (AnyAction (HideQuestion t)) = t == q
    isHide _ _                            = False
instance Show AnyAction where
  show (AnyAction (ShowQuestion q)) = "ShowQuestion " ++ show q
  show (AnyAction (HideQuestion q)) = "HideQuestion " ++ show q
  show (AnyAction (JumpToPage   p)) = "JumpToPage "   ++ show p
  show (AnyAction (SetValue q v))   = "SetValue "     ++ show q ++ " " ++ show v

instance Eq AnyAction where
  AnyAction (ShowQuestion a) == AnyAction (ShowQuestion b) = a == b
  AnyAction (HideQuestion a) == AnyAction (HideQuestion b) = a == b
  AnyAction (JumpToPage   a) == AnyAction (JumpToPage   b) = a == b
  AnyAction (SetValue a x)   == AnyAction (SetValue b y)   = a == b && x == y
  _                          == _                          = False

instance Show Rule where
  show r = "Rule{priority=" ++ show (rulePriority r) ++ "}"

instance Eq Rule where
  a == b = rulePriority a == rulePriority b
        && ruleConditions a == ruleConditions b

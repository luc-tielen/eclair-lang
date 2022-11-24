{-# LANGUAGE UndecidableInstances #-}

module Eclair.AST.Analysis
  ( Result(..)
  , PointsToAnalysis(..)
  , SemanticInfo(..)
  , SemanticErrors(..)
  , hasSemanticErrors
  , runAnalysis
  , VariableInFact(..)
  , UngroundedVar(..)
  , EmptyModule(..)
  , WildcardInFact(..)
  , WildcardInRuleHead(..)
  , WildcardInAssignment(..)
  , RuleWithContradiction(..)
  , DuplicateOptions(..)
  , OptionsForUnknownRelation(..)
  , IR.NodeId(..)
  , Container
  , computeUsageMapping
  ) where

import qualified Language.Souffle.Interpreted as S
import qualified Language.Souffle.Analysis as S
import qualified Eclair.AST.IR as IR
import qualified Data.Map as Map
import Eclair.Id


type NodeId = IR.NodeId

type Position = Word32

-- The facts submitted to Datalog closely follow the AST structure,
-- but are denormalized so that Datalog can easily process it.

data LitNumber
  = LitNumber NodeId Word32
  deriving stock Generic
  deriving anyclass S.Marshal
  deriving S.Fact via S.FactOptions LitNumber "lit_number" 'S.Input

data LitString
  = LitString NodeId Text
  deriving stock Generic
  deriving anyclass S.Marshal
  deriving S.Fact via S.FactOptions LitString "lit_string" 'S.Input

data Var
  = Var NodeId Id
  deriving stock Generic
  deriving anyclass S.Marshal
  deriving S.Fact via S.FactOptions Var "variable" 'S.Input

newtype Hole
  = Hole NodeId
  deriving stock Generic
  deriving anyclass S.Marshal
  deriving S.Fact via S.FactOptions Hole "hole" 'S.Input

data Assign
  = Assign { assignId :: NodeId, lhsId :: NodeId, rhsId :: NodeId }
  deriving stock Generic
  deriving anyclass S.Marshal
  deriving S.Fact via S.FactOptions Assign "assign" 'S.Input

data Atom
  = Atom NodeId Id
  deriving stock Generic
  deriving anyclass S.Marshal
  deriving S.Fact via S.FactOptions Atom "atom" 'S.Input

data AtomArg
  = AtomArg { atomId :: NodeId, atomArgPos :: Word32, atomArgId :: NodeId }
  deriving stock Generic
  deriving anyclass S.Marshal
  deriving S.Fact via S.FactOptions AtomArg "atom_arg" 'S.Input

data Rule
  = Rule NodeId Id
  deriving stock Generic
  deriving anyclass S.Marshal
  deriving S.Fact via S.FactOptions Rule "rule" 'S.Input

data RuleArg
  = RuleArg { raRuleId :: NodeId, raArgPos :: Word32, raArgId :: NodeId }
  deriving stock Generic
  deriving anyclass S.Marshal
  deriving S.Fact via S.FactOptions RuleArg "rule_arg" 'S.Input

data RuleClause
  = RuleClause { rcRuleId :: NodeId, rcClausePos :: Word32, rcClauseId :: NodeId }
  deriving stock Generic
  deriving anyclass S.Marshal
  deriving S.Fact via S.FactOptions RuleClause "rule_clause" 'S.Input

-- NOTE: not storing types right now, but might be useful later?
data DeclareType
  = DeclareType NodeId Id
  deriving stock Generic
  deriving anyclass S.Marshal
  deriving S.Fact via S.FactOptions DeclareType "declare_type" 'S.Input

data Options
  = Options NodeId Id
  deriving stock Generic
  deriving anyclass S.Marshal
  deriving S.Fact via S.FactOptions Options "options" 'S.Input

newtype InputRelation
  = InputRelation Id
  deriving stock Generic
  deriving anyclass S.Marshal
  deriving S.Fact via S.FactOptions InputRelation "input_fact" 'S.Input

newtype OutputRelation
  = OutputRelation Id
  deriving stock Generic
  deriving anyclass S.Marshal
  deriving S.Fact via S.FactOptions OutputRelation "output_fact" 'S.Input

newtype Module
  = Module NodeId
  deriving stock Generic
  deriving anyclass S.Marshal
  deriving S.Fact via S.FactOptions Module "module" 'S.Input

data ModuleDecl
  = ModuleDecl { moduleId :: NodeId, declId :: NodeId }
  deriving stock Generic
  deriving anyclass S.Marshal
  deriving S.Fact via S.FactOptions ModuleDecl "module_declaration" 'S.Input

data RuleVariable
  = RuleVariable { rvRuleId :: NodeId, rvVarId :: NodeId }
  deriving stock Generic
  deriving anyclass S.Marshal
  deriving S.Fact via S.FactOptions RuleVariable "rule_variable" 'S.Input

data PointsToVar
  = PointsToVar
  { ptRuleId :: NodeId
  , ptVar1Id :: NodeId
  , ptVar2Id :: NodeId
  , ptVar2Name :: Id
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass S.Marshal
  deriving S.Fact via S.FactOptions PointsToVar "points_to_var" 'S.Output

newtype RuleWithContradiction
  = RuleWithContradiction
  { unRuleWithContradiction :: NodeId
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass S.Marshal
  deriving S.Fact via S.FactOptions RuleWithContradiction "rule_with_contradiction" 'S.Output

data VariableInFact
  = VariableInFact NodeId Id
  deriving stock (Generic, Eq, Show)
  deriving anyclass S.Marshal
  deriving S.Fact via S.FactOptions VariableInFact "variable_in_fact" 'S.Output

data UngroundedVar
  = UngroundedVar
  { ungroundedRuleNodeId :: NodeId
  , ungroundedVarNodeId :: NodeId
  , ungroundedVarName :: Id
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass S.Marshal
  deriving S.Fact via S.FactOptions UngroundedVar "ungrounded_variable" 'S.Output

data WildcardInFact
  = WildcardInFact
  { factNodeId :: NodeId
  , factArgId :: NodeId
  , wildcardFactPos :: Position
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass S.Marshal
  deriving S.Fact via S.FactOptions WildcardInFact "wildcard_in_fact" 'S.Output

data WildcardInRuleHead
  = WildcardInRuleHead
  { wildcardRuleNodeId :: NodeId
  , wildcardRuleArgId :: NodeId
  , wildcardRuleHeadPos :: Position
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass S.Marshal
  deriving S.Fact via S.FactOptions WildcardInRuleHead "wildcard_in_rule_head" 'S.Output

data WildcardInAssignment
  = WildcardInAssignment
  { wildcardAssignNodeId :: NodeId
  , wildcardNodeId :: NodeId
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass S.Marshal
  deriving S.Fact via S.FactOptions WildcardInAssignment "wildcard_in_assignment" 'S.Output

newtype EmptyModule
  = EmptyModule NodeId
  deriving stock (Generic, Eq, Show)
  deriving anyclass S.Marshal
  deriving S.Fact via S.FactOptions EmptyModule "empty_module" 'S.Output

data DuplicateOptions
  = DuplicateOptions NodeId NodeId
  deriving stock (Generic, Eq, Show)
  deriving anyclass S.Marshal
  deriving S.Fact via S.FactOptions DuplicateOptions "duplicate_options" 'S.Output

data OptionsForUnknownRelation
  = OptionsForUnknownRelation NodeId Id
  deriving stock (Generic, Eq, Show)
  deriving anyclass S.Marshal
  deriving S.Fact via S.FactOptions OptionsForUnknownRelation "option_for_unknown_relation" 'S.Output

data SemanticAnalysis
  = SemanticAnalysis
  deriving S.Program
  via S.ProgramOptions SemanticAnalysis "semantic_analysis"
      '[ LitNumber
       , LitString
       , Var
       , Hole
       , Assign
       , Atom
       , AtomArg
       , Rule
       , RuleArg
       , RuleClause
       , DeclareType
       , Options
       , InputRelation
       , OutputRelation
       , Module
       , ModuleDecl
       , RuleVariable
       , PointsToVar
       , RuleWithContradiction
       , VariableInFact
       , UngroundedVar
       , EmptyModule
       , WildcardInRuleHead
       , WildcardInFact
       , WildcardInAssignment
       , DuplicateOptions
       , OptionsForUnknownRelation
       ]

-- TODO: change to Vector when finished for performance
type Container = []

-- Points-to analysis of variables.
-- For now only takes variables mapping to other variables into account.
newtype PointsToAnalysis
  = PointsToAnalysis (Map NodeId IR.AST)
  deriving (Eq, Show)

mkPointsToAnalysis :: Container PointsToVar -> PointsToAnalysis
mkPointsToAnalysis =
  PointsToAnalysis . Map.fromList . toList . map toEntry
  where
    toEntry (PointsToVar _ var1Id var2Id var2Name) =
      (var1Id, IR.Var var2Id var2Name)

data SemanticInfo
  = SemanticInfo
  { pointsToAnalysis :: PointsToAnalysis
  , rulesWithContradictions :: Container RuleWithContradiction
  } deriving (Eq, Show)

data Result
  = Result
  { semanticInfo :: SemanticInfo
  , semanticErrors :: SemanticErrors
  }
  deriving (Eq, Show)

data SemanticErrors
  = SemanticErrors
  { emptyModules :: Container EmptyModule
  , variablesInFacts :: Container VariableInFact
  , ungroundedVars :: Container UngroundedVar
  , wildcardsInFacts :: Container WildcardInFact
  , wildcardsInRuleHeads :: Container WildcardInRuleHead
  , wildcardsInAssignments :: Container WildcardInAssignment
  , duplicateOptions :: Container DuplicateOptions
  , optionsForUnknownRelations :: Container OptionsForUnknownRelation
  }
  deriving (Eq, Show, Exception)

hasSemanticErrors :: Result -> Bool
hasSemanticErrors result =
  isNotNull emptyModules ||
  isNotNull variablesInFacts ||
  isNotNull ungroundedVars ||
  isNotNull wildcardsInFacts ||
  isNotNull wildcardsInRuleHeads ||
  isNotNull wildcardsInAssignments ||
  isNotNull duplicateOptions ||
  isNotNull optionsForUnknownRelations
  where
    errs = semanticErrors result
    isNotNull :: (SemanticErrors -> [a]) -> Bool
    isNotNull f = not . null $ f errs

analysis :: S.Handle SemanticAnalysis -> S.Analysis S.SouffleM IR.AST Result
analysis prog = S.mkAnalysis addFacts run getFacts
  where
    addFacts :: IR.AST -> S.SouffleM ()
    addFacts ast = usingReaderT Nothing $ flip (zygo getNodeId) ast $ \case
      IR.LitF nodeId lit ->
        case lit of
          IR.LNumber x ->
            S.addFact prog $ LitNumber nodeId x
          IR.LString x ->
            S.addFact prog $ LitString nodeId x
      IR.PWildcardF nodeId ->
        S.addFact prog $ Var nodeId (Id "_")
      IR.VarF nodeId var -> do
        S.addFact prog $ Var nodeId var
        maybeRuleId <- ask
        for_ maybeRuleId $ \ruleId ->
          S.addFact prog $ RuleVariable ruleId nodeId
      IR.HoleF nodeId ->
        S.addFact prog $ Hole nodeId
      IR.AssignF nodeId (lhsId', lhsAction) (rhsId', rhsAction) -> do
        S.addFact prog $ Assign nodeId lhsId' rhsId'
        lhsAction
        rhsAction
      IR.AtomF nodeId atom (unzip -> (argNodeIds, actions)) -> do
        S.addFact prog $ Atom nodeId atom
        S.addFacts prog $ mapWithPos (AtomArg nodeId) argNodeIds
        sequence_ actions
      IR.RuleF nodeId rule ruleArgs ruleClauses -> do
        let (argNodeIds, argActions) = unzip ruleArgs
            (clauseNodeIds, clauseActions) = unzip ruleClauses
        S.addFact prog $ Rule nodeId rule
        S.addFacts prog $ mapWithPos (RuleArg nodeId) argNodeIds
        S.addFacts prog $ mapWithPos (RuleClause nodeId) clauseNodeIds
        local (const $ Just nodeId) $ do
          sequence_ argActions
          sequence_ clauseActions
      IR.OptionsF nodeId name usageMode -> do
        S.addFact prog $ Options nodeId name
        case usageMode of
          IR.Input ->
            S.addFact prog $ InputRelation name
          IR.Output ->
            S.addFact prog $ OutputRelation name
          IR.InputOutput -> do
            S.addFact prog $ InputRelation name
            S.addFact prog $ OutputRelation name
          IR.Internal ->
            pass
      IR.DeclareTypeF nodeId ty _ ->
        S.addFact prog $ DeclareType nodeId ty
      IR.ModuleF nodeId (unzip -> (declNodeIds, actions)) -> do
        S.addFact prog $ Module nodeId
        S.addFacts prog $ map (ModuleDecl nodeId) declNodeIds
        sequence_ actions

    run :: S.SouffleM ()
    run = do
      -- TODO: optimal CPU core count? just use all cores?
      S.setNumThreads prog 8
      S.run prog

    getFacts :: S.SouffleM Result
    getFacts = do
      info <- SemanticInfo <$> (mkPointsToAnalysis <$> S.getFacts prog)
                           <*> S.getFacts prog
      errs <- SemanticErrors <$> S.getFacts prog
                             <*> S.getFacts prog
                             <*> S.getFacts prog
                             <*> S.getFacts prog
                             <*> S.getFacts prog
                             <*> S.getFacts prog
                             <*> S.getFacts prog
                             <*> S.getFacts prog
      pure $ Result info errs

    getNodeId :: IR.ASTF NodeId -> NodeId
    getNodeId = \case
      IR.LitF nodeId _ -> nodeId
      IR.VarF nodeId _ -> nodeId
      IR.HoleF nodeId -> nodeId
      IR.AssignF nodeId _ _ -> nodeId
      IR.AtomF nodeId _ _ -> nodeId
      IR.RuleF nodeId _ _ _ -> nodeId
      IR.OptionsF nodeId _ _ -> nodeId
      IR.DeclareTypeF nodeId _ _ -> nodeId
      IR.ModuleF nodeId _ -> nodeId

    mapWithPos :: (Word32 -> a -> b) -> [a] -> [b]
    mapWithPos g = zipWith g [0..]

runAnalysis :: IR.AST -> IO Result
runAnalysis ast = S.runSouffle SemanticAnalysis $ \case
  Nothing -> panic "Failed to load Souffle during semantic analysis!"
  Just prog -> S.execAnalysis (analysis prog) ast

computeUsageMapping :: IR.AST -> Map Id IR.UsageMode
computeUsageMapping ast =
  Map.fromList pairs
  where
    pairs = flip cata ast $ \case
      IR.OptionsF _ name mode ->
        one (name, mode)
      astf ->
        fold astf

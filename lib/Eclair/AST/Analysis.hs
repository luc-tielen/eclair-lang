{-# LANGUAGE UndecidableInstances #-}

module Eclair.AST.Analysis
  ( Result(..)
  , SemanticInfo(..)
  , SemanticErrors(..)
  , hasSemanticErrors
  , runAnalysis
  , UngroundedVar(..)
  , WildcardInFact(..)
  , WildcardInRuleHead(..)
  , WildcardInConstraint(..)
  , WildcardInBinOp(..)
  , WildcardInExtern(..)
  , UnconstrainedRuleVar(..)
  , DeadCode(..)
  , DeadInternalRelation(..)
  , NoOutputRelation(..)
  , ConflictingDefinitionGroup(..)
  , ExternUsedAsFact(..)
  , ExternUsedAsRule(..)
  , CyclicNegation(..)
  , NodeId(..)
  , Container
  , computeUsageMapping
  ) where

import qualified Data.List.NonEmpty as NE
import Data.List.Extra (nubOrdOn)
import qualified Eclair.Datalog as DL
import qualified Eclair.AST.IR as IR
import qualified Data.Map as Map
import Eclair.Common.Id
import Eclair.Common.Location (NodeId(..))


type Position = Word32

-- The facts submitted to Datalog closely follow the AST structure,
-- but are denormalized so that Datalog can easily process it.

data LitNumber
  = LitNumber NodeId Word32
  deriving stock Generic
  deriving anyclass DL.Marshal
  deriving DL.Fact via DL.FactOptions LitNumber 'DL.Input "lit_number"

data LitString
  = LitString NodeId Text
  deriving stock Generic
  deriving anyclass DL.Marshal
  deriving DL.Fact via DL.FactOptions LitString 'DL.Input "lit_string"

data Var
  = Var NodeId Id
  deriving stock Generic
  deriving anyclass DL.Marshal
  deriving DL.Fact via DL.FactOptions Var 'DL.Input "variable"

newtype Hole
  = Hole NodeId
  deriving stock Generic
  deriving anyclass DL.Marshal
  deriving DL.Fact via DL.FactOptions Hole 'DL.Input "hole"

data Constraint
  = Constraint
  { constraintId :: NodeId
  , constraintOperator :: Text
  , constraintLhsId :: NodeId
  , constraintRhsId :: NodeId
  }
  deriving stock Generic
  deriving anyclass DL.Marshal
  deriving DL.Fact via DL.FactOptions Constraint 'DL.Input "constraint"

data BinOp
  = BinOp
  { binOpId :: NodeId
  , op :: Text
  , binOpLhsId :: NodeId
  , binOpRhsId :: NodeId
  }
  deriving stock Generic
  deriving anyclass DL.Marshal
  deriving DL.Fact via DL.FactOptions BinOp 'DL.Input "binop"

data Atom
  = Atom NodeId Id
  deriving stock Generic
  deriving anyclass DL.Marshal
  deriving DL.Fact via DL.FactOptions Atom 'DL.Input "atom"

data AtomArg
  = AtomArg { atomId :: NodeId, atomArgPos :: Word32, atomArgId :: NodeId }
  deriving stock Generic
  deriving anyclass DL.Marshal
  deriving DL.Fact via DL.FactOptions AtomArg 'DL.Input "atom_arg"

data Rule
  = Rule NodeId Id
  deriving stock Generic
  deriving anyclass DL.Marshal
  deriving DL.Fact via DL.FactOptions Rule 'DL.Input "rule"

data RuleArg
  = RuleArg { raRuleId :: NodeId, raArgPos :: Word32, raArgId :: NodeId }
  deriving stock Generic
  deriving anyclass DL.Marshal
  deriving DL.Fact via DL.FactOptions RuleArg 'DL.Input "rule_arg"

data RuleClause
  = RuleClause { rcRuleId :: NodeId, rcClausePos :: Word32, rcClauseId :: NodeId }
  deriving stock Generic
  deriving anyclass DL.Marshal
  deriving DL.Fact via DL.FactOptions RuleClause 'DL.Input "rule_clause"

data Negation
  = Negation
  { negationNodeId :: NodeId
  , negationInnerNodeId :: NodeId
  }
  deriving stock Generic
  deriving anyclass DL.Marshal
  deriving DL.Fact via DL.FactOptions Negation 'DL.Input "negation"

-- NOTE: not storing types right now, but might be useful later?
data DeclareType
  = DeclareType NodeId Id
  deriving stock Generic
  deriving anyclass DL.Marshal
  deriving DL.Fact via DL.FactOptions DeclareType 'DL.Input "declare_type"

data ExternDefinition
  = ExternDefinition NodeId Id
  deriving stock Generic
  deriving anyclass DL.Marshal
  deriving DL.Fact via DL.FactOptions ExternDefinition 'DL.Input "extern_definition"

newtype InputRelation
  = InputRelation Id
  deriving stock Generic
  deriving anyclass DL.Marshal
  deriving DL.Fact via DL.FactOptions InputRelation 'DL.Input "input_relation"

newtype OutputRelation
  = OutputRelation Id
  deriving stock Generic
  deriving anyclass DL.Marshal
  deriving DL.Fact via DL.FactOptions OutputRelation 'DL.Input "output_relation"

newtype InternalRelation
  = InternalRelation Id
  deriving stock Generic
  deriving anyclass DL.Marshal
  deriving DL.Fact via DL.FactOptions InternalRelation 'DL.Input "internal_relation"

newtype Module
  = Module NodeId
  deriving stock Generic
  deriving anyclass DL.Marshal
  deriving DL.Fact via DL.FactOptions Module 'DL.Input "module"

data ModuleDecl
  = ModuleDecl { moduleId :: NodeId, declId :: NodeId }
  deriving stock Generic
  deriving anyclass DL.Marshal
  deriving DL.Fact via DL.FactOptions ModuleDecl 'DL.Input "module_declaration"

data ScopedValue
  = ScopedValue { svScopeId :: NodeId, svNodeId :: NodeId }
  deriving stock Generic
  deriving anyclass DL.Marshal
  deriving DL.Fact via DL.FactOptions ScopedValue 'DL.Input "scoped_value"

data UngroundedVar loc
  = UngroundedVar
  { ungroundedRuleLoc :: loc
  , ungroundedVarLoc :: loc
  , ungroundedVarName :: Id
  }
  deriving stock (Generic, Eq, Functor)
  deriving anyclass DL.Marshal
  deriving DL.Fact via DL.FactOptions (UngroundedVar loc) 'DL.Output "ungrounded_variable"

data WildcardInFact loc
  = WildcardInFact
  { factLoc :: loc
  , factArgLoc :: loc
  , wildcardFactPos :: Position
  }
  deriving stock (Generic, Eq, Functor)
  deriving anyclass DL.Marshal
  deriving DL.Fact via DL.FactOptions (WildcardInFact loc) 'DL.Output "wildcard_in_fact"

data WildcardInRuleHead loc
  = WildcardInRuleHead
  { wildcardRuleLoc :: loc
  , wildcardRuleArgLoc :: loc
  , wildcardRuleHeadPos :: Position
  }
  deriving stock (Generic, Eq, Functor)
  deriving anyclass DL.Marshal
  deriving DL.Fact via DL.FactOptions (WildcardInRuleHead loc) 'DL.Output "wildcard_in_rule_head"

data WildcardInConstraint loc
  = WildcardInConstraint
  { wildcardConstraintLoc :: loc
  , wildcardConstraintPos :: loc
  }
  deriving stock (Generic, Eq, Functor)
  deriving anyclass DL.Marshal
  deriving DL.Fact via DL.FactOptions (WildcardInConstraint loc) 'DL.Output "wildcard_in_constraint"

data WildcardInBinOp loc
  = WildcardInBinOp
  { wildcardBinOpLoc :: loc
  , wildcardBinOpPos :: loc
  }
  deriving stock (Generic, Eq, Functor)
  deriving anyclass DL.Marshal
  deriving DL.Fact via DL.FactOptions (WildcardInBinOp loc) 'DL.Output "wildcard_in_binop"

data WildcardInExtern loc
  = WildcardInExtern
  { wildcardExternAtomLoc :: loc
  , wildcardExternAtomArgLoc :: loc
  , wildcardExternArgPos :: Position
  }
  deriving stock (Generic, Eq, Functor)
  deriving anyclass DL.Marshal
  deriving DL.Fact via DL.FactOptions (WildcardInExtern loc) 'DL.Output "wildcard_in_extern"

data UnconstrainedRuleVar loc
  = UnconstrainedRuleVar
  { urvRuleLoc :: loc
  , urvVarLoc :: loc
  , urvVarName :: Id
  }
  deriving stock (Generic, Eq, Functor)
  deriving anyclass DL.Marshal
  deriving DL.Fact via DL.FactOptions (UnconstrainedRuleVar loc) 'DL.Output "unconstrained_rule_var"

newtype DeadCode
  = DeadCode { unDeadCode :: NodeId }
  deriving stock (Generic, Eq)
  deriving anyclass DL.Marshal
  deriving DL.Fact via DL.FactOptions DeadCode 'DL.Output "dead_code"

newtype NoOutputRelation loc
  = NoOutputRelation loc
  deriving stock (Generic, Eq, Functor)
  deriving anyclass DL.Marshal
  deriving DL.Fact via DL.FactOptions (NoOutputRelation loc) 'DL.Output "no_output_relation"

data DeadInternalRelation loc
  = DeadInternalRelation loc Id
  deriving stock (Generic, Eq, Functor)
  deriving anyclass DL.Marshal
  deriving DL.Fact via DL.FactOptions (DeadInternalRelation loc) 'DL.Output "dead_internal_relation"

data ConflictingDefinitions loc
  = ConflictingDefinitions
  { cdFirstLoc :: loc
  , cdSecondLoc :: loc
  , cdName :: Id
  }
  deriving stock (Generic, Eq, Functor)
  deriving anyclass DL.Marshal
  deriving DL.Fact via DL.FactOptions (ConflictingDefinitions loc) 'DL.Output "conflicting_definitions"

data ConflictingDefinitionGroup loc
  = ConflictingDefinitionGroup
  { cdgName :: Id
  , cdgLocs :: NonEmpty loc
  } deriving stock (Eq, Functor)

data ExternUsedAsFact loc
  = ExternUsedAsFact
  { externAsFactLoc :: loc
  , externAsFactExternLoc :: loc
  , externAsFactName :: Id
  }
  deriving stock (Generic, Eq, Functor)
  deriving anyclass DL.Marshal
  deriving DL.Fact via DL.FactOptions (ExternUsedAsFact loc) 'DL.Output "extern_used_as_fact"

data ExternUsedAsRule loc
  = ExternUsedAsRule
  { externAsRuleLoc :: loc
  , externAsRuleExternLoc :: loc
  , externAsRuleName :: Id
  }
  deriving stock (Generic, Eq, Functor)
  deriving anyclass DL.Marshal
  deriving DL.Fact via DL.FactOptions (ExternUsedAsRule loc) 'DL.Output "extern_used_as_rule"

newtype CyclicNegation loc
  = CyclicNegation loc
  deriving stock (Generic, Eq, Functor)
  deriving anyclass DL.Marshal
  deriving DL.Fact via DL.FactOptions (CyclicNegation loc) 'DL.Output "cyclic_negation"

data SemanticAnalysis
  = SemanticAnalysis
  deriving DL.Program
  via DL.ProgramOptions SemanticAnalysis "semantic_analysis"
      '[ LitNumber
       , LitString
       , Var
       , Hole
       , Constraint
       , BinOp
       , Atom
       , AtomArg
       , Rule
       , RuleArg
       , RuleClause
       , Negation
       , DeclareType
       , ExternDefinition
       , InputRelation
       , OutputRelation
       , InternalRelation
       , Module
       , ModuleDecl
       , ScopedValue
       , UngroundedVar NodeId
       , WildcardInRuleHead NodeId
       , WildcardInFact NodeId
       , WildcardInConstraint NodeId
       , WildcardInBinOp NodeId
       , WildcardInExtern NodeId
       , UnconstrainedRuleVar NodeId
       , DeadCode
       , NoOutputRelation NodeId
       , DeadInternalRelation NodeId
       , ConflictingDefinitions NodeId
       , ExternUsedAsFact NodeId
       , ExternUsedAsRule NodeId
       , CyclicNegation NodeId
       ]

-- TODO: change to Vector when finished for performance
type Container = []

newtype SemanticInfo
  = SemanticInfo
  { deadCodeIds :: Container DeadCode
  } deriving Eq

data Result
  = Result
  { semanticInfo :: SemanticInfo
  , semanticErrors :: SemanticErrors NodeId
  }
  deriving Eq

data SemanticErrors loc
  = SemanticErrors
  { ungroundedVars :: Container (UngroundedVar loc)
  , wildcardsInFacts :: Container (WildcardInFact loc)
  , wildcardsInRuleHeads :: Container (WildcardInRuleHead loc)
  , wildcardsInConstraints :: Container (WildcardInConstraint loc)
  , wildcardsInBinOps :: Container (WildcardInBinOp loc)
  , wildcardsInExternAtoms :: Container (WildcardInExtern loc)
  , unconstrainedVars :: Container (UnconstrainedRuleVar loc)
  , deadInternalRelations :: Container (DeadInternalRelation loc)
  , noOutputRelations :: Container (NoOutputRelation loc)
  , conflictingDefinitions :: Container (ConflictingDefinitionGroup loc)
  , externsUsedAsFact :: Container (ExternUsedAsFact loc)
  , externsUsedAsRule :: Container (ExternUsedAsRule loc)
  , cyclicNegations :: Container (CyclicNegation loc)
  }
  deriving (Eq, Functor)

hasSemanticErrors :: Result -> Bool
hasSemanticErrors result =
  isNotNull ungroundedVars ||
  isNotNull wildcardsInFacts ||
  isNotNull wildcardsInRuleHeads ||
  isNotNull wildcardsInConstraints ||
  isNotNull wildcardsInBinOps ||
  isNotNull wildcardsInExternAtoms ||
  isNotNull unconstrainedVars ||
  isNotNull deadInternalRelations ||
  isNotNull noOutputRelations ||
  isNotNull conflictingDefinitions ||
  isNotNull externsUsedAsFact ||
  isNotNull cyclicNegations
  where
    errs = semanticErrors result
    isNotNull :: (SemanticErrors NodeId -> [a]) -> Bool
    isNotNull f = not . null $ f errs

analysis :: Word -> DL.Handle SemanticAnalysis -> DL.Analysis DL.DatalogM IR.AST Result
analysis numCores prog = DL.mkAnalysis addFacts run getFacts
  where
    addFacts :: IR.AST -> DL.DatalogM ()
    addFacts ast = usingReaderT Nothing $ flip (zygo IR.getNodeIdF) ast $ \case
      IR.LitF nodeId lit -> do
        mScopeId <- ask
        for_ mScopeId $ \scopeId ->
          DL.addFact prog $ ScopedValue scopeId nodeId
        case lit of
          IR.LNumber x ->
            DL.addFact prog $ LitNumber nodeId x
          IR.LString x ->
            DL.addFact prog $ LitString nodeId x
      IR.PWildcardF nodeId ->
        DL.addFact prog $ Var nodeId (Id "_")
      IR.VarF nodeId var -> do
        DL.addFact prog $ Var nodeId var
        mScopeId <- ask
        for_ mScopeId $ \scopeId ->
          DL.addFact prog $ ScopedValue scopeId nodeId
      IR.HoleF nodeId ->
        DL.addFact prog $ Hole nodeId
      IR.BinOpF nodeId arithOp (lhsId', lhsAction) (rhsId', rhsAction) -> do
        let textualOp = case arithOp of
              IR.Plus -> "+"
              IR.Minus -> "-"
              IR.Multiply -> "*"
              IR.Divide -> "/"
        mScopeId <- ask
        for_ mScopeId $ \scopeId ->
          DL.addFact prog $ ScopedValue scopeId nodeId
        DL.addFact prog $ BinOp nodeId textualOp lhsId' rhsId'
        lhsAction
        rhsAction
      IR.ConstraintF nodeId constraintOp (lhsId', lhsAction) (rhsId', rhsAction) -> do
        let textualOp = case constraintOp of
              IR.Equals -> "="
              IR.NotEquals -> "!="
              IR.LessThan -> "<"
              IR.LessOrEqual -> "<="
              IR.GreaterThan -> "<"
              IR.GreaterOrEqual -> "<="
        DL.addFact prog $ Constraint nodeId textualOp lhsId' rhsId'
        lhsAction
        rhsAction
      IR.NotF nodeId (innerNodeId, action) -> do
        DL.addFact prog $ Negation nodeId innerNodeId
        local (const $ Just nodeId) action
      IR.AtomF nodeId atom (unzip -> (argNodeIds, actions)) -> do
        DL.addFact prog $ Atom nodeId atom
        mScopeId <- ask
        DL.addFacts prog $ mapWithPos (AtomArg nodeId) argNodeIds

        for_ mScopeId $ \scopeId ->
          DL.addFact prog $ ScopedValue scopeId nodeId

        let maybeAddScope =
              if isJust mScopeId
                then id
                else local (const $ Just nodeId)
        maybeAddScope $ sequence_ actions
      IR.RuleF nodeId rule ruleArgs ruleClauses -> do
        let (argNodeIds, argActions) = unzip ruleArgs
            (clauseNodeIds, clauseActions) = unzip ruleClauses
        DL.addFact prog $ Rule nodeId rule
        DL.addFacts prog $ mapWithPos (RuleArg nodeId) argNodeIds
        DL.addFacts prog $ mapWithPos (RuleClause nodeId) clauseNodeIds
        local (const $ Just nodeId) $ do
          sequence_ argActions
          sequence_ clauseActions
      IR.ExternDefinitionF nodeId name _ _ -> do
        DL.addFact prog $ ExternDefinition nodeId name
      IR.DeclareTypeF nodeId name _ usageMode -> do
        DL.addFact prog $ DeclareType nodeId name

        case usageMode of
          IR.Input ->
            DL.addFact prog $ InputRelation name
          IR.Output ->
            DL.addFact prog $ OutputRelation name
          IR.InputOutput -> do
            DL.addFact prog $ InputRelation name
            DL.addFact prog $ OutputRelation name
          IR.Internal ->
            DL.addFact prog $ InternalRelation name
      IR.ModuleF nodeId (unzip -> (declNodeIds, actions)) -> do
        DL.addFact prog $ Module nodeId
        DL.addFacts prog $ map (ModuleDecl nodeId) declNodeIds
        sequence_ actions

    run :: DL.DatalogM ()
    run = do
      DL.setNumThreads prog (fromIntegral numCores)
      DL.run prog

    getFacts :: DL.DatalogM Result
    getFacts = do
      info <- SemanticInfo <$> DL.getFacts prog
      errs <- SemanticErrors <$> DL.getFacts prog
                             <*> DL.getFacts prog
                             <*> DL.getFacts prog
                             <*> DL.getFacts prog
                             <*> DL.getFacts prog
                             <*> DL.getFacts prog
                             <*> DL.getFacts prog
                             <*> DL.getFacts prog
                             <*> DL.getFacts prog
                             <*> (groupConflicts <$> DL.getFacts prog)
                             <*> DL.getFacts prog
                             <*> DL.getFacts prog
                             <*> DL.getFacts prog
      pure $ Result info errs

    mapWithPos :: (Word32 -> a -> b) -> [a] -> [b]
    mapWithPos g = zipWith g [0..]

groupConflicts :: Container (ConflictingDefinitions NodeId) -> Container (ConflictingDefinitionGroup NodeId)
groupConflicts conflicts =
  conflicts
  & sortWith sameConflict
  & groupBy ((==) `on` sameConflict)
  & map (\cg ->
    let firstConflict = head cg
        declName = cdName firstConflict
        locs = NE.cons (cdFirstLoc firstConflict) (map cdSecondLoc cg)
     in ConflictingDefinitionGroup declName locs
  )
  & nubOrdOn cdgName
  where
    sameConflict = cdName &&& cdFirstLoc

runAnalysis :: Word -> IR.AST -> IO Result
runAnalysis numCores ast = DL.runDatalog SemanticAnalysis $ \case
  Nothing -> panic "Failed to load semantic analysis!"
  Just prog -> DL.execAnalysis (analysis numCores prog) ast

computeUsageMapping :: IR.AST -> Map Id IR.UsageMode
computeUsageMapping ast =
  Map.fromList pairs
  where
    pairs = flip cata ast $ \case
      IR.DeclareTypeF _ name _ mode ->
        one (name, mode)
      astf ->
        fold astf

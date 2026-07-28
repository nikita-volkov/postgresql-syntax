{-# LANGUAGE AllowAmbiguousTypes #-}

module Properties
  ( spec,
  )
where

import qualified Data.Proxy as Proxy
import qualified Data.Text as Text
import qualified Data.Typeable as Typeable
import PostgresqlSyntax
import Prelude
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (counterexample, (===))
import qualified Test.QuickCheck as Qc

-- | The property suite: a parse/render round-trip plus a bounded
-- 'Arbitrary' generator for every AST node type.
spec :: Spec
spec = do
  -- Each AST node type gets its own describe group, named after the type,
  -- holding its parse/render round-trip ('IsAst') and generator-bound
  -- ('Arbitrary') properties.
  fullSpec @AExpr
  fullSpec @AExprReversableOp
  fullSpec @AexprConst
  fullSpec @AliasClause
  fullSpec @AllOp
  fullSpec @AnyName
  fullSpec @AnyOperator
  fullSpec @ArrayBounds
  fullSpec @ArrayExpr
  fullSpec @ArrayExprList
  fullSpec @AscDesc
  fullSpec @Attrs
  fullSpec @BExpr
  fullSpec @BExprIsOp
  fullSpec @Bconst
  fullSpec @Bit
  fullSpec @CExpr
  fullSpec @CallStmt
  fullSpec @CaseExpr
  fullSpec @Character
  fullSpec @Columnref
  fullSpec @CommonTableExpr
  fullSpec @ConfExpr
  fullSpec @ConstCharacter
  fullSpec @ConstDatetime
  fullSpec @ConstTypename
  fullSpec @DeleteStmt
  fullSpec @ExplicitRow
  fullSpec @ExprList
  fullSpec @ExtractArg
  fullSpec @ExtractList
  fullSpec @Fconst
  fullSpec @ForLockingClause
  fullSpec @ForLockingItem
  fullSpec @ForLockingStrength
  fullSpec @FrameBound
  fullSpec @FrameClause
  fullSpec @FrameClauseMode
  fullSpec @FrameExtent
  fullSpec @FuncAliasClause
  fullSpec @FuncApplication
  fullSpec @FuncApplicationParams
  fullSpec @FuncArgExpr
  fullSpec @FuncConstArgs
  fullSpec @FuncExpr
  fullSpec @FuncExprCommonSubexpr
  fullSpec @FuncExprWindowless
  fullSpec @FuncName
  fullSpec @FuncTable
  fullSpec @GenericType
  fullSpec @GroupByItem
  fullSpec @Iconst
  fullSpec @Ident
  fullSpec @ImplicitRow
  fullSpec @InExpr
  fullSpec @IndexElem
  fullSpec @IndexElemDef
  fullSpec @IndexParams
  fullSpec @Indirection
  fullSpec @IndirectionEl
  fullSpec @InsertColumnItem
  fullSpec @InsertColumnList
  fullSpec @InsertRest
  fullSpec @InsertStmt
  fullSpec @InsertTarget
  fullSpec @Interval
  fullSpec @IntervalSecond
  fullSpec @JoinMeth
  fullSpec @JoinQual
  fullSpec @JoinType
  fullSpec @JoinedTable
  fullSpec @LimitClause
  fullSpec @MathOp
  fullSpec @NameList
  fullSpec @NullsOrder
  fullSpec @Numeric
  fullSpec @OffsetClause
  fullSpec @OnConflict
  fullSpec @OnConflictDo
  fullSpec @PostgresqlSyntax.Op
  fullSpec @OptOrdinality
  fullSpec @OptTempTableName
  fullSpec @OverClause
  fullSpec @OverlayList
  fullSpec @OverrideKind
  fullSpec @PositionList
  fullSpec @PreparableStmt
  fullSpec @QualAllOp
  fullSpec @QualOp
  fullSpec @QualifiedName
  fullSpec @RelationExpr
  fullSpec @RelationExprOptAlias
  fullSpec @Row
  fullSpec @RowsfromItem
  fullSpec @RowsfromList
  fullSpec @Sconst
  fullSpec @SelectBinOp
  fullSpec @SelectClause
  fullSpec @SelectFetchFirstValue
  fullSpec @SelectLimit
  fullSpec @SelectLimitValue
  fullSpec @SelectNoParens
  fullSpec @SelectStmt
  fullSpec @SelectWithParens
  fullSpec @SetClause
  fullSpec @SetClauseList
  fullSpec @SetTarget
  fullSpec @SetTargetList
  fullSpec @SimpleSelect
  fullSpec @SimpleTypename
  fullSpec @SortBy
  fullSpec @SortClause
  fullSpec @SubType
  fullSpec @SubqueryOp
  fullSpec @SubstrList
  fullSpec @SubstrListFromFor
  fullSpec @SymbolicExprBinOp
  fullSpec @TableFuncElement
  fullSpec @TableFuncElementList
  fullSpec @TableRef
  fullSpec @TablesampleClause
  fullSpec @TargetEl
  fullSpec @TargetList
  fullSpec @Targeting
  fullSpec @Timezone
  fullSpec @TrimList
  fullSpec @TrimModifier
  fullSpec @TypeList
  fullSpec @Typename
  fullSpec @UpdateStmt
  fullSpec @VerbalExprBinOp
  fullSpec @WhenClause
  fullSpec @WhenClauseList
  fullSpec @WhereOrCurrentClause
  fullSpec @WindowDefinition
  fullSpec @WindowExclusionClause
  fullSpec @WindowSpecification
  fullSpec @WithClause
  fullSpec @Xconst

  -- The two node types below can't round-trip as a top-level parse target
  -- (their renderings only make sense embedded after other text), so they
  -- get only the generator-bound property. Their 'Arbitrary' generators are
  -- still exercised — and bounded — like every other node's.
  --
  -- OptVarying's grammar makes the leading space before "VARYING"
  -- mandatory only because every caller embeds it straight after other text
  -- (see "PostgresqlSyntax.Ast.Character" and "PostgresqlSyntax.Ast.Bit",
  -- neither of which even uses its toTextBuilder directly). Parsed
  -- standalone, 'parse''s 'totally' wrapper strips that leading space
  -- before OptVarying's own parser gets a chance to require it.
  onlyArbitrarySpec @OptVarying
  -- TypenameArrayDimensions' ExplicitTypenameArrayDimensions alternative
  -- requires a leading space before "ARRAY" that only makes sense when
  -- embedded right after a Typename's own text, and 'parse''s 'totally'
  -- wrapper strips that leading space before this type's own parser runs.
  onlyArbitrarySpec @TypenameArrayDimensions

-- * Per-node-type property groups

-- | The property group every AST node type that round-trips through
-- 'parse' \/ 'toText' must satisfy: a parse/render round-trip ('IsAst') and
-- a bounded 'Arbitrary' generator. The group is labelled with the type's
-- name, derived from its 'Typeable' representation.
fullSpec :: forall a. (IsAst a, Eq a, Show a, Typeable.Typeable a, Qc.Arbitrary a) => Spec
fullSpec = byTypeName @a $ do
  describe "IsAst"
    $ prop "Roundtrips"
    $ \(a :: a) ->
      let sql = toText a
       in counterexample (Text.unpack sql) (parse sql === Right a)
  arbitrarySpec @a

-- | Like 'fullSpec' minus the round-trip property, for the two node types
-- whose renderings only round-trip when embedded after other text (see the
-- call sites in 'spec' for why).
onlyArbitrarySpec :: forall a. (IsAst a, Show a, Typeable.Typeable a, Qc.Arbitrary a) => Spec
onlyArbitrarySpec = byTypeName @a (arbitrarySpec @a)

-- | The 'Arbitrary' generator-bounds sub-group, shared by 'fullSpec' and
-- 'onlyArbitrarySpec'.
arbitrarySpec :: forall a. (IsAst a, Show a, Qc.Arbitrary a) => Spec
arbitrarySpec =
  -- Two invariants every 'Arbitrary' instance in this library must satisfy,
  -- independent of parsing (hence the only properties run for the node types
  -- that can't round-trip as a top-level parse target — see
  -- 'onlyArbitrarySpec'):
  --
  -- 1. 'terminatesAtZero': at size 0 the generator must escape every recursive
  --    strongly-connected component and yield a small value. A non-escaping
  --    base case turns size-0 generation into an unbounded random walk that
  --    renders to arbitrarily deep nesting.
  -- 2. 'growsBounded': at the suite's maximum size (hspec's default 'maxSize'
  --    is 100) the rendered output must stay within a budget, so a generator
  --    that explodes super-linearly — e.g. a list whose length doesn't consume
  --    the size budget — is caught by its output length rather than by a stack
  --    overflow deep inside a round-trip prop.
  describe "Arbitrary" $ do
    prop "Terminates at size 0" terminatesAtZero
    prop "Grows boundedly" growsBounded
  where
    terminatesAtZero =
      Qc.forAll (Qc.resize 0 (Qc.arbitrary @a)) $ \x ->
        let len = Text.length (toText x)
         in Qc.counterexample
              ("rendered " <> show len <> " chars at size 0 (max " <> show zeroSizeMaxLen <> ")")
              (len <= zeroSizeMaxLen)
      where
        -- Rendered-length ceiling for size-0 generation. A well-behaved
        -- generator produces a leaf at size 0, so this only ever trips on a
        -- non-terminating base case (which renders unbounded nesting).
        zeroSizeMaxLen = 500

    growsBounded =
      Qc.forAll (Qc.resize maxGenSize (Qc.arbitrary @a)) $ \x ->
        let len = Text.length (toText x)
         in Qc.counterexample
              ("rendered " <> show len <> " chars at size " <> show maxGenSize <> " (max " <> show maxGenSizeMaxLen <> ")")
              (len <= maxGenSizeMaxLen)
      where
        -- The size at which the growth bound is measured. Matches hspec's
        -- default 'maxSize', i.e. the largest size any prop in this suite is
        -- run at.
        maxGenSize = 100
        -- Rendered-length budget at 'maxGenSize'. Catches super-linear (e.g.
        -- quasi-polynomial) explosion that a @div 2@-per-edge rule doesn't
        -- bound.
        maxGenSizeMaxLen = 10000

byTypeName :: forall a. (Typeable.Typeable a) => Spec -> Spec
byTypeName = describe typeName
  where
    typeName = Text.unpack (last (Text.splitOn "." (Text.pack qualifiedName)))
      where
        qualifiedName =
          Typeable.tyConName (Typeable.typeRepTyCon (Typeable.typeRep (Proxy.Proxy @a)))

{-# LANGUAGE AllowAmbiguousTypes #-}

module Main (main) where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Proxy as Proxy
import qualified Data.Text as Text
import qualified Data.Typeable as Typeable
import PostgresqlSyntax
import Prelude
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (counterexample, (===))
import qualified Test.QuickCheck as Qc

main :: IO ()
main = hspec $ parallel $ do
  -- Each AST node type gets its own describe group, named after the type,
  -- holding its parse/render round-trip ('IsAst') and generator-bound
  -- ('Arbitrary') properties.
  astNode @AExpr
  astNode @AExprReversableOp
  astNode @AexprConst
  astNode @AliasClause
  astNode @AllOp
  astNode @AnyName
  astNode @AnyOperator
  astNode @ArrayBounds
  astNode @ArrayExpr
  astNode @ArrayExprList
  astNode @AscDesc
  astNode @Attrs
  astNode @BExpr
  astNode @BExprIsOp
  astNode @Bconst
  astNode @Bit
  astNode @CExpr
  astNode @CallStmt
  astNode @CaseExpr
  astNode @Character
  astNode @Columnref
  astNode @CommonTableExpr
  astNode @ConfExpr
  astNode @ConstCharacter
  astNode @ConstDatetime
  astNode @ConstTypename
  astNode @DeleteStmt
  astNode @ExplicitRow
  astNode @ExprList
  astNode @ExtractArg
  astNode @ExtractList
  astNode @Fconst
  astNode @ForLockingClause
  astNode @ForLockingItem
  astNode @ForLockingStrength
  astNode @FrameBound
  astNode @FrameClause
  astNode @FrameClauseMode
  astNode @FrameExtent
  astNode @FuncAliasClause
  astNode @FuncApplication
  astNode @FuncApplicationParams
  astNode @FuncArgExpr
  astNode @FuncConstArgs
  astNode @FuncExpr
  astNode @FuncExprCommonSubexpr
  astNode @FuncExprWindowless
  astNode @FuncName
  astNode @FuncTable
  astNode @GenericType
  astNode @GroupByItem
  astNode @Iconst
  astNode @Ident
  astNode @ImplicitRow
  astNode @InExpr
  astNode @IndexElem
  astNode @IndexElemDef
  astNode @IndexParams
  astNode @Indirection
  astNode @IndirectionEl
  astNode @InsertColumnItem
  astNode @InsertColumnList
  astNode @InsertRest
  astNode @InsertStmt
  astNode @InsertTarget
  astNode @Interval
  astNode @IntervalSecond
  astNode @JoinMeth
  astNode @JoinQual
  astNode @JoinType
  astNode @JoinedTable
  astNode @LimitClause
  astNode @MathOp
  astNode @NameList
  astNode @NullsOrder
  astNode @Numeric
  astNode @OffsetClause
  astNode @OnConflict
  astNode @OnConflictDo
  astNode @PostgresqlSyntax.Op
  astNode @OptOrdinality
  astNode @OptTempTableName
  astNode @OverClause
  astNode @OverlayList
  astNode @OverrideKind
  astNode @PositionList
  astNode @PreparableStmt
  astNode @QualAllOp
  astNode @QualOp
  astNode @QualifiedName
  astNode @RelationExpr
  astNode @RelationExprOptAlias
  astNode @Row
  astNode @RowsfromItem
  astNode @RowsfromList
  astNode @Sconst
  astNode @SelectBinOp
  astNode @SelectClause
  astNode @SelectFetchFirstValue
  astNode @SelectLimit
  astNode @SelectLimitValue
  astNode @SelectNoParens
  astNode @SelectStmt
  astNode @SelectWithParens
  astNode @SetClause
  astNode @SetClauseList
  astNode @SetTarget
  astNode @SetTargetList
  astNode @SimpleSelect
  astNode @SimpleTypename
  astNode @SortBy
  astNode @SortClause
  astNode @SubType
  astNode @SubqueryOp
  astNode @SubstrList
  astNode @SubstrListFromFor
  astNode @SymbolicExprBinOp
  astNode @TableFuncElement
  astNode @TableFuncElementList
  astNode @TableRef
  astNode @TablesampleClause
  astNode @TargetEl
  astNode @TargetList
  astNode @Targeting
  astNode @Timezone
  astNode @TrimList
  astNode @TrimModifier
  astNode @TypeList
  astNode @Typename
  astNode @UpdateStmt
  astNode @VerbalExprBinOp
  astNode @WhenClause
  astNode @WhenClauseList
  astNode @WhereOrCurrentClause
  astNode @WindowDefinition
  astNode @WindowExclusionClause
  astNode @WindowSpecification
  astNode @WithClause
  astNode @Xconst

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
  astNodeSkippingRoundtrip @OptVarying
  -- TypenameArrayDimensions' ExplicitTypenameArrayDimensions alternative
  -- requires a leading space before "ARRAY" that only makes sense when
  -- embedded right after a Typename's own text, and 'parse''s 'totally'
  -- wrapper strips that leading space before this type's own parser runs.
  astNodeSkippingRoundtrip @TypenameArrayDimensions

  describe "Parsers" $ do
    it "preparableStmt"
      $ forM_
        [ "select i :: int8 from auth.user as u\n\
          \inner join edgenode.usere_provider as p\n\
          \on u.id = p.user_id\n\
          \inner join edgenode.provider_branch as b\n\
          \on b.provider_fk = p.provider_id",
          -- FOR locking clause before LIMIT (PostgreSQL accepts both orderings)
          "select * from items for update limit 1",
          "select * from items limit 1 for update",
          "select * from items for share limit 10",
          "select * from items for no key update limit 1",
          "select * from items for key share limit 1",
          "select * from items for update of items nowait limit 1",
          "select * from items for update skip locked limit 1",
          "select * from items order by id for update limit 1",
          "select * from items for update offset 5 limit 10"
        ]
        (parsesTo @PreparableStmt)
    it "typename"
      $ forM_
        [ "int4[]",
          "int4[][]",
          "int4?[]",
          "int4?[]?",
          "aa array",
          "DOUBLE PRECISION",
          "bool",
          "int2",
          "int4",
          "int8",
          "float4",
          "float8",
          "numeric",
          "char",
          "text",
          "bytea",
          "date",
          "timestamp",
          "timestamptz",
          "time",
          "timetz",
          "interval",
          "uuid",
          "inet",
          "json",
          "jsonb"
        ]
        (parsesTo @Typename)
    it "sconst"
      $ forM_
        [ "'it''s good'",
          "$$it's good$$",
          "$x$it's good$x$"
        ]
        (parsesTo @Sconst)

  describe "Nesting depth" $ do
    it "redundant parens, depth 50"
      $ parsesWithin @AExpr 5 (Text.replicate 50 "(" <> "a + b" <> Text.replicate 50 ")")
    it "redundant parens around a select, depth 50"
      $ parsesWithin @PreparableStmt 5 ("select " <> Text.replicate 50 "(" <> "a + b" <> Text.replicate 50 ")")
    it "sum of COALESCE terms in two wrapped groups"
      $ let terms off = Text.intercalate " + " ["coalesce(c" <> Text.pack (show (off + i)) <> ", 0)" | i <- [1 .. 24 :: Int]]
            coalesceSumInput = Text.replicate 6 "(" <> "(" <> terms 0 <> ") - (" <> terms 24 <> ")" <> Text.replicate 6 ")"
         in parsesWithin @AExpr 5 coalesceSumInput
    -- The parenthesised sub-select has two possible representations.
    it "redundant parens around a sub-select are canonicalised"
      $ parse @SelectWithParens "((select 1))"
      `shouldBe` (WithParensSelectWithParens . NoParensSelectWithParens <$> parse @SelectNoParens "select 1")
    it "OVERLAPS still parses" $ do
      let render :: AExpr -> Text
          render = toText
      fmap render (parse @AExpr "(1, 2) overlaps (3, 4)") `shouldBe` Right "(1, 2) OVERLAPS (3, 4)"
      fmap render (parse @AExpr "row(1, 2) overlaps row(3, 4)") `shouldBe` Right "ROW (1, 2) OVERLAPS ROW (3, 4)"

  describe "Error reporting" $ do
    it "Typo in FROM keyword"
      $ reportsError @PreparableStmt
        "select i :: int8 fom auth.user as u\n\
        \inner join edgenode.usere_provider as p\n\
        \on u.id = p.user_id\n\
        \inner join edgenode.provider_branch as b\n\
        \on b.provider_fk = p.provider_id"
        "(20,\"offset=20:\\nunexpected space\\nexpecting end of input\\n\")"
    it "Typo in NOT keyword"
      $ reportsError @PreparableStmt
        "select i :: int8 from auth.user as u\n\
        \WHERE u.id IS NO NULL && TRUE"
        "(51,\"offset=51:\\nexpecting white space\\n\")"

-- * Per-node-type property groups

-- | The property suite every AST node type that round-trips through
-- 'parse' \/ 'toText' must satisfy: a parse/render round-trip ('IsAst') and
-- a bounded 'Arbitrary' generator. The group is labelled with the type's
-- name, derived from its 'Typeable' representation.
astNode :: forall a. (IsAst a, Eq a, Show a, Typeable.Typeable a, Qc.Arbitrary a) => Spec
astNode = byTypeName @a $ do
  describe "IsAst"
    $ prop "Roundtrips"
    $ \(a :: a) ->
      let sql = toText a
       in counterexample (Text.unpack sql) (parse sql === Right a)
  arbitraryBounds @a

-- | Like 'astNode' minus the round-trip property, for the two node types
-- whose renderings only round-trip when embedded after other text (see the
-- call sites in 'main' for why).
astNodeSkippingRoundtrip :: forall a. (IsAst a, Show a, Typeable.Typeable a, Qc.Arbitrary a) => Spec
astNodeSkippingRoundtrip = byTypeName @a (arbitraryBounds @a)

-- | The 'Arbitrary' generator-bounds sub-group, shared by 'astNode' and
-- 'astNodeSkippingRoundtrip'.
arbitraryBounds :: forall a. (IsAst a, Show a, Qc.Arbitrary a) => Spec
arbitraryBounds =
  describe "Arbitrary"
    $ prop "Has proper generator bounds" (generatorBounds @a)

byTypeName :: forall a. (Typeable.Typeable a) => Spec -> Spec
byTypeName = describe typeName
  where
    typeName = Text.unpack (last (Text.splitOn "." (Text.pack qualifiedName)))
      where
        qualifiedName =
          Typeable.tyConName (Typeable.typeRepTyCon (Typeable.typeRep (Proxy.Proxy @a)))

-- * Generator-bound property

--
-- Two invariants every 'Arbitrary' instance in this library must satisfy,
-- independent of parsing (hence the only property run for the node types
-- that can't round-trip as a top-level parse target — see
-- 'astNodeSkippingRoundtrip'):
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

-- | Both generator invariants conjoined so each type appears once.
-- QuickCheck reports which conjunct fails.
generatorBounds :: forall a. (IsAst a, Qc.Arbitrary a, Show a) => Qc.Property
generatorBounds =
  terminatesAtZero Qc..&&. growsBounded
  where
    -- \| Rendered-length ceiling for size-0 generation. A well-behaved
    -- generator produces a leaf at size 0, so this only ever trips on a
    -- non-terminating base case (which renders unbounded nesting).
    zeroSizeMaxLen = 500
    -- \| The size at which the growth bound is measured. Matches hspec's
    -- default 'maxSize', i.e. the largest size any prop in this suite is
    -- run at.
    maxGenSize = 100
    -- \| Rendered-length budget at 'maxGenSize'. Catches super-linear (e.g.
    -- quasi-polynomial) explosion that a @div 2@-per-edge rule doesn't
    -- bound.
    maxGenSizeMaxLen = 10000

    terminatesAtZero =
      Qc.forAll (Qc.resize 0 (Qc.arbitrary @a)) $ \x ->
        let len = Text.length (toText x)
         in Qc.counterexample
              ("rendered " <> show len <> " chars at size 0 (max " <> show zeroSizeMaxLen <> ")")
              (len <= zeroSizeMaxLen)

    growsBounded =
      Qc.forAll (Qc.resize maxGenSize (Qc.arbitrary @a)) $ \x ->
        let len = Text.length (toText x)
         in Qc.counterexample
              ("rendered " <> show len <> " chars at size " <> show maxGenSize <> " (max " <> show maxGenSizeMaxLen <> ")")
              (len <= maxGenSizeMaxLen)

-- * Example-based parse helpers

parsesTo :: forall a. (IsAst a) => Text -> Expectation
parsesTo input =
  case parse @a input of
    Left err -> expectationFailure (err <> "\ninput: " <> Text.unpack input)
    Right _ -> pure ()

reportsError :: forall a. (IsAst a) => Text -> String -> Expectation
reportsError input expected =
  case parseWithPosError @a input of
    Left err -> show (NonEmpty.head err) `shouldBe` expected
    Right _ -> expectationFailure "expected a parse error, but it succeeded"

parsesWithin :: forall a. (IsAst a) => Int -> Text -> Expectation
parsesWithin seconds input = do
  result <-
    timeout (seconds * 1000000)
      $ case parse @a input of
        Left err -> expectationFailure (err <> "\ninput: " <> Text.unpack input)
        Right _ -> pure ()
  case result of
    Nothing -> expectationFailure ("Did not finish parsing within " <> show seconds <> "s")
    Just () -> pure ()

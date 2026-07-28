{-# LANGUAGE AllowAmbiguousTypes #-}

module Main (main) where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import PostgresqlSyntax
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (Property, counterexample, (===))
import Prelude

main :: IO ()
main = hspec $ parallel $ do
  describe "Round-trip parse/render" $ do
    prop "AExpr" (roundTrip @AExpr)
    prop "AExprReversableOp" (roundTrip @AExprReversableOp)
    prop "AexprConst" (roundTrip @AexprConst)
    prop "AliasClause" (roundTrip @AliasClause)
    prop "AllOp" (roundTrip @AllOp)
    prop "AnyName" (roundTrip @AnyName)
    prop "AnyOperator" (roundTrip @AnyOperator)
    prop "ArrayBounds" (roundTrip @ArrayBounds)
    prop "ArrayExpr" (roundTrip @ArrayExpr)
    prop "ArrayExprList" (roundTrip @ArrayExprList)
    prop "AscDesc" (roundTrip @AscDesc)
    prop "Attrs" (roundTrip @Attrs)
    prop "BExpr" (roundTrip @BExpr)
    prop "BExprIsOp" (roundTrip @BExprIsOp)
    prop "Bconst" (roundTrip @Bconst)
    prop "Bit" (roundTrip @Bit)
    prop "CExpr" (roundTrip @CExpr)
    prop "CallStmt" (roundTrip @CallStmt)
    prop "CaseExpr" (roundTrip @CaseExpr)
    prop "Character" (roundTrip @Character)
    prop "Columnref" (roundTrip @Columnref)
    prop "CommonTableExpr" (roundTrip @CommonTableExpr)
    prop "ConfExpr" (roundTrip @ConfExpr)
    prop "ConstCharacter" (roundTrip @ConstCharacter)
    prop "ConstDatetime" (roundTrip @ConstDatetime)
    prop "ConstTypename" (roundTrip @ConstTypename)
    prop "DeleteStmt" (roundTrip @DeleteStmt)
    prop "ExplicitRow" (roundTrip @ExplicitRow)
    prop "ExprList" (roundTrip @ExprList)
    prop "ExtractArg" (roundTrip @ExtractArg)
    prop "ExtractList" (roundTrip @ExtractList)
    prop "Fconst" (roundTrip @Fconst)
    prop "ForLockingClause" (roundTrip @ForLockingClause)
    prop "ForLockingItem" (roundTrip @ForLockingItem)
    prop "ForLockingStrength" (roundTrip @ForLockingStrength)
    prop "FrameBound" (roundTrip @FrameBound)
    prop "FrameClause" (roundTrip @FrameClause)
    prop "FrameClauseMode" (roundTrip @FrameClauseMode)
    prop "FrameExtent" (roundTrip @FrameExtent)
    prop "FuncAliasClause" (roundTrip @FuncAliasClause)
    prop "FuncApplication" (roundTrip @FuncApplication)
    prop "FuncApplicationParams" (roundTrip @FuncApplicationParams)
    prop "FuncArgExpr" (roundTrip @FuncArgExpr)
    prop "FuncConstArgs" (roundTrip @FuncConstArgs)
    prop "FuncExpr" (roundTrip @FuncExpr)
    prop "FuncExprCommonSubexpr" (roundTrip @FuncExprCommonSubexpr)
    prop "FuncExprWindowless" (roundTrip @FuncExprWindowless)
    prop "FuncName" (roundTrip @FuncName)
    prop "FuncTable" (roundTrip @FuncTable)
    prop "GenericType" (roundTrip @GenericType)
    prop "GroupByItem" (roundTrip @GroupByItem)
    prop "Iconst" (roundTrip @Iconst)
    prop "Ident" (roundTrip @Ident)
    prop "ImplicitRow" (roundTrip @ImplicitRow)
    prop "InExpr" (roundTrip @InExpr)
    prop "IndexElem" (roundTrip @IndexElem)
    prop "IndexElemDef" (roundTrip @IndexElemDef)
    prop "IndexParams" (roundTrip @IndexParams)
    prop "Indirection" (roundTrip @Indirection)
    prop "IndirectionEl" (roundTrip @IndirectionEl)
    prop "InsertColumnItem" (roundTrip @InsertColumnItem)
    prop "InsertColumnList" (roundTrip @InsertColumnList)
    prop "InsertRest" (roundTrip @InsertRest)
    prop "InsertStmt" (roundTrip @InsertStmt)
    prop "InsertTarget" (roundTrip @InsertTarget)
    prop "Interval" (roundTrip @Interval)
    prop "IntervalSecond" (roundTrip @IntervalSecond)
    prop "JoinMeth" (roundTrip @JoinMeth)
    prop "JoinQual" (roundTrip @JoinQual)
    prop "JoinType" (roundTrip @JoinType)
    prop "JoinedTable" (roundTrip @JoinedTable)
    prop "LimitClause" (roundTrip @LimitClause)
    prop "MathOp" (roundTrip @MathOp)
    prop "NameList" (roundTrip @NameList)
    prop "NullsOrder" (roundTrip @NullsOrder)
    prop "Numeric" (roundTrip @Numeric)
    prop "OffsetClause" (roundTrip @OffsetClause)
    prop "OnConflict" (roundTrip @OnConflict)
    prop "OnConflictDo" (roundTrip @OnConflictDo)
    prop "Op" (roundTrip @PostgresqlSyntax.Op)
    prop "OptOrdinality" (roundTrip @OptOrdinality)
    prop "OptTempTableName" (roundTrip @OptTempTableName)
    -- OptVarying is intentionally omitted: its grammar makes the leading
    -- space before "VARYING" mandatory only because every caller embeds it
    -- straight after other text (see "PostgresqlSyntax.Ast.Character" and
    -- "PostgresqlSyntax.Ast.Bit", neither of which even uses its toTextBuilder
    -- directly). Parsed standalone, `parse`'s `totally` wrapper strips that
    -- leading space before OptVarying's own parser gets a chance to require
    -- it, so it can never round-trip as a top-level parse target.
    prop "OverClause" (roundTrip @OverClause)
    prop "OverlayList" (roundTrip @OverlayList)
    prop "OverrideKind" (roundTrip @OverrideKind)
    prop "PositionList" (roundTrip @PositionList)
    prop "PreparableStmt" (roundTrip @PreparableStmt)
    prop "QualAllOp" (roundTrip @QualAllOp)
    prop "QualOp" (roundTrip @QualOp)
    prop "QualifiedName" (roundTrip @QualifiedName)
    prop "RelationExpr" (roundTrip @RelationExpr)
    prop "RelationExprOptAlias" (roundTrip @RelationExprOptAlias)
    prop "Row" (roundTrip @Row)
    prop "RowsfromItem" (roundTrip @RowsfromItem)
    prop "RowsfromList" (roundTrip @RowsfromList)
    prop "Sconst" (roundTrip @Sconst)
    prop "SelectBinOp" (roundTrip @SelectBinOp)
    prop "SelectClause" (roundTrip @SelectClause)
    prop "SelectFetchFirstValue" (roundTrip @SelectFetchFirstValue)
    prop "SelectLimit" (roundTrip @SelectLimit)
    prop "SelectLimitValue" (roundTrip @SelectLimitValue)
    prop "SelectNoParens" (roundTrip @SelectNoParens)
    prop "SelectStmt" (roundTrip @SelectStmt)
    prop "SelectWithParens" (roundTrip @SelectWithParens)
    prop "SetClause" (roundTrip @SetClause)
    prop "SetClauseList" (roundTrip @SetClauseList)
    prop "SetTarget" (roundTrip @SetTarget)
    prop "SetTargetList" (roundTrip @SetTargetList)
    prop "SimpleSelect" (roundTrip @SimpleSelect)
    prop "SimpleTypename" (roundTrip @SimpleTypename)
    prop "SortBy" (roundTrip @SortBy)
    prop "SortClause" (roundTrip @SortClause)
    prop "SubType" (roundTrip @SubType)
    prop "SubqueryOp" (roundTrip @SubqueryOp)
    prop "SubstrList" (roundTrip @SubstrList)
    prop "SubstrListFromFor" (roundTrip @SubstrListFromFor)
    prop "SymbolicExprBinOp" (roundTrip @SymbolicExprBinOp)
    prop "TableFuncElement" (roundTrip @TableFuncElement)
    prop "TableFuncElementList" (roundTrip @TableFuncElementList)
    prop "TableRef" (roundTrip @TableRef)
    prop "TablesampleClause" (roundTrip @TablesampleClause)
    prop "TargetEl" (roundTrip @TargetEl)
    prop "TargetList" (roundTrip @TargetList)
    prop "Targeting" (roundTrip @Targeting)
    prop "Timezone" (roundTrip @Timezone)
    prop "TrimList" (roundTrip @TrimList)
    prop "TrimModifier" (roundTrip @TrimModifier)
    prop "TypeList" (roundTrip @TypeList)
    prop "Typename" (roundTrip @Typename)
    -- TypenameArrayDimensions is intentionally omitted: same reason as
    -- OptVarying above — its ExplicitTypenameArrayDimensions alternative
    -- requires a leading space before "ARRAY" that only makes sense when
    -- embedded right after a Typename's own text, and `parse`'s `totally`
    -- wrapper strips that leading space before this type's own parser runs.
    prop "UpdateStmt" (roundTrip @UpdateStmt)
    prop "VerbalExprBinOp" (roundTrip @VerbalExprBinOp)
    prop "WhenClause" (roundTrip @WhenClause)
    prop "WhenClauseList" (roundTrip @WhenClauseList)
    prop "WhereOrCurrentClause" (roundTrip @WhereOrCurrentClause)
    prop "WindowDefinition" (roundTrip @WindowDefinition)
    prop "WindowExclusionClause" (roundTrip @WindowExclusionClause)
    prop "WindowSpecification" (roundTrip @WindowSpecification)
    prop "WithClause" (roundTrip @WithClause)
    prop "Xconst" (roundTrip @Xconst)

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

-- * Round-trip property

roundTrip :: (IsAst a, Eq a, Show a) => a -> Property
roundTrip a = counterexample (Text.unpack sql) (parse sql === Right a)
  where
    sql = toText a

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

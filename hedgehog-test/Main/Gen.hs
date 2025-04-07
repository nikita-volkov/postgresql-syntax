{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main.Gen where

import qualified Data.HashSet as HashSet
import qualified Data.List as List
import qualified Data.Text as Text
import Hedgehog (Gen)
import Hedgehog.Gen
import qualified Hedgehog.Range as Range
import PostgresqlSyntax.Ast
import qualified PostgresqlSyntax.KeywordSet as KeywordSet
import qualified PostgresqlSyntax.Validation as Validation
import Prelude hiding (bit, bool, filter, fromList, maybe, sortBy)

-- * Generic

inSet set = filter (flip HashSet.member set)

notInSet set = filter (not . flip HashSet.member set)

-- * Statements

preparableStmt =
  choice
    [ SelectPreparableStmt <$> selectStmt,
      InsertPreparableStmt <$> insertStmt,
      UpdatePreparableStmt <$> updateStmt,
      DeletePreparableStmt <$> deleteStmt,
      CallPreparableStmt <$> callStmt
    ]

-- * Call

callStmt = CallStmt <$> funcApplication

-- * Insert

insertStmt = InsertStmt <$> maybe withClause <*> insertTarget <*> insertRest <*> maybe onConflict <*> maybe returningClause

insertTarget = InsertTarget <$> qualifiedName <*> maybe colId

insertRest =
  choice
    [ SelectInsertRest <$> maybe insertColumnList <*> maybe overrideKind <*> selectStmt,
      pure DefaultValuesInsertRest
    ]

overrideKind = enumBounded

insertColumnList = nonEmpty (Range.exponential 1 7) insertColumnItem

insertColumnItem = InsertColumnItem <$> colId <*> maybe indirection

onConflict = OnConflict <$> maybe confExpr <*> onConflictDo

onConflictDo =
  choice
    [ UpdateOnConflictDo <$> setClauseList <*> maybe whereClause,
      pure NothingOnConflictDo
    ]

confExpr =
  choice
    [ WhereConfExpr <$> indexParams <*> maybe whereClause,
      ConstraintConfExpr <$> name
    ]

returningClause = targetList

-- * Update

updateStmt = UpdateStmt <$> maybe withClause <*> relationExprOptAlias <*> setClauseList <*> maybe fromClause <*> maybe whereOrCurrentClause <*> maybe returningClause

setClauseList = nonEmpty (Range.exponential 1 10) setClause

setClause =
  choice
    [ TargetSetClause <$> setTarget <*> aExpr,
      TargetListSetClause <$> setTargetList <*> aExpr
    ]

setTarget = SetTarget <$> colId <*> maybe indirection

setTargetList = nonEmpty (Range.exponential 1 10) setTarget

-- * Delete

deleteStmt = DeleteStmt <$> maybe withClause <*> relationExprOptAlias <*> maybe usingClause <*> maybe whereOrCurrentClause <*> maybe returningClause

usingClause = fromList

-- * Select

selectStmt = Left <$> selectNoParens

-- ** selectNoParens

selectNoParens =
  frequency
    [ (90, SelectNoParens <$> maybe withClause <*> (Left <$> simpleSelect) <*> maybe sortClause <*> maybe selectLimit <*> maybe forLockingClause),
      (10, SelectNoParens <$> fmap Just withClause <*> selectClause <*> fmap Just sortClause <*> fmap Just selectLimit <*> fmap Just forLockingClause)
    ]

terminalSelectNoParens =
  SelectNoParens <$> pure Nothing <*> (Left <$> terminalSimpleSelect) <*> pure Nothing <*> pure Nothing <*> pure Nothing

-- ** selectWithParens

selectWithParens = sized $ \size ->
  if size <= 1
    then discard
    else
      frequency
        [ (95, NoParensSelectWithParens <$> selectNoParens),
          (5, WithParensSelectWithParens <$> selectWithParens)
        ]

terminalSelectWithParens = NoParensSelectWithParens <$> terminalSelectNoParens

-- ** selectClause

selectClause =
  choice
    [ Left <$> simpleSelect,
      Right <$> small selectWithParens
    ]

nonTrailingSelectClause = Left <$> nonTrailingSimpleSelect

-- ** simpleSelect

simpleSelect =
  choice
    [ normalSimpleSelect,
      tableSimpleSelect,
      valuesSimpleSelect,
      small nonTrailingSelectClause >>= binSimpleSelect
    ]

nonTrailingSimpleSelect = choice [normalSimpleSelect, valuesSimpleSelect, tableSimpleSelect]

normalSimpleSelect = NormalSimpleSelect <$> maybe targeting <*> maybe intoClause <*> maybe fromClause <*> maybe whereClause <*> maybe groupClause <*> maybe havingClause <*> maybe windowClause

tableSimpleSelect = TableSimpleSelect <$> relationExpr

valuesSimpleSelect = ValuesSimpleSelect <$> valuesClause

binSimpleSelect leftSelect =
  BinSimpleSelect <$> selectBinOp <*> pure leftSelect <*> maybe allOrDistinct <*> small selectClause

terminalSimpleSelect = pure (NormalSimpleSelect Nothing Nothing Nothing Nothing Nothing Nothing Nothing)

-- * Targeting

targeting =
  choice
    [ NormalTargeting <$> targetList,
      AllTargeting <$> maybe targetList,
      DistinctTargeting <$> maybe (nonEmpty (Range.exponential 1 8) aExpr) <*> targetList
    ]

targetList = nonEmpty (Range.exponential 1 8) targetEl

targetEl =
  choice
    [ pure AsteriskTargetEl,
      AliasedExprTargetEl <$> aExpr <*> colLabel,
      ImplicitlyAliasedExprTargetEl <$> prefixAExpr <*> ident,
      ExprTargetEl <$> aExpr
    ]

-- * BinSimpleSelect

selectBinOp = element [UnionSelectBinOp, IntersectSelectBinOp, ExceptSelectBinOp]

-- * With Clause

withClause = WithClause <$> bool <*> nonEmpty (Range.exponential 1 7) commonTableExpr

commonTableExpr = CommonTableExpr <$> name <*> maybe (nonEmpty (Range.exponential 1 8) name) <*> maybe bool <*> small preparableStmt

-- * Into Clause

intoClause = optTempTableName

optTempTableName =
  choice
    [ TemporaryOptTempTableName <$> bool <*> qualifiedName,
      TempOptTempTableName <$> bool <*> qualifiedName,
      LocalTemporaryOptTempTableName <$> bool <*> qualifiedName,
      LocalTempOptTempTableName <$> bool <*> qualifiedName,
      GlobalTemporaryOptTempTableName <$> bool <*> qualifiedName,
      GlobalTempOptTempTableName <$> bool <*> qualifiedName,
      UnloggedOptTempTableName <$> bool <*> qualifiedName,
      TableOptTempTableName <$> qualifiedName,
      QualifedOptTempTableName <$> qualifiedName
    ]

-- * From Clause

fromList = nonEmpty (Range.exponential 1 8) tableRef

fromClause = fromList

tableRef = choice [relationExprTableRef, selectTableRef, joinTableRef]

relationExprTableRef = RelationExprTableRef <$> relationExpr <*> maybe aliasClause <*> maybe tablesampleClause

funcTableRef = FuncTableRef <$> bool <*> funcTable <*> maybe funcAliasClause

selectTableRef = SelectTableRef <$> bool <*> small selectWithParens <*> maybe aliasClause

joinTableRef = JoinTableRef <$> joinedTable <*> maybe aliasClause

relationExpr =
  choice
    [ SimpleRelationExpr <$> qualifiedName <*> bool,
      OnlyRelationExpr <$> qualifiedName <*> bool
    ]

relationExprOptAlias = RelationExprOptAlias <$> relationExpr <*> maybe ((,) <$> bool <*> colId)

tablesampleClause = TablesampleClause <$> funcName <*> exprList <*> maybe repeatableClause

repeatableClause = aExpr

funcTable =
  choice
    [ FuncExprFuncTable <$> funcExprWindowless <*> optOrdinality,
      RowsFromFuncTable <$> rowsfromList <*> optOrdinality
    ]

rowsfromItem = RowsfromItem <$> funcExprWindowless <*> maybe colDefList

rowsfromList = nonEmpty (Range.exponential 1 8) rowsfromItem

colDefList = tableFuncElementList

optOrdinality = bool

tableFuncElementList = nonEmpty (Range.exponential 1 7) tableFuncElement

tableFuncElement = TableFuncElement <$> colId <*> typename <*> maybe collateClause

collateClause = anyName

aliasClause = AliasClause <$> bool <*> name <*> maybe (nonEmpty (Range.exponential 1 8) name)

funcAliasClause =
  choice
    [ AliasFuncAliasClause <$> aliasClause,
      AsFuncAliasClause <$> tableFuncElementList,
      AsColIdFuncAliasClause <$> colId <*> tableFuncElementList,
      ColIdFuncAliasClause <$> colId <*> tableFuncElementList
    ]

joinedTable =
  frequency
    [ (5,) $ InParensJoinedTable <$> joinedTable,
      (95,) $ MethJoinedTable <$> joinMeth <*> tableRef <*> choice [relationExprTableRef, selectTableRef, funcTableRef]
    ]

joinMeth =
  choice
    [ pure CrossJoinMeth,
      QualJoinMeth <$> maybe joinType <*> joinQual,
      NaturalJoinMeth <$> maybe joinType
    ]

joinType =
  choice
    [ FullJoinType <$> bool,
      LeftJoinType <$> bool,
      RightJoinType <$> bool,
      pure InnerJoinType
    ]

joinQual =
  choice
    [ UsingJoinQual <$> nonEmpty (Range.exponential 1 8) name,
      OnJoinQual <$> aExpr
    ]

-- * Group Clause

groupClause = nonEmpty (Range.exponential 1 8) groupByItem

groupByItem =
  choice
    [ ExprGroupByItem <$> aExpr,
      pure EmptyGroupingSetGroupByItem,
      RollupGroupByItem <$> nonEmpty (Range.exponential 1 8) aExpr,
      CubeGroupByItem <$> nonEmpty (Range.exponential 1 8) aExpr,
      GroupingSetsGroupByItem <$> nonEmpty (Range.exponential 1 3) groupByItem
    ]

-- * Having Clause

havingClause = aExpr

-- * Where Clause

whereClause = aExpr

whereOrCurrentClause =
  choice
    [ ExprWhereOrCurrentClause <$> aExpr,
      CursorWhereOrCurrentClause <$> cursorName
    ]

-- * Window Clause

windowClause = nonEmpty (Range.exponential 1 8) windowDefinition

windowDefinition = WindowDefinition <$> name <*> windowSpecification

windowSpecification = WindowSpecification <$> maybe name <*> maybe (nonEmpty (Range.exponential 1 8) nonSuffixOpAExpr) <*> maybe sortClause <*> maybe frameClause

frameClause = FrameClause <$> frameClauseMode <*> frameExtent <*> maybe windowExclusionClause

frameClauseMode = element [RangeFrameClauseMode, RowsFrameClauseMode, GroupsFrameClauseMode]

frameExtent =
  choice
    [ SingularFrameExtent <$> frameBound,
      BetweenFrameExtent <$> frameBound <*> frameBound
    ]

frameBound =
  choice
    [ pure UnboundedPrecedingFrameBound,
      pure UnboundedFollowingFrameBound,
      pure CurrentRowFrameBound,
      PrecedingFrameBound <$> prefixAExpr,
      FollowingFrameBound <$> prefixAExpr
    ]

windowExclusionClause = element [CurrentRowWindowExclusionClause, GroupWindowExclusionClause, TiesWindowExclusionClause, NoOthersWindowExclusionClause]

-- * Values Clause

valuesClause = nonEmpty (Range.exponential 1 8) (nonEmpty (Range.exponential 1 8) aExpr)

-- * Sort Clause

sortClause = nonEmpty (Range.exponential 1 8) sortBy

sortBy =
  choice
    [ UsingSortBy <$> nonSuffixOpAExpr <*> qualAllOp <*> maybe nullsOrder,
      AscDescSortBy <$> nonSuffixOpAExpr <*> maybe ascDesc <*> maybe nullsOrder
    ]

-- * All or distinct

allOrDistinct = bool

-- * Limit

selectLimit =
  choice
    [ LimitOffsetSelectLimit <$> limitClause <*> offsetClause,
      OffsetLimitSelectLimit <$> offsetClause <*> limitClause,
      LimitSelectLimit <$> limitClause,
      OffsetSelectLimit <$> offsetClause
    ]

limitClause =
  choice
    [ LimitLimitClause <$> selectLimitValue <*> maybe aExpr,
      FetchOnlyLimitClause <$> bool <*> maybe selectFetchFirstValue <*> bool
    ]

selectFetchFirstValue =
  choice
    [ ExprSelectFetchFirstValue <$> cExpr,
      NumSelectFetchFirstValue <$> bool <*> iconstOrFconst
    ]

selectLimitValue =
  choice
    [ ExprSelectLimitValue <$> aExpr,
      pure AllSelectLimitValue
    ]

offsetClause =
  choice
    [ ExprOffsetClause <$> aExpr,
      FetchFirstOffsetClause <$> selectFetchFirstValue <*> bool
    ]

-- * For Locking

forLockingClause =
  choice
    [ ItemsForLockingClause <$> nonEmpty (Range.exponential 1 8) forLockingItem,
      pure ReadOnlyForLockingClause
    ]

forLockingItem = ForLockingItem <$> forLockingStrength <*> maybe (nonEmpty (Range.exponential 1 8) qualifiedName) <*> maybe bool

forLockingStrength =
  element
    [ UpdateForLockingStrength,
      NoKeyUpdateForLockingStrength,
      ShareForLockingStrength,
      KeyForLockingStrength
    ]

-- * Expressions

exprList = nonEmpty (Range.exponential 1 7) aExpr

aExpr =
  recursive
    choice
    [ CExprAExpr <$> cExpr,
      pure DefaultAExpr
    ]
    [ TypecastAExpr <$> prefixAExpr <*> typename,
      CollateAExpr <$> prefixAExpr <*> anyName,
      AtTimeZoneAExpr <$> prefixAExpr <*> aExpr,
      PlusAExpr <$> aExpr,
      MinusAExpr <$> aExpr,
      SymbolicBinOpAExpr <$> prefixAExpr <*> symbolicExprBinOp <*> aExpr,
      PrefixQualOpAExpr <$> qualOp <*> aExpr,
      SuffixQualOpAExpr <$> prefixAExpr <*> qualOp,
      AndAExpr <$> prefixAExpr <*> aExpr,
      OrAExpr <$> prefixAExpr <*> aExpr,
      NotAExpr <$> aExpr,
      VerbalExprBinOpAExpr <$> prefixAExpr <*> bool <*> verbalExprBinOp <*> prefixAExpr <*> maybe aExpr,
      ReversableOpAExpr <$> prefixAExpr <*> bool <*> aExprReversableOp,
      IsnullAExpr <$> prefixAExpr,
      NotnullAExpr <$> prefixAExpr,
      OverlapsAExpr <$> row <*> row,
      SubqueryAExpr <$> prefixAExpr <*> subqueryOp <*> subType <*> choice [Left <$> selectWithParens, Right <$> nonSelectAExpr],
      UniqueAExpr <$> selectWithParens
    ]

prefixAExpr =
  choice
    [ CExprAExpr <$> cExpr,
      pure DefaultAExpr,
      UniqueAExpr <$> selectWithParens
    ]

nonSuffixOpAExpr =
  recursive
    choice
    [ CExprAExpr <$> cExpr,
      pure DefaultAExpr
    ]
    [ TypecastAExpr <$> prefixAExpr <*> typename,
      CollateAExpr <$> prefixAExpr <*> anyName,
      AtTimeZoneAExpr <$> prefixAExpr <*> nonSuffixOpAExpr,
      PlusAExpr <$> nonSuffixOpAExpr,
      MinusAExpr <$> nonSuffixOpAExpr,
      SymbolicBinOpAExpr <$> prefixAExpr <*> symbolicExprBinOp <*> nonSuffixOpAExpr,
      PrefixQualOpAExpr <$> qualOp <*> nonSuffixOpAExpr,
      AndAExpr <$> prefixAExpr <*> nonSuffixOpAExpr,
      OrAExpr <$> prefixAExpr <*> nonSuffixOpAExpr,
      NotAExpr <$> nonSuffixOpAExpr,
      VerbalExprBinOpAExpr <$> prefixAExpr <*> bool <*> verbalExprBinOp <*> prefixAExpr <*> maybe nonSuffixOpAExpr,
      IsnullAExpr <$> prefixAExpr,
      NotnullAExpr <$> prefixAExpr,
      UniqueAExpr <$> selectWithParens
    ]

nonSelectAExpr =
  choice
    [ TypecastAExpr <$> prefixAExpr <*> typename,
      CollateAExpr <$> prefixAExpr <*> anyName,
      AtTimeZoneAExpr <$> prefixAExpr <*> aExpr,
      PlusAExpr <$> aExpr,
      MinusAExpr <$> aExpr,
      SymbolicBinOpAExpr <$> prefixAExpr <*> symbolicExprBinOp <*> aExpr,
      PrefixQualOpAExpr <$> qualOp <*> aExpr,
      SuffixQualOpAExpr <$> prefixAExpr <*> qualOp,
      AndAExpr <$> prefixAExpr <*> aExpr,
      OrAExpr <$> prefixAExpr <*> aExpr,
      NotAExpr <$> aExpr,
      VerbalExprBinOpAExpr <$> prefixAExpr <*> bool <*> verbalExprBinOp <*> prefixAExpr <*> maybe aExpr,
      ReversableOpAExpr <$> prefixAExpr <*> bool <*> aExprReversableOp,
      IsnullAExpr <$> prefixAExpr,
      NotnullAExpr <$> prefixAExpr,
      OverlapsAExpr <$> row <*> row,
      SubqueryAExpr <$> prefixAExpr <*> subqueryOp <*> subType <*> choice [Left <$> selectWithParens, Right <$> nonSelectAExpr],
      UniqueAExpr <$> selectWithParens
    ]

bExpr =
  recursive
    choice
    [ CExprBExpr <$> cExpr
    ]
    [ TypecastBExpr <$> prefixBExpr <*> typename,
      PlusBExpr <$> bExpr,
      MinusBExpr <$> bExpr,
      SymbolicBinOpBExpr <$> prefixBExpr <*> symbolicExprBinOp <*> bExpr,
      QualOpBExpr <$> qualOp <*> bExpr,
      IsOpBExpr <$> prefixBExpr <*> bool <*> bExprIsOp
    ]

prefixBExpr =
  choice
    [ CExprBExpr <$> cExpr
    ]

cExpr =
  recursive
    choice
    [ ColumnrefCExpr <$> columnref
    ]
    [ AexprConstCExpr <$> aexprConst,
      ParamCExpr <$> integral (Range.linear 1 19) <*> maybe indirection,
      InParensCExpr <$> nonSelectAExpr <*> maybe indirection,
      CaseCExpr <$> caseExpr,
      FuncCExpr <$> funcExpr,
      SelectWithParensCExpr <$> selectWithParens <*> maybe indirection,
      ExistsCExpr <$> selectWithParens,
      ArrayCExpr <$> choice [Left <$> selectWithParens, Right <$> arrayExpr],
      ExplicitRowCExpr <$> explicitRow,
      ImplicitRowCExpr <$> implicitRow,
      GroupingCExpr <$> exprList
    ]

caseExpr = CaseExpr <$> maybe aExpr <*> whenClauseList <*> maybe aExpr

whenClauseList = nonEmpty (Range.exponential 1 7) whenClause

whenClause = WhenClause <$> small aExpr <*> small aExpr

inExpr =
  choice
    [ SelectInExpr <$> NoParensSelectWithParens <$> selectNoParens,
      ExprListInExpr <$> exprList
    ]

arrayExpr =
  small
    $ choice
      [ ExprListArrayExpr <$> exprList,
        ArrayExprListArrayExpr <$> arrayExprList,
        pure EmptyArrayExpr
      ]

arrayExprList = nonEmpty (Range.exponential 1 4) arrayExpr

row =
  choice
    [ ExplicitRowRow <$> explicitRow,
      ImplicitRowRow <$> implicitRow
    ]

explicitRow = maybe exprList

implicitRow = ImplicitRow <$> exprList <*> aExpr

-- ** FuncExpr

funcExpr =
  choice
    [ ApplicationFuncExpr <$> funcApplication <*> maybe withinGroupClause <*> maybe filterClause <*> maybe overClause,
      SubexprFuncExpr <$> funcExprCommonSubexpr
    ]

funcExprWindowless =
  choice
    [ ApplicationFuncExprWindowless <$> funcApplication,
      CommonSubexprFuncExprWindowless <$> funcExprCommonSubexpr
    ]

funcApplication = FuncApplication <$> funcName <*> maybe funcApplicationParams

funcApplicationParams =
  choice
    [ NormalFuncApplicationParams <$> maybe allOrDistinct <*> nonEmpty (Range.exponential 1 8) funcArgExpr <*> maybe sortClause,
      VariadicFuncApplicationParams <$> maybe (nonEmpty (Range.exponential 1 8) funcArgExpr) <*> funcArgExpr <*> maybe sortClause,
      pure StarFuncApplicationParams
    ]

funcArgExpr =
  choice
    [ ExprFuncArgExpr <$> small aExpr,
      ColonEqualsFuncArgExpr <$> name <*> small aExpr,
      EqualsGreaterFuncArgExpr <$> name <*> small aExpr
    ]

withinGroupClause = sortClause

filterClause = aExpr

overClause = choice [WindowOverClause <$> windowSpecification, ColIdOverClause <$> colId]

funcExprCommonSubexpr =
  choice
    [ CollationForFuncExprCommonSubexpr <$> aExpr,
      pure CurrentDateFuncExprCommonSubexpr,
      CurrentTimeFuncExprCommonSubexpr <$> maybe iconst,
      CurrentTimestampFuncExprCommonSubexpr <$> maybe iconst,
      LocalTimeFuncExprCommonSubexpr <$> maybe iconst,
      LocalTimestampFuncExprCommonSubexpr <$> maybe iconst,
      pure CurrentRoleFuncExprCommonSubexpr,
      pure CurrentUserFuncExprCommonSubexpr,
      pure SessionUserFuncExprCommonSubexpr,
      pure UserFuncExprCommonSubexpr,
      pure CurrentCatalogFuncExprCommonSubexpr,
      pure CurrentSchemaFuncExprCommonSubexpr,
      CastFuncExprCommonSubexpr <$> aExpr <*> typename,
      ExtractFuncExprCommonSubexpr <$> maybe extractList,
      OverlayFuncExprCommonSubexpr <$> overlayList,
      PositionFuncExprCommonSubexpr <$> maybe positionList,
      SubstringFuncExprCommonSubexpr <$> maybe substrList,
      TreatFuncExprCommonSubexpr <$> aExpr <*> typename,
      TrimFuncExprCommonSubexpr <$> maybe trimModifier <*> trimList,
      NullIfFuncExprCommonSubexpr <$> aExpr <*> aExpr,
      CoalesceFuncExprCommonSubexpr <$> exprList,
      GreatestFuncExprCommonSubexpr <$> exprList,
      LeastFuncExprCommonSubexpr <$> exprList
    ]

extractList = ExtractList <$> extractArg <*> aExpr

extractArg =
  choice
    [ IdentExtractArg <$> ident,
      pure YearExtractArg,
      pure MonthExtractArg,
      pure DayExtractArg,
      pure HourExtractArg,
      pure MinuteExtractArg,
      pure SecondExtractArg,
      SconstExtractArg <$> sconst
    ]

overlayList = OverlayList <$> aExpr <*> overlayPlacing <*> substrFrom <*> maybe substrFor

overlayPlacing = aExpr

positionList = PositionList <$> bExpr <*> bExpr

substrList =
  choice
    [ ExprSubstrList <$> aExpr <*> substrListFromFor,
      ExprListSubstrList <$> exprList
    ]

substrListFromFor =
  choice
    [ FromForSubstrListFromFor <$> substrFrom <*> substrFor,
      ForFromSubstrListFromFor <$> substrFor <*> substrFrom,
      FromSubstrListFromFor <$> substrFrom,
      ForSubstrListFromFor <$> substrFor
    ]

substrFrom = aExpr

substrFor = aExpr

trimModifier = enumBounded

trimList =
  choice
    [ ExprFromExprListTrimList <$> aExpr <*> exprList,
      FromExprListTrimList <$> exprList,
      ExprListTrimList <$> exprList
    ]

-- * Operators

qualOp = choice [OpQualOp <$> op, OperatorQualOp <$> anyOperator]

qualAllOp =
  choice
    [ AllQualAllOp <$> allOp,
      AnyQualAllOp <$> anyOperator
    ]

op = do
  a <- text (Range.exponential 1 7) (listElement "+-*/<>=~!@#%^&|`?")
  case Validation.op a of
    Nothing -> return a
    _ -> discard

anyOperator =
  recursive
    choice
    [ AllOpAnyOperator <$> allOp
    ]
    [ QualifiedAnyOperator <$> colId <*> anyOperator
    ]

allOp = choice [OpAllOp <$> op, MathAllOp <$> mathOp]

mathOp = enumBounded

symbolicExprBinOp =
  choice
    [ MathSymbolicExprBinOp <$> mathOp,
      QualSymbolicExprBinOp <$> qualOp
    ]

binOp = element (toList KeywordSet.symbolicBinOp <> ["AND", "OR", "IS DISTINCT FROM", "IS NOT DISTINCT FROM"])

verbalExprBinOp = enumBounded

aExprReversableOp =
  choice
    [ pure NullAExprReversableOp,
      pure TrueAExprReversableOp,
      pure FalseAExprReversableOp,
      pure UnknownAExprReversableOp,
      DistinctFromAExprReversableOp <$> aExpr,
      OfAExprReversableOp <$> typeList,
      BetweenAExprReversableOp <$> bool <*> bExpr <*> aExpr,
      BetweenSymmetricAExprReversableOp <$> bExpr <*> aExpr,
      InAExprReversableOp <$> inExpr,
      pure DocumentAExprReversableOp
    ]

bExprIsOp =
  choice
    [ DistinctFromBExprIsOp <$> bExpr,
      OfBExprIsOp <$> typeList,
      pure DocumentBExprIsOp
    ]

subqueryOp =
  choice
    [ AllSubqueryOp <$> allOp,
      AnySubqueryOp <$> anyOperator,
      LikeSubqueryOp <$> bool,
      IlikeSubqueryOp <$> bool
    ]

-- * Constants

aexprConst =
  choice
    [ IAexprConst <$> iconst,
      FAexprConst <$> fconst,
      SAexprConst <$> sconst,
      BAexprConst <$> text (Range.exponential 1 100) (listElement "01"),
      XAexprConst <$> text (Range.exponential 1 100) (listElement "0123456789abcdefABCDEF"),
      FuncAexprConst <$> funcName <*> maybe funcConstArgs <*> sconst,
      ConstTypenameAexprConst <$> constTypename <*> sconst,
      StringIntervalAexprConst <$> sconst <*> maybe interval,
      IntIntervalAexprConst <$> integral (Range.exponential 0 2309482309483029) <*> sconst,
      BoolAexprConst <$> bool,
      pure NullAexprConst
    ]

funcConstArgs = FuncConstArgs <$> nonEmpty (Range.exponential 1 7) funcArgExpr <*> maybe sortClause

constTypename =
  choice
    [ NumericConstTypename <$> numeric,
      ConstBitConstTypename <$> constBit,
      ConstCharacterConstTypename <$> constCharacter,
      ConstDatetimeConstTypename <$> constDatetime
    ]

numeric =
  choice
    [ pure IntNumeric,
      pure IntegerNumeric,
      pure SmallintNumeric,
      pure BigintNumeric,
      pure RealNumeric,
      FloatNumeric <$> maybe iconst,
      pure DoublePrecisionNumeric,
      DecimalNumeric <$> maybe (nonEmpty (Range.exponential 1 7) (small aExpr)),
      DecNumeric <$> maybe (nonEmpty (Range.exponential 1 7) (small aExpr)),
      NumericNumeric <$> maybe (nonEmpty (Range.exponential 1 7) (small aExpr)),
      pure BooleanNumeric
    ]

bit = Bit <$> bool <*> maybe (nonEmpty (Range.exponential 1 7) (small aExpr))

constBit = bit

constCharacter = ConstCharacter <$> character <*> maybe iconst

character =
  choice
    [ CharacterCharacter <$> bool,
      CharCharacter <$> bool,
      pure VarcharCharacter,
      NationalCharacterCharacter <$> bool,
      NationalCharCharacter <$> bool,
      NcharCharacter <$> bool
    ]

constDatetime =
  choice
    [ TimestampConstDatetime <$> maybe iconst <*> maybe bool,
      TimeConstDatetime <$> maybe iconst <*> maybe bool
    ]

interval =
  choice
    [ pure YearInterval,
      pure MonthInterval,
      pure DayInterval,
      pure HourInterval,
      pure MinuteInterval,
      SecondInterval <$> intervalSecond,
      pure YearToMonthInterval,
      pure DayToHourInterval,
      pure DayToMinuteInterval,
      DayToSecondInterval <$> intervalSecond,
      pure HourToMinuteInterval,
      HourToSecondInterval <$> intervalSecond,
      MinuteToSecondInterval <$> intervalSecond
    ]

intervalSecond = maybe iconst

sconst = text (Range.exponential 0 1000) unicode

iconstOrFconst = choice [Left <$> iconst <|> Right <$> fconst]

fconst =
  filter (\a -> fromIntegral (round a :: Int) /= a)
    $ realFrac_ (Range.exponentialFloat 0 309457394857984375983475943)

iconst = integral (Range.exponential 0 maxBound)

-- * Types

nullable = pure False

arrayDimensionsAmount = int (Range.exponential 0 4)

-- ** Typename

typename = Typename <$> bool <*> simpleTypename <*> pure False <*> maybe ((,) <$> typenameArrayDimensions <*> pure False)

typenameArrayDimensions =
  choice
    [ BoundsTypenameArrayDimensions <$> arrayBounds,
      ExplicitTypenameArrayDimensions <$> maybe iconst
    ]

arrayBounds = nonEmpty (Range.exponential 1 4) (maybe iconst)

simpleTypename =
  choice
    [ GenericTypeSimpleTypename <$> genericType,
      NumericSimpleTypename <$> numeric,
      BitSimpleTypename <$> bit,
      CharacterSimpleTypename <$> character,
      ConstDatetimeSimpleTypename <$> constDatetime,
      ConstIntervalSimpleTypename <$> choice [Left <$> maybe interval, Right <$> iconst]
    ]

genericType = GenericType <$> typeFunctionName <*> maybe attrs <*> maybe typeModifiers

attrs = nonEmpty (Range.exponential 1 10) attrName

typeModifiers = exprList

typeList = nonEmpty (Range.exponential 1 7) typename

subType = enumBounded

-- * Names

columnref = Columnref <$> colId <*> maybe indirection

keywordNotInSet = \set -> notInSet set $ do
  a <- element startList
  b <- text (Range.linear 1 29) (element contList)
  return (Text.cons a b)
  where
    startList = "abcdefghijklmnopqrstuvwxyz_" <> List.filter isLower (enumFromTo '\200' '\377')
    contList = startList <> "0123456789$"

ident = identWithSet mempty

typeName = identWithSet KeywordSet.typeFunctionName

name = identWithSet KeywordSet.colId

cursorName = name

identWithSet set =
  frequency
    [ (95,) $ UnquotedIdent <$> (keywordNotInSet . HashSet.difference KeywordSet.keyword) set,
      (5,) $ QuotedIdent <$> text (Range.linear 1 30) quotedChar
    ]

qualifiedName =
  choice
    [ SimpleQualifiedName <$> name,
      IndirectedQualifiedName <$> name <*> indirection
    ]

indirection = nonEmpty (Range.linear 1 3) indirectionEl

indirectionEl =
  choice
    [ AttrNameIndirectionEl <$> name,
      pure AllIndirectionEl,
      ExprIndirectionEl <$> (small aExpr),
      SliceIndirectionEl <$> maybe (small aExpr) <*> maybe (small aExpr)
    ]

quotedChar = filter (not . isControl) unicode

colId = name

colLabel = name

attrName = colLabel

typeFunctionName = name

funcName =
  choice
    [ TypeFuncName <$> typeFunctionName,
      IndirectedFuncName <$> colId <*> indirection
    ]

anyName = AnyName <$> colId <*> maybe attrs

-- * Indexes

indexParams = nonEmpty (Range.exponential 1 5) indexElem

indexElem = IndexElem <$> indexElemDef <*> maybe collate <*> maybe class_ <*> maybe ascDesc <*> maybe nullsOrder

indexElemDef =
  choice
    [ IdIndexElemDef <$> colId,
      FuncIndexElemDef <$> funcExprWindowless,
      ExprIndexElemDef <$> aExpr
    ]

collate = anyName

class_ = anyName

ascDesc = enumBounded

nullsOrder = enumBounded

-- * Helpers

listElement :: [a] -> Gen a
listElement = element

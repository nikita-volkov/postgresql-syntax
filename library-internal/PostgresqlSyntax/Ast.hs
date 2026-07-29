-- |
-- Names for nodes mostly resemble the according definitions in the @gram.y@
-- original Postgres parser file, except for the cases where we can optimize on that.
--
-- For reasoning see the docs of the parsing module of this project.
module PostgresqlSyntax.Ast
  ( -- * Node modules
    module PostgresqlSyntax.Ast.AExpr,
    module PostgresqlSyntax.Ast.AExprReversableOp,
    module PostgresqlSyntax.Ast.AexprConst,
    module PostgresqlSyntax.Ast.AliasClause,
    module PostgresqlSyntax.Ast.AllOp,
    module PostgresqlSyntax.Ast.AnyName,
    module PostgresqlSyntax.Ast.AnyOperator,
    module PostgresqlSyntax.Ast.ArrayBounds,
    module PostgresqlSyntax.Ast.ArrayExpr,
    module PostgresqlSyntax.Ast.ArrayExprList,
    module PostgresqlSyntax.Ast.AscDesc,
    module PostgresqlSyntax.Ast.Attrs,
    module PostgresqlSyntax.Ast.BExpr,
    module PostgresqlSyntax.Ast.BExprIsOp,
    module PostgresqlSyntax.Ast.Bconst,
    module PostgresqlSyntax.Ast.Bit,
    module PostgresqlSyntax.Ast.CExpr,
    module PostgresqlSyntax.Ast.CallStmt,
    module PostgresqlSyntax.Ast.CaseExpr,
    module PostgresqlSyntax.Ast.Character,
    module PostgresqlSyntax.Ast.Columnref,
    module PostgresqlSyntax.Ast.CommonTableExpr,
    module PostgresqlSyntax.Ast.ConfExpr,
    module PostgresqlSyntax.Ast.ConstCharacter,
    module PostgresqlSyntax.Ast.ConstDatetime,
    module PostgresqlSyntax.Ast.ConstTypename,
    module PostgresqlSyntax.Ast.DeleteStmt,
    module PostgresqlSyntax.Ast.ExplicitRow,
    module PostgresqlSyntax.Ast.ExprList,
    module PostgresqlSyntax.Ast.ExtractArg,
    module PostgresqlSyntax.Ast.ExtractList,
    module PostgresqlSyntax.Ast.Fconst,
    module PostgresqlSyntax.Ast.ForLockingClause,
    module PostgresqlSyntax.Ast.ForLockingItem,
    module PostgresqlSyntax.Ast.ForLockingStrength,
    module PostgresqlSyntax.Ast.FrameBound,
    module PostgresqlSyntax.Ast.FrameClause,
    module PostgresqlSyntax.Ast.FrameClauseMode,
    module PostgresqlSyntax.Ast.FrameExtent,
    module PostgresqlSyntax.Ast.FuncAliasClause,
    module PostgresqlSyntax.Ast.FuncApplication,
    module PostgresqlSyntax.Ast.FuncApplicationParams,
    module PostgresqlSyntax.Ast.FuncArgExpr,
    module PostgresqlSyntax.Ast.FuncConstArgs,
    module PostgresqlSyntax.Ast.FuncExpr,
    module PostgresqlSyntax.Ast.FuncExprCommonSubexpr,
    module PostgresqlSyntax.Ast.FuncExprWindowless,
    module PostgresqlSyntax.Ast.FuncName,
    module PostgresqlSyntax.Ast.FuncTable,
    module PostgresqlSyntax.Ast.GenericType,
    module PostgresqlSyntax.Ast.GroupByItem,
    module PostgresqlSyntax.Ast.Iconst,
    module PostgresqlSyntax.Ast.Ident,
    module PostgresqlSyntax.Ast.ImplicitRow,
    module PostgresqlSyntax.Ast.InExpr,
    module PostgresqlSyntax.Ast.IndexElem,
    module PostgresqlSyntax.Ast.IndexElemDef,
    module PostgresqlSyntax.Ast.IndexParams,
    module PostgresqlSyntax.Ast.Indirection,
    module PostgresqlSyntax.Ast.IndirectionEl,
    module PostgresqlSyntax.Ast.InsertColumnItem,
    module PostgresqlSyntax.Ast.InsertColumnList,
    module PostgresqlSyntax.Ast.InsertRest,
    module PostgresqlSyntax.Ast.InsertStmt,
    module PostgresqlSyntax.Ast.InsertTarget,
    module PostgresqlSyntax.Ast.Interval,
    module PostgresqlSyntax.Ast.IntervalSecond,
    module PostgresqlSyntax.Ast.JoinMeth,
    module PostgresqlSyntax.Ast.JoinQual,
    module PostgresqlSyntax.Ast.JoinType,
    module PostgresqlSyntax.Ast.JoinedTable,
    module PostgresqlSyntax.Ast.LimitClause,
    module PostgresqlSyntax.Ast.MathOp,
    module PostgresqlSyntax.Ast.NameList,
    module PostgresqlSyntax.Ast.NullsOrder,
    module PostgresqlSyntax.Ast.Numeric,
    module PostgresqlSyntax.Ast.OffsetClause,
    module PostgresqlSyntax.Ast.OnConflict,
    module PostgresqlSyntax.Ast.OnConflictDo,
    module PostgresqlSyntax.Ast.Op,
    module PostgresqlSyntax.Ast.OptOrdinality,
    module PostgresqlSyntax.Ast.OptTempTableName,
    module PostgresqlSyntax.Ast.OptVarying,
    module PostgresqlSyntax.Ast.OverClause,
    module PostgresqlSyntax.Ast.OverlayList,
    module PostgresqlSyntax.Ast.OverrideKind,
    module PostgresqlSyntax.Ast.PositionList,
    module PostgresqlSyntax.Ast.PreparableStmt,
    module PostgresqlSyntax.Ast.QualAllOp,
    module PostgresqlSyntax.Ast.QualOp,
    module PostgresqlSyntax.Ast.QualifiedName,
    module PostgresqlSyntax.Ast.RelationExpr,
    module PostgresqlSyntax.Ast.RelationExprOptAlias,
    module PostgresqlSyntax.Ast.Row,
    module PostgresqlSyntax.Ast.RowsfromItem,
    module PostgresqlSyntax.Ast.RowsfromList,
    module PostgresqlSyntax.Ast.Sconst,
    module PostgresqlSyntax.Ast.SelectBinOp,
    module PostgresqlSyntax.Ast.SelectClause,
    module PostgresqlSyntax.Ast.SelectFetchFirstValue,
    module PostgresqlSyntax.Ast.SelectLimit,
    module PostgresqlSyntax.Ast.SelectLimitValue,
    module PostgresqlSyntax.Ast.SelectNoParens,
    module PostgresqlSyntax.Ast.SelectStmt,
    module PostgresqlSyntax.Ast.SelectWithParens,
    module PostgresqlSyntax.Ast.SetClause,
    module PostgresqlSyntax.Ast.SetClauseList,
    module PostgresqlSyntax.Ast.SetTarget,
    module PostgresqlSyntax.Ast.SetTargetList,
    module PostgresqlSyntax.Ast.SimpleSelect,
    module PostgresqlSyntax.Ast.SimpleTypename,
    module PostgresqlSyntax.Ast.SortBy,
    module PostgresqlSyntax.Ast.SortClause,
    module PostgresqlSyntax.Ast.SubType,
    module PostgresqlSyntax.Ast.SubqueryOp,
    module PostgresqlSyntax.Ast.SubstrList,
    module PostgresqlSyntax.Ast.SubstrListFromFor,
    module PostgresqlSyntax.Ast.SymbolicExprBinOp,
    module PostgresqlSyntax.Ast.TableFuncElement,
    module PostgresqlSyntax.Ast.TableFuncElementList,
    module PostgresqlSyntax.Ast.TableRef,
    module PostgresqlSyntax.Ast.TablesampleClause,
    module PostgresqlSyntax.Ast.TargetEl,
    module PostgresqlSyntax.Ast.TargetList,
    module PostgresqlSyntax.Ast.Targeting,
    module PostgresqlSyntax.Ast.Timezone,
    module PostgresqlSyntax.Ast.TrimList,
    module PostgresqlSyntax.Ast.TrimModifier,
    module PostgresqlSyntax.Ast.TypeList,
    module PostgresqlSyntax.Ast.Typename,
    module PostgresqlSyntax.Ast.TypenameArrayDimensions,
    module PostgresqlSyntax.Ast.UpdateStmt,
    module PostgresqlSyntax.Ast.VerbalExprBinOp,
    module PostgresqlSyntax.Ast.WhenClause,
    module PostgresqlSyntax.Ast.WhenClauseList,
    module PostgresqlSyntax.Ast.WhereOrCurrentClause,
    module PostgresqlSyntax.Ast.WindowDefinition,
    module PostgresqlSyntax.Ast.WindowExclusionClause,
    module PostgresqlSyntax.Ast.WindowSpecification,
    module PostgresqlSyntax.Ast.WithClause,
    module PostgresqlSyntax.Ast.Xconst,

    -- * Bare aliases
    ReturningClause,
    UsingClause,
    FromList,
    FromClause,
    IntoClause,
    HavingClause,
    ExistingWindowName,
    PartitionClause,
    RepeatableClause,
    ColDefList,
    CollateClause,
    WhereClause,
    WithinGroupClause,
    FilterClause,
    OverlayPlacing,
    SubstrFrom,
    SubstrFor,
    CaseArg,
    CaseDefault,
    ConstBit,
    ColId,
    ColLabel,
    Name,
    CursorName,
    AttrName,
    TypeModifiers,
    Collate,
    Class,
    TypeFunctionName,
    GroupClause,
    ValuesClause,
    WindowClause,
  )
where

import PostgresqlSyntax.Ast.AExpr hiding (filteredParser)
import PostgresqlSyntax.Ast.AExprReversableOp
import PostgresqlSyntax.Ast.AexprConst
import PostgresqlSyntax.Ast.AliasClause
import PostgresqlSyntax.Ast.AllOp
import PostgresqlSyntax.Ast.AnyName hiding (filteredParser)
import PostgresqlSyntax.Ast.AnyOperator
import PostgresqlSyntax.Ast.ArrayBounds
import PostgresqlSyntax.Ast.ArrayExpr
import PostgresqlSyntax.Ast.ArrayExprList
import PostgresqlSyntax.Ast.AscDesc
import PostgresqlSyntax.Ast.Attrs
import PostgresqlSyntax.Ast.BExpr
import PostgresqlSyntax.Ast.BExprIsOp
import PostgresqlSyntax.Ast.Bconst
import PostgresqlSyntax.Ast.Bit
import PostgresqlSyntax.Ast.CExpr hiding (customizedParser)
import PostgresqlSyntax.Ast.CallStmt
import PostgresqlSyntax.Ast.CaseExpr
import PostgresqlSyntax.Ast.Character
import PostgresqlSyntax.Ast.Columnref
import PostgresqlSyntax.Ast.CommonTableExpr
import PostgresqlSyntax.Ast.ConfExpr
import PostgresqlSyntax.Ast.ConstCharacter
import PostgresqlSyntax.Ast.ConstDatetime
import PostgresqlSyntax.Ast.ConstTypename
import PostgresqlSyntax.Ast.DeleteStmt
import PostgresqlSyntax.Ast.ExplicitRow
import PostgresqlSyntax.Ast.ExprList
import PostgresqlSyntax.Ast.ExtractArg
import PostgresqlSyntax.Ast.ExtractList
import PostgresqlSyntax.Ast.Fconst
import PostgresqlSyntax.Ast.ForLockingClause
import PostgresqlSyntax.Ast.ForLockingItem
import PostgresqlSyntax.Ast.ForLockingStrength
import PostgresqlSyntax.Ast.FrameBound
import PostgresqlSyntax.Ast.FrameClause
import PostgresqlSyntax.Ast.FrameClauseMode
import PostgresqlSyntax.Ast.FrameExtent
import PostgresqlSyntax.Ast.FuncAliasClause
import PostgresqlSyntax.Ast.FuncApplication
import PostgresqlSyntax.Ast.FuncApplicationParams
import PostgresqlSyntax.Ast.FuncArgExpr
import PostgresqlSyntax.Ast.FuncConstArgs
import PostgresqlSyntax.Ast.FuncExpr
import PostgresqlSyntax.Ast.FuncExprCommonSubexpr
import PostgresqlSyntax.Ast.FuncExprWindowless
import PostgresqlSyntax.Ast.FuncName
import PostgresqlSyntax.Ast.FuncTable
import PostgresqlSyntax.Ast.GenericType
import PostgresqlSyntax.Ast.GroupByItem
import PostgresqlSyntax.Ast.Iconst
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.ImplicitRow
import PostgresqlSyntax.Ast.InExpr
import PostgresqlSyntax.Ast.IndexElem
import PostgresqlSyntax.Ast.IndexElemDef
import PostgresqlSyntax.Ast.IndexParams
import PostgresqlSyntax.Ast.Indirection
import PostgresqlSyntax.Ast.IndirectionEl
import PostgresqlSyntax.Ast.InsertColumnItem
import PostgresqlSyntax.Ast.InsertColumnList
import PostgresqlSyntax.Ast.InsertRest
import PostgresqlSyntax.Ast.InsertStmt
import PostgresqlSyntax.Ast.InsertTarget
import PostgresqlSyntax.Ast.Interval
import PostgresqlSyntax.Ast.IntervalSecond
import PostgresqlSyntax.Ast.JoinMeth
import PostgresqlSyntax.Ast.JoinQual
import PostgresqlSyntax.Ast.JoinType
import PostgresqlSyntax.Ast.JoinedTable
import PostgresqlSyntax.Ast.LimitClause
import PostgresqlSyntax.Ast.MathOp
import PostgresqlSyntax.Ast.NameList
import PostgresqlSyntax.Ast.NullsOrder
import PostgresqlSyntax.Ast.Numeric
import PostgresqlSyntax.Ast.OffsetClause
import PostgresqlSyntax.Ast.OnConflict
import PostgresqlSyntax.Ast.OnConflictDo
import PostgresqlSyntax.Ast.Op
import PostgresqlSyntax.Ast.OptOrdinality
import PostgresqlSyntax.Ast.OptTempTableName
import PostgresqlSyntax.Ast.OptVarying
import PostgresqlSyntax.Ast.OverClause
import PostgresqlSyntax.Ast.OverlayList
import PostgresqlSyntax.Ast.OverrideKind
import PostgresqlSyntax.Ast.PositionList
import PostgresqlSyntax.Ast.PreparableStmt
import PostgresqlSyntax.Ast.QualAllOp
import PostgresqlSyntax.Ast.QualOp
import PostgresqlSyntax.Ast.QualifiedName
import PostgresqlSyntax.Ast.RelationExpr
import PostgresqlSyntax.Ast.RelationExprOptAlias hiding (customizedParser)
import PostgresqlSyntax.Ast.Row
import PostgresqlSyntax.Ast.RowsfromItem
import PostgresqlSyntax.Ast.RowsfromList
import PostgresqlSyntax.Ast.Sconst
import PostgresqlSyntax.Ast.SelectBinOp
import PostgresqlSyntax.Ast.SelectClause
import PostgresqlSyntax.Ast.SelectFetchFirstValue
import PostgresqlSyntax.Ast.SelectLimit
import PostgresqlSyntax.Ast.SelectLimitValue
import PostgresqlSyntax.Ast.SelectNoParens
import PostgresqlSyntax.Ast.SelectStmt
import PostgresqlSyntax.Ast.SelectWithParens
import PostgresqlSyntax.Ast.SetClause
import PostgresqlSyntax.Ast.SetClauseList
import PostgresqlSyntax.Ast.SetTarget
import PostgresqlSyntax.Ast.SetTargetList
import PostgresqlSyntax.Ast.SimpleSelect
import PostgresqlSyntax.Ast.SimpleTypename
import PostgresqlSyntax.Ast.SortBy
import PostgresqlSyntax.Ast.SortClause
import PostgresqlSyntax.Ast.SubType
import PostgresqlSyntax.Ast.SubqueryOp
import PostgresqlSyntax.Ast.SubstrList
import PostgresqlSyntax.Ast.SubstrListFromFor
import PostgresqlSyntax.Ast.SymbolicExprBinOp
import PostgresqlSyntax.Ast.TableFuncElement
import PostgresqlSyntax.Ast.TableFuncElementList
import PostgresqlSyntax.Ast.TableRef
import PostgresqlSyntax.Ast.TablesampleClause
import PostgresqlSyntax.Ast.TargetEl
import PostgresqlSyntax.Ast.TargetList
import PostgresqlSyntax.Ast.Targeting
import PostgresqlSyntax.Ast.Timezone
import PostgresqlSyntax.Ast.TrimList
import PostgresqlSyntax.Ast.TrimModifier
import PostgresqlSyntax.Ast.TypeList
import PostgresqlSyntax.Ast.Typename
import PostgresqlSyntax.Ast.TypenameArrayDimensions
import PostgresqlSyntax.Ast.UpdateStmt
import PostgresqlSyntax.Ast.VerbalExprBinOp
import PostgresqlSyntax.Ast.WhenClause
import PostgresqlSyntax.Ast.WhenClauseList
import PostgresqlSyntax.Ast.WhereOrCurrentClause
import PostgresqlSyntax.Ast.WindowDefinition
import PostgresqlSyntax.Ast.WindowExclusionClause
import PostgresqlSyntax.Ast.WindowSpecification
import PostgresqlSyntax.Ast.WithClause
import PostgresqlSyntax.Ast.Xconst
import PostgresqlSyntax.Prelude

-- * Bare aliases

type ReturningClause = TargetList

type UsingClause = FromList

type FromList = NonEmpty TableRef

type FromClause = NonEmpty TableRef

type IntoClause = OptTempTableName

type HavingClause = AExpr

type ExistingWindowName = ColId

type PartitionClause = ExprList

type RepeatableClause = AExpr

type ColDefList = TableFuncElementList

type CollateClause = AnyName

type WhereClause = AExpr

type WithinGroupClause = SortClause

type FilterClause = AExpr

type OverlayPlacing = AExpr

type SubstrFrom = AExpr

type SubstrFor = AExpr

type CaseArg = AExpr

type CaseDefault = AExpr

type ConstBit = Bit

type ColId = Ident

type ColLabel = Ident

type Name = ColId

type CursorName = Name

type AttrName = ColLabel

type TypeModifiers = ExprList

type Collate = AnyName

type Class = AnyName

type TypeFunctionName = Ident

type GroupClause = NonEmpty GroupByItem

type ValuesClause = NonEmpty ExprList

type WindowClause = NonEmpty WindowDefinition

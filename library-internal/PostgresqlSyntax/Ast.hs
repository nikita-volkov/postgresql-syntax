-- |
-- Names for nodes mostly resemble the according definitions in the @gram.y@
-- original Postgres parser file, except for the cases where we can optimize on that.
--
-- For reasoning see the docs of the parsing module of this project.
module PostgresqlSyntax.Ast
  ( -- * Nodes
    AExpr (..),
    AExprReversableOp (..),
    AexprConst (..),
    AliasClause (..),
    AllOp (..),
    AnyName (..),
    AnyOperator (..),
    ArrayBounds (..),
    ArrayExpr (..),
    ArrayExprList (..),
    AscDesc (..),
    Attrs (..),
    BExpr (..),
    BExprIsOp (..),
    Bconst (..),
    Bit (..),
    CExpr (..),
    CallStmt (..),
    CaseExpr (..),
    Character (..),
    Columnref (..),
    CommonTableExpr (..),
    ConfExpr (..),
    ConstCharacter (..),
    ConstDatetime (..),
    ConstTypename (..),
    DeleteStmt (..),
    ExplicitRow (..),
    ExprList (..),
    ExtractArg (..),
    ExtractList (..),
    Fconst (..),
    ForLockingClause (..),
    ForLockingItem (..),
    ForLockingStrength (..),
    FrameBound (..),
    FrameClause (..),
    FrameClauseMode (..),
    FrameExtent (..),
    FuncAliasClause (..),
    FuncApplication (..),
    FuncApplicationParams (..),
    FuncArgExpr (..),
    FuncConstArgs (..),
    FuncExpr (..),
    FuncExprCommonSubexpr (..),
    FuncExprWindowless (..),
    FuncName (..),
    FuncTable (..),
    GenericType (..),
    GroupByItem (..),
    Iconst (..),
    Ident (..),
    ImplicitRow (..),
    InExpr (..),
    IndexElem (..),
    IndexElemDef (..),
    IndexParams (..),
    Indirection (..),
    IndirectionEl (..),
    InsertColumnItem (..),
    InsertColumnList (..),
    InsertRest (..),
    InsertStmt (..),
    InsertTarget (..),
    Interval (..),
    IntervalSecond (..),
    JoinMeth (..),
    JoinQual (..),
    JoinType (..),
    JoinedTable (..),
    LimitClause (..),
    MathOp (..),
    NameList (..),
    NullsOrder (..),
    Numeric (..),
    OffsetClause (..),
    OnConflict (..),
    OnConflictDo (..),
    Op (..),
    OptOrdinality (..),
    OptTempTableName (..),
    OptVarying (..),
    OverClause (..),
    OverlayList (..),
    OverrideKind (..),
    PositionList (..),
    PreparableStmt (..),
    QualAllOp (..),
    QualOp (..),
    QualifiedName (..),
    RelationExpr (..),
    RelationExprOptAlias (..),
    Row (..),
    RowsfromItem (..),
    RowsfromList (..),
    Sconst (..),
    SelectBinOp (..),
    SelectClause (..),
    SelectFetchFirstValue (..),
    SelectLimit (..),
    SelectLimitValue (..),
    SelectNoParens (..),
    SelectStmt (..),
    SelectWithParens (..),
    SetClause (..),
    SetClauseList (..),
    SetTarget (..),
    SetTargetList (..),
    SimpleSelect (..),
    SimpleTypename (..),
    SortBy (..),
    SortClause (..),
    SubType (..),
    SubqueryOp (..),
    SubstrList (..),
    SubstrListFromFor (..),
    SymbolicExprBinOp (..),
    TableFuncElement (..),
    TableFuncElementList (..),
    TableRef (..),
    TablesampleClause (..),
    TargetEl (..),
    TargetList (..),
    Targeting (..),
    Timezone (..),
    TrimList (..),
    TrimModifier (..),
    TypeList (..),
    Typename (..),
    TypenameArrayDimensions (..),
    UpdateStmt (..),
    VerbalExprBinOp (..),
    WhenClause (..),
    WhenClauseList (..),
    WhereOrCurrentClause (..),
    WindowDefinition (..),
    WindowExclusionClause (..),
    WindowSpecification (..),
    WithClause (..),
    Xconst (..),

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

import PostgresqlSyntax.Ast.AExpr
import PostgresqlSyntax.Ast.AExprReversableOp
import PostgresqlSyntax.Ast.AexprConst
import PostgresqlSyntax.Ast.AliasClause
import PostgresqlSyntax.Ast.AllOp
import PostgresqlSyntax.Ast.AnyName
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
import PostgresqlSyntax.Ast.CExpr
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
import PostgresqlSyntax.Ast.RelationExprOptAlias
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

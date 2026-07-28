module PostgresqlSyntax.Ast.SimpleSelect
  ( SimpleSelect (..),
    baseSimpleSelect,
    selectClauseBase,
    extendSelectClause,
  )
where

import qualified HeadedMegaparsec as Parser
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.ExprList
import PostgresqlSyntax.Ast.GroupByItem
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.OptTempTableName
import PostgresqlSyntax.Ast.RelationExpr
import PostgresqlSyntax.Ast.SelectBinOp
import PostgresqlSyntax.Ast.SelectClause
import PostgresqlSyntax.Ast.TableRef
import PostgresqlSyntax.Ast.Targeting
import PostgresqlSyntax.Ast.WindowDefinition
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- simple_select:
--   |  SELECT opt_all_clause opt_target_list
--       into_clause from_clause where_clause
--       group_clause having_clause window_clause
--   |  SELECT distinct_clause target_list
--       into_clause from_clause where_clause
--       group_clause having_clause window_clause
--   |  values_clause
--   |  TABLE relation_expr
--   |  select_clause UNION all_or_distinct select_clause
--   |  select_clause INTERSECT all_or_distinct select_clause
--   |  select_clause EXCEPT all_or_distinct select_clause
-- @
--
-- Hosts the real @select_clause@ grammar (including its
-- @UNION@\/@INTERSECT@\/@EXCEPT@-chaining) for both itself and
-- "PostgresqlSyntax.Ast.SelectNoParens", which shares it — see
-- 'PostgresqlSyntax.Ast.SelectClause'\'s module documentation for why.
data SimpleSelect
  = NormalSimpleSelect (Maybe Targeting) (Maybe OptTempTableName) (Maybe (NonEmpty TableRef)) (Maybe AExpr) (Maybe (NonEmpty GroupByItem)) (Maybe AExpr) (Maybe (NonEmpty WindowDefinition))
  | ValuesSimpleSelect (NonEmpty ExprList)
  | TableSimpleSelect RelationExpr
  | BinSimpleSelect SelectBinOp SelectClause (Maybe Bool) SelectClause
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst SimpleSelect where
  toTextBuilder = \case
    NormalSimpleSelect a b c d e f g ->
      optLexemes
        [ Just "SELECT",
          fmap toTextBuilder a,
          fmap intoClause b,
          fmap fromClause c,
          fmap whereClause d,
          fmap groupClause e,
          fmap havingClause f,
          fmap windowClause g
        ]
    ValuesSimpleSelect a -> valuesClause a
    TableSimpleSelect a -> "TABLE " <> toTextBuilder a
    BinSimpleSelect a b c d -> toTextBuilder b <> " " <> toTextBuilder a <> foldMap (mappend " " . renderAllOrDistinct) c <> " " <> toTextBuilder d
    where
      intoClause a = "INTO " <> toTextBuilder a
      fromClause a = "FROM " <> commaNonEmpty toTextBuilder a
      whereClause a = "WHERE " <> toTextBuilder a
      groupClause a = "GROUP BY " <> commaNonEmpty toTextBuilder a
      havingClause a = "HAVING " <> toTextBuilder a
      windowClause a = "WINDOW " <> commaNonEmpty toTextBuilder a
      valuesClause a = "VALUES " <> commaNonEmpty (renderInParens . toTextBuilder) a
  parser = do
    a <- baseSimpleSelect
    extendMany suffix a
    where
      suffix headSimpleSelect = do
        op <- Parser.space1 *> parser <* Parser.space1
        Parser.endHead
        distinct <- optional (allOrDistinct <* Parser.space1)
        rhs <- selectClauseBase >>= extendSelectClause
        return (BinSimpleSelect op (SimpleSelectSelectClause headSimpleSelect) distinct rhs)

-- |
-- The non-recursive base cases only (no @select_clause BINOP
-- select_clause@ extension) — see this module's own boot-exposed
-- signature.
baseSimpleSelect :: Parser SimpleSelect
baseSimpleSelect =
  asum
    [ do
        keyword "select"
        Parser.notFollowedBy $ Parser.satisfy isAlphaNum
        Parser.endHead
        targeting <- optional (Parser.space1 *> parser)
        intoClause <- optional (Parser.space1 *> keyword "into" *> Parser.endHead *> Parser.space1 *> parser)
        fromClause <- optional (Parser.space1 *> keyword "from" *> Parser.endHead *> Parser.space1 *> Parser.sep1 commaSeparator parser)
        whereClause <- optional (Parser.space1 *> keyword "where" *> Parser.space1 *> Parser.endHead *> parser)
        groupClause <- optional (Parser.space1 *> keyphrase "group by" *> Parser.endHead *> Parser.space1 *> Parser.sep1 commaSeparator parser)
        havingClause <- optional (Parser.space1 *> keyword "having" *> Parser.endHead *> Parser.space1 *> parser)
        windowClause <- optional (Parser.space1 *> keyword "window" *> Parser.endHead *> Parser.space1 *> Parser.sep1 commaSeparator parser)
        return (NormalSimpleSelect targeting intoClause fromClause whereClause groupClause havingClause windowClause),
      do
        keyword "table"
        Parser.space1
        Parser.endHead
        TableSimpleSelect <$> parser,
      ValuesSimpleSelect <$> valuesClause
    ]
  where
    valuesClause = do
      keyword "values"
      Parser.space
      Parser.sep1 commaSeparator $ do
        Parser.char '('
        Parser.endHead
        Parser.space
        a <- ExprList <$> Parser.sep1 commaSeparator parser
        Parser.space
        Parser.char ')'
        return a

selectClauseBase :: Parser SelectClause
selectClauseBase =
  asum
    [ WithParensSelectClause <$> parser,
      SimpleSelectSelectClause <$> baseSimpleSelect
    ]

extendSelectClause :: SelectClause -> Parser SelectClause
extendSelectClause = extendMany suffix
  where
    suffix headSelectClause = SimpleSelectSelectClause <$> extensionSimpleSelect headSelectClause
    extensionSimpleSelect headSelectClause = do
      op <- Parser.space1 *> parser <* Parser.space1
      Parser.endHead
      distinct <- optional (allOrDistinct <* Parser.space1)
      rhs <- selectClauseBase >>= extendSelectClause
      return (BinSimpleSelect op headSelectClause distinct rhs)

instance Qc.Arbitrary SimpleSelect where
  arbitrary =
    Qc.sized $ \n ->
      if n <= 1
        then TableSimpleSelect <$> Qc.arbitrary
        else
          Qc.resize (div n 2) $
            Qc.oneof
              [ NormalSimpleSelect
                  <$> Qc.arbitrary
                  <*> Qc.arbitrary
                  <*> Qc.arbitrary
                  <*> Qc.arbitrary
                  <*> Qc.arbitrary
                  <*> Qc.arbitrary
                  <*> Qc.arbitrary,
                ValuesSimpleSelect <$> Qc.nonEmptyUpTo 7 Qc.arbitrary,
                TableSimpleSelect <$> Qc.arbitrary,
                BinSimpleSelect <$> Qc.arbitrary <*> Qc.arbitrary <*> Qc.arbitrary <*> Qc.arbitrary
              ]

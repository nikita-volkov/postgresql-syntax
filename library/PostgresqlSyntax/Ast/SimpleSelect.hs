module PostgresqlSyntax.Ast.SimpleSelect
  ( SimpleSelect (..),
    baseSimpleSelect,
    selectClauseBase,
    extendSelectClause,
  )
where

import HeadedMegaparsec
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
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import Test.QuickCheck (scale)

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
        op <- space1 *> parser <* space1
        endHead
        distinct <- optional (allOrDistinct <* space1)
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
        notFollowedBy $ satisfy isAlphaNum
        endHead
        targeting <- optional (space1 *> parser)
        intoClause <- optional (space1 *> keyword "into" *> endHead *> space1 *> parser)
        fromClause <- optional (space1 *> keyword "from" *> endHead *> space1 *> sep1 commaSeparator parser)
        whereClause <- optional (space1 *> keyword "where" *> space1 *> endHead *> parser)
        groupClause <- optional (space1 *> keyphrase "group by" *> endHead *> space1 *> sep1 commaSeparator parser)
        havingClause <- optional (space1 *> keyword "having" *> endHead *> space1 *> parser)
        windowClause <- optional (space1 *> keyword "window" *> endHead *> space1 *> sep1 commaSeparator parser)
        return (NormalSimpleSelect targeting intoClause fromClause whereClause groupClause havingClause windowClause),
      do
        keyword "table"
        space1
        endHead
        TableSimpleSelect <$> parser,
      ValuesSimpleSelect <$> valuesClause
    ]
  where
    valuesClause = do
      keyword "values"
      space
      sep1 commaSeparator $ do
        char '('
        endHead
        space
        a <- ExprList <$> sep1 commaSeparator parser
        space
        char ')'
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
      op <- space1 *> parser <* space1
      endHead
      distinct <- optional (allOrDistinct <* space1)
      rhs <- selectClauseBase >>= extendSelectClause
      return (BinSimpleSelect op headSelectClause distinct rhs)

instance Arbitrary SimpleSelect where
  arbitrary =
    sized $ \n ->
      if n <= 1
        then TableSimpleSelect <$> arbitrary
        else
          oneof
            [ NormalSimpleSelect
                <$> scale (`div` 7) arbitrary
                <*> scale (`div` 7) arbitrary
                <*> scale (`div` 7) arbitrary
                <*> scale (`div` 7) arbitrary
                <*> scale (`div` 7) arbitrary
                <*> scale (`div` 7) arbitrary
                <*> scale (`div` 7) arbitrary,
              ValuesSimpleSelect <$> do
                len <- choose (0, 7)
                x <- scale (`div` 2) arbitrary
                xs <- vectorOf len (scale (`div` 2) arbitrary)
                pure (x :| xs),
              TableSimpleSelect <$> arbitrary,
              BinSimpleSelect <$> arbitrary <*> scale (`div` 2) arbitrary <*> arbitrary <*> scale (`div` 2) arbitrary
            ]

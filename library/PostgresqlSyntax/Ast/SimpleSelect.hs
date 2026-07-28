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
import PostgresqlSyntax.Ast.OptTempTableName
import PostgresqlSyntax.Ast.RelationExpr
import PostgresqlSyntax.Ast.SelectBinOp
import PostgresqlSyntax.Ast.SelectClause
import PostgresqlSyntax.Ast.TableRef
import PostgresqlSyntax.Ast.Targeting
import PostgresqlSyntax.Ast.WindowDefinition
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
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
      TextBuilders.optLexemes
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
    BinSimpleSelect a b c d -> toTextBuilder b <> " " <> toTextBuilder a <> foldMap (mappend " " . TextBuilders.renderAllOrDistinct) c <> " " <> toTextBuilder d
    where
      intoClause a = "INTO " <> toTextBuilder a
      fromClause a = "FROM " <> TextBuilders.commaNonEmpty toTextBuilder a
      whereClause a = "WHERE " <> toTextBuilder a
      groupClause a = "GROUP BY " <> TextBuilders.commaNonEmpty toTextBuilder a
      havingClause a = "HAVING " <> toTextBuilder a
      windowClause a = "WINDOW " <> TextBuilders.commaNonEmpty toTextBuilder a
      valuesClause a = "VALUES " <> TextBuilders.commaNonEmpty (TextBuilders.renderInParens . toTextBuilder) a
  parser = do
    a <- baseSimpleSelect
    extendMany suffix a
    where
      suffix headSimpleSelect = do
        op <- Parsers.space1 *> parser <* Parsers.space1
        Parser.endHead
        distinct <- optional (Parsers.allOrDistinct <* Parsers.space1)
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
        Parsers.keyword "select"
        Parsers.notFollowedBy $ Parsers.satisfy isAlphaNum
        Parser.endHead
        targeting <- optional (Parsers.space1 *> parser)
        intoClause <- optional (Parsers.space1 *> Parsers.keyword "into" *> Parser.endHead *> Parsers.space1 *> parser)
        fromClause <- optional (Parsers.space1 *> Parsers.keyword "from" *> Parser.endHead *> Parsers.space1 *> Parsers.sep1 Parsers.commaSeparator parser)
        whereClause <- optional (Parsers.space1 *> Parsers.keyword "where" *> Parsers.space1 *> Parser.endHead *> parser)
        groupClause <- optional (Parsers.space1 *> Parsers.keyphrase "group by" *> Parser.endHead *> Parsers.space1 *> Parsers.sep1 Parsers.commaSeparator parser)
        havingClause <- optional (Parsers.space1 *> Parsers.keyword "having" *> Parser.endHead *> Parsers.space1 *> parser)
        windowClause <- optional (Parsers.space1 *> Parsers.keyword "window" *> Parser.endHead *> Parsers.space1 *> Parsers.sep1 Parsers.commaSeparator parser)
        return (NormalSimpleSelect targeting intoClause fromClause whereClause groupClause havingClause windowClause),
      do
        Parsers.keyword "table"
        Parsers.space1
        Parser.endHead
        TableSimpleSelect <$> parser,
      ValuesSimpleSelect <$> valuesClause
    ]
  where
    valuesClause = do
      Parsers.keyword "values"
      Parsers.space
      Parsers.sep1 Parsers.commaSeparator $ do
        Parsers.char '('
        Parser.endHead
        Parsers.space
        a <- ExprList <$> Parsers.sep1 Parsers.commaSeparator parser
        Parsers.space
        Parsers.char ')'
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
      op <- Parsers.space1 *> parser <* Parsers.space1
      Parser.endHead
      distinct <- optional (Parsers.allOrDistinct <* Parsers.space1)
      rhs <- selectClauseBase >>= extendSelectClause
      return (BinSimpleSelect op headSelectClause distinct rhs)

instance Qc.Arbitrary SimpleSelect where
  shrink = Qc.genericShrink
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

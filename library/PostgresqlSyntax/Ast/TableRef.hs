module PostgresqlSyntax.Ast.TableRef where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.AliasClause
import PostgresqlSyntax.Ast.FuncAliasClause
import PostgresqlSyntax.Ast.FuncTable
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.JoinMeth
import PostgresqlSyntax.Ast.JoinedTable
import PostgresqlSyntax.Ast.RelationExpr
import {-# SOURCE #-} PostgresqlSyntax.Ast.SelectWithParens (SelectWithParens)
import PostgresqlSyntax.Ast.TablesampleClause
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, head, many, some, tail, try)
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- | relation_expr opt_alias_clause
-- | relation_expr opt_alias_clause tablesample_clause
-- | func_table func_alias_clause
-- | LATERAL_P func_table func_alias_clause
-- | xmltable opt_alias_clause
-- | LATERAL_P xmltable opt_alias_clause
-- | select_with_parens opt_alias_clause
-- | LATERAL_P select_with_parens opt_alias_clause
-- | joined_table
-- | '(' joined_table ')' alias_clause
--
-- TODO: Add xmltable
-- @
--
-- 'PostgresqlSyntax.Ast.JoinedTable' and 'PostgresqlSyntax.Ast.JoinMeth'
-- have their own modules for the type declarations, but the actual
-- parsing\/rendering of joined tables is hosted here (and here alone),
-- since it's genuinely mutually recursive with table-ref parsing itself
-- (a @table_ref@ can be a @joined_table@, and a @joined_table@'s branches
-- each embed two @table_ref@s) — see the doc on 'JoinMeth' for why that
-- type's own instance isn't what's used below.
data TableRef
  = -- |
    -- @
    --    | relation_expr opt_alias_clause
    --    | relation_expr opt_alias_clause tablesample_clause
    -- @
    RelationExprTableRef RelationExpr (Maybe AliasClause) (Maybe TablesampleClause)
  | -- |
    -- @
    --    | func_table func_alias_clause
    --    | LATERAL_P func_table func_alias_clause
    -- @
    FuncTableRef Bool FuncTable (Maybe FuncAliasClause)
  | -- |
    -- @
    --    | select_with_parens opt_alias_clause
    --    | LATERAL_P select_with_parens opt_alias_clause
    -- @
    SelectTableRef Bool SelectWithParens (Maybe AliasClause)
  | -- |
    -- @
    --    | joined_table
    --    | '(' joined_table ')' alias_clause
    -- @
    JoinTableRef JoinedTable (Maybe AliasClause)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst TableRef where
  toTextBuilder = renderTableRef
    where
      renderTableRef = \case
        RelationExprTableRef a b c ->
          optLexemes
            [ Just (toTextBuilder a),
              fmap toTextBuilder b,
              fmap toTextBuilder c
            ]
        FuncTableRef a b c ->
          optLexemes
            [ if a then Just "LATERAL" else Nothing,
              Just (toTextBuilder b),
              fmap toTextBuilder c
            ]
        SelectTableRef a b c ->
          optLexemes
            [ if a then Just "LATERAL" else Nothing,
              Just (toTextBuilder b),
              fmap toTextBuilder c
            ]
        JoinTableRef a b -> case b of
          Just c -> renderInParens (renderJoinedTable a) <> " " <> toTextBuilder c
          Nothing -> renderJoinedTable a
      renderJoinedTable = \case
        InParensJoinedTable a -> renderInParens (renderJoinedTable a)
        MethJoinedTable a b c -> case a of
          CrossJoinMeth -> renderTableRef b <> " CROSS JOIN " <> renderTableRef c
          QualJoinMeth d e -> renderTableRef b <> suffixMaybe toTextBuilder d <> " JOIN " <> renderTableRef c <> " " <> toTextBuilder e
          NaturalJoinMeth d -> renderTableRef b <> " NATURAL" <> suffixMaybe toTextBuilder d <> " JOIN " <> renderTableRef c

  --
  -- >>> testParser tableRef "a left join b on (a.i = b.i)"
  -- JoinTableRef (MethJoinedTable (QualJoinMeth...
  parser =
    Parser.label "table reference" $
      do
        tr <- nonTrailingTableRef
        recur tr
    where
      recur tr =
        asum
          [ do
              tr2 <- Parser.wrapToHead (Parser.space1 *> trailingTableRef tr)
              Parser.endHead
              recur tr2,
            pure tr
          ]
      nonTrailingTableRef =
        asum
          [lateralTableRef <|> Parser.wrapToHead nonLateralTableRef <|> relationExprTableRef <|> joinedTableWithAliasTableRef <|> inParensJoinedTableTableRef]
        where
          relationExprTableRef = do
            relationExpr <- parser
            Parser.endHead
            optAliasClause <- optional (Parser.space1 *> parser)
            optTablesampleClause <- optional (Parser.space1 *> parser)
            return (RelationExprTableRef relationExpr optAliasClause optTablesampleClause)
          lateralTableRef = do
            keyword "lateral"
            Parser.space1
            Parser.endHead
            lateralableTableRef True
          nonLateralTableRef = lateralableTableRef False
          lateralableTableRef lateral =
            asum
              [ do
                  a <- parser
                  b <- optional (Parser.space1 *> parser)
                  return (FuncTableRef lateral a b),
                do
                  select <- parser
                  optAliasClause <- optional $ Parser.space1 *> parser
                  return (SelectTableRef lateral select optAliasClause)
              ]
          inParensJoinedTableTableRef = JoinTableRef <$> inParensJoinedTable <*> pure Nothing
          joinedTableWithAliasTableRef = do
            joinedTable <- Parser.wrapToHead (inParens joinedTable)
            Parser.space1
            alias <- parser
            return (JoinTableRef joinedTable (Just alias))
      trailingTableRef tableRef =
        JoinTableRef <$> trailingJoinedTable tableRef <*> pure Nothing
      joinedTable =
        headP >>= tailP
        where
          headP =
            asum
              [ do
                  tr <- Parser.wrapToHead nonTrailingTableRef
                  Parser.space1
                  trailingJoinedTable tr,
                inParensJoinedTable
              ]
          tailP jt =
            asum
              [ do
                  jt2 <- Parser.wrapToHead (Parser.space1 *> trailingJoinedTable (JoinTableRef jt Nothing))
                  Parser.endHead
                  tailP jt2,
                pure jt
              ]

      -- ==== References
      -- @
      --   | '(' joined_table ')'
      -- @
      inParensJoinedTable = InParensJoinedTable <$> inParens joinedTable

      -- ==== References
      -- @
      --   | table_ref CROSS JOIN table_ref
      --   | table_ref join_type JOIN table_ref join_qual
      --   | table_ref JOIN table_ref join_qual
      --   | table_ref NATURAL join_type JOIN table_ref
      --   | table_ref NATURAL JOIN table_ref
      -- @
      trailingJoinedTable tr1 =
        asum
          [ do
              keyphrase "cross join"
              Parser.endHead
              Parser.space1
              tr2 <- nonTrailingTableRef
              return (MethJoinedTable CrossJoinMeth tr1 tr2),
            do
              jt <- joinTypedJoin
              Parser.endHead
              Parser.space1
              tr2 <- parser
              Parser.space1
              jq <- parser
              return (MethJoinedTable (QualJoinMeth jt jq) tr1 tr2),
            do
              keyword "natural"
              Parser.endHead
              Parser.space1
              jt <- joinTypedJoin
              Parser.space1
              tr2 <- nonTrailingTableRef
              return (MethJoinedTable (NaturalJoinMeth jt) tr1 tr2)
          ]
        where
          joinTypedJoin =
            Just
              <$> (parser <* Parser.endHead <* Parser.space1 <* keyword "join")
                <|> Nothing
              <$ keyword "join"

instance Qc.Arbitrary TableRef where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.sized $ \n ->
      if n <= 1
        then RelationExprTableRef <$> Qc.arbitrary <*> pure Nothing <*> pure Nothing
        else
          Qc.oneof
            [ RelationExprTableRef <$> Qc.arbitrary <*> Qc.scale (`div` 2) Qc.arbitrary <*> Qc.scale (`div` 2) Qc.arbitrary,
              FuncTableRef <$> Qc.arbitrary <*> Qc.scale (`div` 2) Qc.arbitrary <*> Qc.scale (`div` 2) Qc.arbitrary,
              SelectTableRef <$> Qc.arbitrary <*> Qc.scale (`div` 2) Qc.arbitrary <*> Qc.scale (`div` 2) Qc.arbitrary,
              JoinTableRef <$> Qc.scale (`div` 2) Qc.arbitrary <*> Qc.scale (`div` 2) Qc.arbitrary
            ]

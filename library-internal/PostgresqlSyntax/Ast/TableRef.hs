module PostgresqlSyntax.Ast.TableRef where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.AliasClause
import PostgresqlSyntax.Ast.FuncAliasClause
import PostgresqlSyntax.Ast.FuncTable
import PostgresqlSyntax.Ast.JoinMeth
import PostgresqlSyntax.Ast.JoinedTable
import PostgresqlSyntax.Ast.RelationExpr
import {-# SOURCE #-} PostgresqlSyntax.Ast.SelectWithParens (SelectWithParens)
import PostgresqlSyntax.Ast.TablesampleClause
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, head, many, some, tail, try)
import PostgresqlSyntax.Settings (Settings)
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
  toTextBuilder settings = renderTableRef settings

  parser settings =
    Parser.label "table reference" $
      do
        tr <- nonTrailingTableRef settings
        recur settings tr

-- |
-- Renders a 'TableRef'. Also used, via 'renderJoinedTable', by
-- "PostgresqlSyntax.Ast.JoinedTable"\'s own 'IsAst' instance — see the doc
-- there for why that module doesn't maintain its own copy of this logic.
renderTableRef :: Settings -> TableRef -> TextBuilder
renderTableRef settings = \case
  RelationExprTableRef a b c ->
    TextBuilders.optLexemes
      [ Just (toTextBuilder settings a),
        fmap (toTextBuilder settings) b,
        fmap (toTextBuilder settings) c
      ]
  FuncTableRef a b c ->
    TextBuilders.optLexemes
      [ if a then Just "LATERAL" else Nothing,
        Just (toTextBuilder settings b),
        fmap (toTextBuilder settings) c
      ]
  SelectTableRef a b c ->
    TextBuilders.optLexemes
      [ if a then Just "LATERAL" else Nothing,
        Just (toTextBuilder settings b),
        fmap (toTextBuilder settings) c
      ]
  JoinTableRef a b -> case b of
    Just c -> TextBuilders.renderInParens (renderJoinedTable settings a) <> " " <> toTextBuilder settings c
    Nothing -> renderJoinedTable settings a

-- |
-- Renders a 'PostgresqlSyntax.Ast.JoinedTable.JoinedTable'. This — not
-- "PostgresqlSyntax.Ast.JoinedTable"\'s own 'IsAst' instance — is what
-- actually gets used whenever a joined table is rendered as part of a
-- 'TableRef', since a @table_ref@ and a @joined_table@ are genuinely
-- mutually recursive (a @table_ref@ can be a @joined_table@, and a
-- @joined_table@'s branches each embed two @table_ref@s), and only this
-- module has both in scope non-abstractly at once. Exposed via the
-- 'PostgresqlSyntax.Ast.TableRef.hs-boot' so
-- "PostgresqlSyntax.Ast.JoinedTable" can delegate its own 'IsAst' instance
-- to it, rather than maintaining a second, subtly different copy — see the
-- doc on 'PostgresqlSyntax.Ast.JoinMeth' for why 'JoinMeth'\'s own instance
-- still needs to differ from this one.
renderJoinedTable :: Settings -> JoinedTable -> TextBuilder
renderJoinedTable settings = \case
  InParensJoinedTable a -> TextBuilders.renderInParens (renderJoinedTable settings a)
  MethJoinedTable a b c -> case a of
    CrossJoinMeth -> renderTableRef settings b <> " CROSS JOIN " <> renderTableRef settings c
    QualJoinMeth d e -> renderTableRef settings b <> TextBuilders.suffixMaybe (toTextBuilder settings) d <> " JOIN " <> renderTableRef settings c <> " " <> toTextBuilder settings e
    NaturalJoinMeth d -> renderTableRef settings b <> " NATURAL" <> TextBuilders.suffixMaybe (toTextBuilder settings) d <> " JOIN " <> renderTableRef settings c

recur :: Settings -> TableRef -> Parser TableRef
recur settings tr =
  asum
    [ do
        tr2 <- Parser.wrapToHead (Parsers.space1 *> trailingTableRef settings tr)
        Parser.endHead
        recur settings tr2,
      pure tr
    ]

nonTrailingTableRef :: Settings -> Parser TableRef
nonTrailingTableRef settings =
  asum
    [lateralTableRef <|> Parser.wrapToHead nonLateralTableRef <|> relationExprTableRef <|> joinedTableWithAliasTableRef <|> inParensJoinedTableTableRef]
  where
    relationExprTableRef = do
      relationExpr <- parser settings
      Parser.endHead
      optAliasClause <- optional (Parsers.space1 *> parser settings)
      optTablesampleClause <- optional (Parsers.space1 *> parser settings)
      return (RelationExprTableRef relationExpr optAliasClause optTablesampleClause)
    lateralTableRef = do
      Parsers.keyword "lateral"
      Parsers.space1
      Parser.endHead
      lateralableTableRef True
    nonLateralTableRef = lateralableTableRef False
    lateralableTableRef lateral =
      asum
        [ do
            a <- parser settings
            b <- optional (Parsers.space1 *> parser settings)
            return (FuncTableRef lateral a b),
          do
            select <- parser settings
            optAliasClause <- optional $ Parsers.space1 *> parser settings
            return (SelectTableRef lateral select optAliasClause)
        ]
    inParensJoinedTableTableRef = JoinTableRef <$> inParensJoinedTable settings <*> pure Nothing
    joinedTableWithAliasTableRef = do
      jt <- Parser.wrapToHead (Parsers.inParens (joinedTableParser settings))
      Parsers.space1
      alias <- parser settings
      return (JoinTableRef jt (Just alias))

trailingTableRef :: Settings -> TableRef -> Parser TableRef
trailingTableRef settings tableRef =
  JoinTableRef <$> trailingJoinedTable settings tableRef <*> pure Nothing

-- |
-- Parses a 'PostgresqlSyntax.Ast.JoinedTable.JoinedTable'. See
-- 'renderJoinedTable' for why this — not
-- "PostgresqlSyntax.Ast.JoinedTable"\'s own 'IsAst' instance — is what
-- actually gets used to parse a joined table wherever one can occur inside a
-- 'TableRef'.
joinedTableParser :: Settings -> Parser JoinedTable
joinedTableParser settings =
  headP >>= tailP
  where
    headP =
      asum
        [ do
            tr <- Parser.wrapToHead (nonTrailingTableRef settings)
            Parsers.space1
            trailingJoinedTable settings tr,
          inParensJoinedTable settings
        ]
    tailP jt =
      asum
        [ do
            jt2 <- Parser.wrapToHead (Parsers.space1 *> trailingJoinedTable settings (JoinTableRef jt Nothing))
            Parser.endHead
            tailP jt2,
          pure jt
        ]

-- ==== References
-- @
--   | '(' joined_table ')'
-- @
inParensJoinedTable :: Settings -> Parser JoinedTable
inParensJoinedTable settings = InParensJoinedTable <$> Parsers.inParens (joinedTableParser settings)

-- ==== References
-- @
--   | table_ref CROSS JOIN table_ref
--   | table_ref join_type JOIN table_ref join_qual
--   | table_ref JOIN table_ref join_qual
--   | table_ref NATURAL join_type JOIN table_ref
--   | table_ref NATURAL JOIN table_ref
-- @
trailingJoinedTable :: Settings -> TableRef -> Parser JoinedTable
trailingJoinedTable settings tr1 =
  asum
    [ do
        Parsers.keyphrase "cross join"
        Parser.endHead
        Parsers.space1
        tr2 <- nonTrailingTableRef settings
        return (MethJoinedTable CrossJoinMeth tr1 tr2),
      do
        jt <- joinTypedJoin
        Parser.endHead
        Parsers.space1
        tr2 <- parser settings
        Parsers.space1
        jq <- parser settings
        return (MethJoinedTable (QualJoinMeth jt jq) tr1 tr2),
      do
        Parsers.keyword "natural"
        Parser.endHead
        Parsers.space1
        jt <- joinTypedJoin
        Parsers.space1
        tr2 <- nonTrailingTableRef settings
        return (MethJoinedTable (NaturalJoinMeth jt) tr1 tr2)
    ]
  where
    joinTypedJoin =
      Just
        <$> (parser settings <* Parser.endHead <* Parsers.space1 <* Parsers.keyword "join")
          <|> Nothing
        <$ Parsers.keyword "join"

instance Qc.Arbitrary TableRef where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.sized $ \n ->
      if n <= 1
        then RelationExprTableRef <$> Qc.arbitrary <*> pure Nothing <*> pure Nothing
        else
          Qc.oneof
            [ RelationExprTableRef <$> Qc.arbitrary <*> Gens.terminatingMaybe Qc.arbitrary <*> Gens.terminatingMaybe Qc.arbitrary,
              FuncTableRef <$> Qc.arbitrary <*> Qc.arbitrary <*> Gens.terminatingMaybe Qc.arbitrary,
              SelectTableRef <$> Qc.arbitrary <*> Gens.downscale Qc.arbitrary <*> Gens.terminatingMaybe Qc.arbitrary,
              JoinTableRef <$> Qc.arbitrary <*> Gens.terminatingMaybe Qc.arbitrary
            ]

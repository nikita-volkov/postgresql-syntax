module PostgresqlSyntax.Ast.TableRef
  ( TableRef (..),
  )
where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Algebra
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
  toTextBuilder settings = \case
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
      Just c -> TextBuilders.renderInParens (toTextBuilder settings a) <> " " <> toTextBuilder settings c
      Nothing -> toTextBuilder settings a

  parser settings = Parser.label "table reference" (parseLeftRecursive settings)

-- |
-- 'PostgresqlSyntax.Ast.JoinedTable' embeds trivially into a bare,
-- alias-less 'TableRef' (@joined_table@ is one of @table_ref@'s
-- alternatives), and a 'TableRef' of that exact shape is recognizable back
-- as one. See 'PostgresqlSyntax.Algebra.LeftRecursion' for how this is
-- used to fold a chain of joins onto a leading 'TableRef'.
instance Refines JoinedTable TableRef where
  embed a = JoinTableRef a Nothing
  project = \case
    JoinTableRef a Nothing -> Just a
    _ -> Nothing

-- |
-- The left-recursion-eliminated form of @table_ref@\/@joined_table@: a
-- 'TableRef' is the non-recursive base (@β@, 'nonTrailingTableRef'), and a
-- @joined_table@ continuation (@α@) is a 'JoinMeth' plus its right
-- operand, applied via 'MethJoinedTable'. All three join kinds sit at the
-- same precedence (@%left JOIN CROSS LEFT FULL RIGHT INNER_P NATURAL@ in
-- @gram.y@), so the default left fold is exactly right — unlike
-- "PostgresqlSyntax.Ast.SimpleSelect", this hub doesn't override
-- 'PostgresqlSyntax.Algebra.foldExtensions'.
instance LeftRecursion TableRef JoinedTable (JoinMeth, TableRef) where
  nonRecursiveBase = nonTrailingTableRef

  -- ==== References
  -- @
  --   | table_ref CROSS JOIN table_ref
  --   | table_ref join_type JOIN table_ref join_qual
  --   | table_ref JOIN table_ref join_qual
  --   | table_ref NATURAL join_type JOIN table_ref
  --   | table_ref NATURAL JOIN table_ref
  -- @
  extension settings =
    Parsers.space1
      *> asum
        [ do
            Parsers.keyphrase "cross join"
            Parser.endHead
            Parsers.space1
            tr2 <- nonTrailingTableRef settings
            return (CrossJoinMeth, tr2),
          do
            jt <- joinTypedJoin
            Parser.endHead
            Parsers.space1
            tr2 <- parser settings
            Parsers.space1
            jq <- parser settings
            return (QualJoinMeth jt jq, tr2),
          do
            Parsers.keyword "natural"
            Parser.endHead
            Parsers.space1
            jt <- joinTypedJoin
            Parsers.space1
            tr2 <- nonTrailingTableRef settings
            return (NaturalJoinMeth jt, tr2)
        ]
    where
      joinTypedJoin =
        Just
          <$> (parser settings <* Parser.endHead <* Parsers.space1 <* Parsers.keyword "join")
            <|> Nothing
          <$ Parsers.keyword "join"

  applyExtension tr1 (joinMeth, tr2) = MethJoinedTable joinMeth tr1 tr2

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
      jt <- Parser.wrapToHead (Parsers.inParens (parser settings))
      Parsers.space1
      alias <- parser settings
      return (JoinTableRef jt (Just alias))

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

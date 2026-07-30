module PostgresqlSyntax.Ast.SimpleSelect
  ( SimpleSelect (..),
    baseSimpleSelect,
    selectClauseBase,
    extendSelectClause,
  )
where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.FromClause
import PostgresqlSyntax.Ast.GroupClause
import PostgresqlSyntax.Ast.HavingClause
import PostgresqlSyntax.Ast.IntoClause
import PostgresqlSyntax.Ast.RelationExpr
import PostgresqlSyntax.Ast.SelectBinOp
import PostgresqlSyntax.Ast.SelectClause
import PostgresqlSyntax.Ast.Targeting
import PostgresqlSyntax.Ast.ValuesClause
import PostgresqlSyntax.Ast.WhereClause
import PostgresqlSyntax.Ast.WindowClause
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import PostgresqlSyntax.Settings (Settings)
import qualified Test.QuickCheck as Qc
import qualified Text.Megaparsec as Megaparsec

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
  = NormalSimpleSelect (Maybe Targeting) (Maybe IntoClause) (Maybe FromClause) (Maybe WhereClause) (Maybe GroupClause) (Maybe HavingClause) (Maybe WindowClause)
  | ValuesSimpleSelect ValuesClause
  | TableSimpleSelect RelationExpr
  | BinSimpleSelect SelectBinOp SelectClause (Maybe Bool) SelectClause
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst SimpleSelect where
  toTextBuilder settings = \case
    NormalSimpleSelect a b c d e f g ->
      TextBuilders.optLexemes
        [ Just "SELECT",
          fmap (toTextBuilder settings) a,
          fmap (toTextBuilder settings) b,
          fmap (toTextBuilder settings) c,
          fmap (toTextBuilder settings) d,
          fmap (toTextBuilder settings) e,
          fmap (toTextBuilder settings) f,
          fmap (toTextBuilder settings) g
        ]
    ValuesSimpleSelect a -> toTextBuilder settings a
    TableSimpleSelect a -> "TABLE " <> toTextBuilder settings a
    BinSimpleSelect a b c d -> toTextBuilder settings b <> " " <> toTextBuilder settings a <> foldMap (mappend " " . TextBuilders.renderAllOrDistinct) c <> " " <> toTextBuilder settings d
  parser settings = do
    a <- baseSimpleSelect settings <|> Parser.parse (Megaparsec.try (Parser.toParsec withParensHead))
    extendMany suffix a
    where
      suffix headSimpleSelect = binopExtension (SimpleSelectSelectClause headSimpleSelect)
      withParensHead = do
        swp <- parser settings
        binopExtension (WithParensSelectClause swp)
      binopExtension headClause = do
        op <- Parsers.space1 *> parser settings <* Parsers.space1
        Parser.endHead
        distinct <- optional (Parsers.allOrDistinct <* Parsers.space1)
        rhs <- selectClauseBase settings >>= extendSelectClause settings
        return (BinSimpleSelect op headClause distinct rhs)

-- |
-- The non-recursive base cases only (no @select_clause BINOP
-- select_clause@ extension) — see this module's own boot-exposed
-- signature.
baseSimpleSelect :: Settings -> Parser SimpleSelect
baseSimpleSelect settings =
  asum
    [ do
        Parsers.keyword "select"
        Parsers.notFollowedBy $ Parsers.satisfy isAlphaNum
        Parser.endHead
        targeting <- optional (Parsers.space1 *> parser settings)
        intoClause <- optional (Parsers.space1 *> parser settings)
        fromClause <- optional (Parsers.space1 *> parser settings)
        whereClause <- optional (Parsers.space1 *> parser settings)
        groupClause <- optional (Parsers.space1 *> parser settings)
        havingClause <- optional (Parsers.space1 *> parser settings)
        windowClause <- optional (Parsers.space1 *> parser settings)
        return (NormalSimpleSelect targeting intoClause fromClause whereClause groupClause havingClause windowClause),
      do
        Parsers.keyword "table"
        Parsers.space1
        Parser.endHead
        TableSimpleSelect <$> parser settings,
      ValuesSimpleSelect <$> parser settings
    ]

selectClauseBase :: Settings -> Parser SelectClause
selectClauseBase settings =
  asum
    [ WithParensSelectClause <$> parser settings,
      SimpleSelectSelectClause <$> baseSimpleSelect settings
    ]

extendSelectClause :: Settings -> SelectClause -> Parser SelectClause
extendSelectClause settings = extendMany suffix
  where
    suffix headSelectClause = SimpleSelectSelectClause <$> extensionSimpleSelect headSelectClause
    extensionSimpleSelect headSelectClause = do
      op <- Parsers.space1 *> parser settings <* Parsers.space1
      Parser.endHead
      distinct <- optional (Parsers.allOrDistinct <* Parsers.space1)
      rhs <- selectClauseBase settings >>= extendSelectClause settings
      return (BinSimpleSelect op headSelectClause distinct rhs)

instance Qc.Arbitrary SimpleSelect where
  shrink = fmap canonicalize . Qc.genericShrink
  arbitrary =
    canonicalize
      <$> Qc.sized
        ( \n ->
            if n <= 1
              then TableSimpleSelect <$> Qc.arbitrary
              else
                Qc.oneof
                  [ NormalSimpleSelect
                      <$> Qc.arbitrary
                      <*> Qc.arbitrary
                      <*> Qc.arbitrary
                      <*> Gens.terminatingMaybe Qc.arbitrary
                      <*> Qc.arbitrary
                      <*> Gens.terminatingMaybe Qc.arbitrary
                      <*> Qc.arbitrary,
                    ValuesSimpleSelect <$> Qc.arbitrary,
                    TableSimpleSelect <$> Qc.arbitrary,
                    BinSimpleSelect <$> Qc.arbitrary <*> Qc.arbitrary <*> Qc.arbitrary <*> Qc.arbitrary
                  ]
        )

-- |
-- Collapses a left-associated @BinSimpleSelect@ chain (@(a OP1 b) OP2
-- c@) to the right-associated shape (@a OP1 (b OP2 c)@) that the parser
-- actually produces: 'parser' above parses each operator's right-hand
-- side via 'extendSelectClause', which itself greedily consumes the rest
-- of the chain before returning — so a chain of @N@ operators nests
-- entirely to the right, and only that shape is reachable by parsing the
-- rendered text (both shapes render identically, since rendering doesn't
-- parenthesize chain elements). Both 'arbitrary' and 'shrink' can
-- otherwise construct the non-canonical shape, which renders fine but
-- parses back to a different, canonical value and so breaks the
-- roundtrip property.
canonicalize :: SimpleSelect -> SimpleSelect
canonicalize s@(BinSimpleSelect {}) =
  case rest of
    (op, distinct, next) : more -> BinSimpleSelect op headClause distinct (buildRight next more)
    [] -> s
  where
    (headClause, rest) = flattenChain (SimpleSelectSelectClause s)
    buildRight lastClause [] = lastClause
    buildRight clause ((op, distinct, next) : more) = SimpleSelectSelectClause (BinSimpleSelect op clause distinct (buildRight next more))
    flattenChain (SimpleSelectSelectClause (BinSimpleSelect op lhs distinct rhs)) =
      let (lhsHead, lhsRest) = flattenChain lhs
          (rhsHead, rhsRest) = flattenChain rhs
       in (lhsHead, lhsRest <> [(op, distinct, rhsHead)] <> rhsRest)
    flattenChain c = (c, [])
canonicalize other = other

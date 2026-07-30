module PostgresqlSyntax.Ast.LimitClause where

import qualified HeadedMegaparsec as Parser
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.SelectFetchFirstValue
import PostgresqlSyntax.Ast.SelectLimitValue
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- limit_clause:
--   | LIMIT select_limit_value
--   | LIMIT select_limit_value ',' select_offset_value
--   | FETCH first_or_next select_fetch_first_value row_or_rows ONLY
--   | FETCH first_or_next row_or_rows ONLY
-- select_offset_value:
--   | a_expr
-- first_or_next:
--   | FIRST_P
--   | NEXT
-- row_or_rows:
--   | ROW
--   | ROWS
-- @
data LimitClause
  = LimitLimitClause SelectLimitValue (Maybe AExpr)
  | FetchOnlyLimitClause Bool (Maybe SelectFetchFirstValue) Bool
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst LimitClause where
  toTextBuilder settings = \case
    LimitLimitClause a b -> "LIMIT " <> toTextBuilder settings a <> foldMap (mappend ", " . toTextBuilder settings) b
    FetchOnlyLimitClause a b c ->
      TextBuilders.optLexemes
        [ Just "FETCH",
          Just (firstOrNext a),
          fmap (toTextBuilder settings) b,
          Just (rowOrRows c),
          Just "ONLY"
        ]
    where
      firstOrNext = bool "FIRST" "NEXT"
      rowOrRows = bool "ROW" "ROWS"
  parser settings =
    ( do
        Parsers.keyword "limit"
        Parser.endHead
        Parsers.space1
        a <- parser settings
        b <- optional $ do
          Parsers.commaSeparator
          parser settings
        return (LimitLimitClause a b)
    )
      <|> ( do
              Parsers.keyword "fetch"
              Parser.endHead
              Parsers.space1
              a <- firstOrNext
              Parsers.space1
              asum
                [ do
                    b <- rowOrRows
                    Parsers.space1
                    Parsers.keyword "only"
                    return (FetchOnlyLimitClause a Nothing b),
                  do
                    b <- parser settings
                    Parsers.space1
                    c <- rowOrRows
                    Parsers.space1
                    Parsers.keyword "only"
                    return (FetchOnlyLimitClause a (Just b) c)
                ]
          )
    where
      firstOrNext =
        False
          <$ Parsers.keyword "first"
            <|> True
          <$ Parsers.keyword "next"
      rowOrRows =
        True
          <$ Parsers.keyword "rows"
            <|> False
          <$ Parsers.keyword "row"

instance Qc.Arbitrary LimitClause where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ LimitLimitClause <$> Qc.arbitrary <*> Gens.terminatingMaybe (Gens.downscale Qc.arbitrary),
        FetchOnlyLimitClause <$> Qc.arbitrary <*> Gens.terminatingMaybe Qc.arbitrary <*> Qc.arbitrary
      ]

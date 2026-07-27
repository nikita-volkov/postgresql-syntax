module PostgresqlSyntax.Ast.LimitClause where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.SelectFetchFirstValue
import PostgresqlSyntax.Ast.SelectLimitValue
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
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
  toTextBuilder = \case
    LimitLimitClause a b -> "LIMIT " <> toTextBuilder a <> foldMap (mappend ", " . toTextBuilder) b
    FetchOnlyLimitClause a b c ->
      optLexemes
        [ Just "FETCH",
          Just (firstOrNext a),
          fmap toTextBuilder b,
          Just (rowOrRows c),
          Just "ONLY"
        ]
    where
      firstOrNext = bool "FIRST" "NEXT"
      rowOrRows = bool "ROW" "ROWS"
  parser =
    ( do
        keyword "limit"
        Parser.endHead
        Parser.space1
        a <- parser
        b <- optional $ do
          commaSeparator
          parser
        return (LimitLimitClause a b)
    )
      <|> ( do
              keyword "fetch"
              Parser.endHead
              Parser.space1
              a <- firstOrNext
              Parser.space1
              asum
                [ do
                    b <- rowOrRows
                    Parser.space1
                    keyword "only"
                    return (FetchOnlyLimitClause a Nothing b),
                  do
                    b <- parser
                    Parser.space1
                    c <- rowOrRows
                    Parser.space1
                    keyword "only"
                    return (FetchOnlyLimitClause a (Just b) c)
                ]
          )
    where
      firstOrNext =
        False <$ keyword "first"
          <|> True <$ keyword "next"
      rowOrRows =
        True <$ keyword "rows"
          <|> False <$ keyword "row"

instance Qc.Arbitrary LimitClause where
  arbitrary =
    Qc.oneof
      [ LimitLimitClause <$> Qc.arbitrary <*> Qc.scale (`div` 2) Qc.arbitrary,
        FetchOnlyLimitClause <$> Qc.arbitrary <*> Qc.scale (`div` 2) Qc.arbitrary <*> Qc.arbitrary
      ]

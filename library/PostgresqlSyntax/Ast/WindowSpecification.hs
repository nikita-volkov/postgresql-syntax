module PostgresqlSyntax.Ast.WindowSpecification where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.ExprList
import PostgresqlSyntax.Ast.FrameClause
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.SortClause
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import PostgresqlSyntax.Helpers.Parsers
import PostgresqlSyntax.Helpers.TextBuilders
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- window_specification:
--   |  '(' opt_existing_window_name opt_partition_clause
--             opt_sort_clause opt_frame_clause ')'
--
-- opt_existing_window_name:
--   |  ColId
--   |  EMPTY
--
-- opt_partition_clause:
--   |  PARTITION BY expr_list
--   |  EMPTY
-- @
--
-- @existing_window_name@ and @partition_clause@ are bare aliases to
-- 'PostgresqlSyntax.Ast.Ident' (ColId) and 'PostgresqlSyntax.Ast.ExprList'
-- respectively.
data WindowSpecification = WindowSpecification (Maybe Ident) (Maybe ExprList) (Maybe SortClause) (Maybe FrameClause)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst WindowSpecification where
  toTextBuilder (WindowSpecification a b c d) =
    renderInParens $
      optLexemes
        [ fmap toTextBuilder a,
          fmap (mappend "PARTITION BY " . toTextBuilder) b,
          fmap toTextBuilder c,
          fmap toTextBuilder d
        ]
  parser =
    inParens $
      asum
        [ do
            a <- parser
            return (WindowSpecification Nothing Nothing Nothing (Just a)),
          do
            a <- parser
            b <- optional (Parser.space1 *> parser)
            return (WindowSpecification Nothing Nothing (Just a) b),
          do
            a <- partitionByClause
            b <- optional (Parser.space1 *> parser)
            c <- optional (Parser.space1 *> parser)
            return (WindowSpecification Nothing (Just a) b c),
          do
            a <- colId
            b <- optional (Parser.space1 *> partitionByClause)
            c <- optional (Parser.space1 *> parser)
            d <- optional (Parser.space1 *> parser)
            return (WindowSpecification (Just a) b c d),
          pure (WindowSpecification Nothing Nothing Nothing Nothing)
        ]
    where
      partitionByClause = keyphrase "partition by" *> Parser.space1 *> Parser.endHead *> (ExprList <$> Parser.sep1 commaSeparator parser)

instance Qc.Arbitrary WindowSpecification where
  shrink = Qc.genericShrink
  arbitrary =
    WindowSpecification
      <$> Qc.arbitrary
      <*> Qc.terminatingMaybe Qc.arbitrary
      <*> Qc.terminatingMaybe Qc.arbitrary
      <*> Qc.terminatingMaybe Qc.arbitrary

module PostgresqlSyntax.Ast.WindowSpecification where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.ExprList
import PostgresqlSyntax.Ast.FrameClause
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.SortClause
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
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
    TextBuilders.renderInParens $
      TextBuilders.optLexemes
        [ fmap toTextBuilder a,
          fmap (mappend "PARTITION BY " . toTextBuilder) b,
          fmap toTextBuilder c,
          fmap toTextBuilder d
        ]
  parser =
    Parsers.inParens $
      asum
        [ do
            a <- parser
            return (WindowSpecification Nothing Nothing Nothing (Just a)),
          do
            a <- parser
            b <- optional (Parsers.space1 *> parser)
            return (WindowSpecification Nothing Nothing (Just a) b),
          do
            a <- partitionByClause
            b <- optional (Parsers.space1 *> parser)
            c <- optional (Parsers.space1 *> parser)
            return (WindowSpecification Nothing (Just a) b c),
          do
            a <- colId
            b <- optional (Parsers.space1 *> partitionByClause)
            c <- optional (Parsers.space1 *> parser)
            d <- optional (Parsers.space1 *> parser)
            return (WindowSpecification (Just a) b c d),
          pure (WindowSpecification Nothing Nothing Nothing Nothing)
        ]
    where
      partitionByClause = Parsers.keyphrase "partition by" *> Parsers.space1 *> Parser.endHead *> (ExprList <$> Parsers.sep1 Parsers.commaSeparator parser)

instance Qc.Arbitrary WindowSpecification where
  shrink = Qc.genericShrink
  arbitrary =
    WindowSpecification
      <$> Qc.arbitrary
      <*> Gens.terminatingMaybe Qc.arbitrary
      <*> Gens.terminatingMaybe Qc.arbitrary
      <*> Gens.terminatingMaybe Qc.arbitrary

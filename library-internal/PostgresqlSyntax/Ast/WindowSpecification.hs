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
  toTextBuilder settings (WindowSpecification a b c d) =
    TextBuilders.renderInParens $
      TextBuilders.optLexemes
        [ fmap (toTextBuilder settings) a,
          fmap (mappend "PARTITION BY " . toTextBuilder settings) b,
          fmap (toTextBuilder settings) c,
          fmap (toTextBuilder settings) d
        ]
  parser settings =
    Parsers.inParens $
      asum
        [ do
            a <- parser settings
            return (WindowSpecification Nothing Nothing Nothing (Just a)),
          do
            a <- parser settings
            b <- optional (Parsers.space1 *> parser settings)
            return (WindowSpecification Nothing Nothing (Just a) b),
          do
            a <- partitionByClause
            b <- optional (Parsers.space1 *> parser settings)
            c <- optional (Parsers.space1 *> parser settings)
            return (WindowSpecification Nothing (Just a) b c),
          do
            a <- colId settings
            b <- optional (Parsers.space1 *> partitionByClause)
            c <- optional (Parsers.space1 *> parser settings)
            d <- optional (Parsers.space1 *> parser settings)
            return (WindowSpecification (Just a) b c d),
          pure (WindowSpecification Nothing Nothing Nothing Nothing)
        ]
    where
      partitionByClause = Parsers.keyphrase "partition by" *> Parsers.space1 *> Parser.endHead *> (ExprList <$> Parsers.sep1 Parsers.commaSeparator (parser settings))

instance Qc.Arbitrary WindowSpecification where
  shrink = Qc.genericShrink
  arbitrary =
    WindowSpecification
      <$> Qc.arbitrary
      <*> Gens.terminatingMaybe Qc.arbitrary
      <*> Gens.terminatingMaybe Qc.arbitrary
      <*> Gens.terminatingMaybe Qc.arbitrary

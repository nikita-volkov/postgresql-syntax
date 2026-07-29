module PostgresqlSyntax.Ast.FuncConstArgs where

import PostgresqlSyntax.Ast.FuncArgExpr
import PostgresqlSyntax.Ast.SortClause
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

-- |
-- The parenthesized-argument-list part of a @func_name '(' func_arg_list
-- opt_sort_clause ')' Sconst@ 'PostgresqlSyntax.Ast.AexprConst' — rendered\/
-- parsed without its enclosing parens, which belong to the caller.
--
-- ==== References
-- @
--   |  func_name '(' func_arg_list opt_sort_clause ')' Sconst
-- @
data FuncConstArgs = FuncConstArgs (NonEmpty FuncArgExpr) (Maybe SortClause)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst FuncConstArgs where
  toTextBuilder settings (FuncConstArgs a b) = TextBuilders.commaNonEmpty (toTextBuilder settings) a <> TextBuilders.suffixMaybe (toTextBuilder settings) b
  parser settings = FuncConstArgs <$> Parsers.sep1 Parsers.commaSeparator (parser settings) <*> optional (Parsers.space1 *> parser settings)

instance Qc.Arbitrary FuncConstArgs where
  shrink = Qc.genericShrink
  arbitrary = FuncConstArgs <$> Gens.nonEmptyUpTo 6 Qc.arbitrary <*> Gens.terminatingMaybe Qc.arbitrary

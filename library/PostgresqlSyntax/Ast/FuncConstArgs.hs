module PostgresqlSyntax.Ast.FuncConstArgs where

import PostgresqlSyntax.Ast.FuncArgExpr
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.SortClause
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
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
  toTextBuilder (FuncConstArgs a b) = commaNonEmpty toTextBuilder a <> suffixMaybe toTextBuilder b
  parser = FuncConstArgs <$> Parser.sep1 commaSeparator parser <*> optional (Parser.space1 *> parser)

instance Qc.Arbitrary FuncConstArgs where
  arbitrary = do
    len <- Qc.choose (0, 6)
    x <- Qc.scale (`div` 2) Qc.arbitrary
    xs <- Qc.vectorOf len (Qc.scale (`div` 2) Qc.arbitrary)
    b <- Qc.scale (`div` 2) Qc.arbitrary
    pure (FuncConstArgs (x :| xs) b)

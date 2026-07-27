module PostgresqlSyntax.Ast.CaseExpr where

import HeadedMegaparsec
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.WhenClauseList
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
-- case_expr:
--   | CASE case_arg when_clause_list case_default END_P
-- @
data CaseExpr = CaseExpr (Maybe AExpr) WhenClauseList (Maybe AExpr)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst CaseExpr where
  toTextBuilder (CaseExpr a b c) =
    optLexemes
      [ Just "CASE",
        fmap toTextBuilder a,
        Just (toTextBuilder b),
        fmap caseDefault c,
        Just "END"
      ]
    where
      caseDefault d = "ELSE " <> toTextBuilder d
  parser = label "case expression" $ do
    keyword "case"
    space1
    endHead
    arg <- optional (parser <* space1)
    whenClauses <- parser
    space1
    default' <- optional elseClause
    keyword "end"
    pure (CaseExpr arg whenClauses default')
    where
      elseClause = do
        keyword "else"
        space1
        endHead
        a <- parser
        space1
        return a

instance Arbitrary CaseExpr where
  arbitrary = CaseExpr <$> scale (`div` 2) arbitrary <*> scale (`div` 2) arbitrary <*> scale (`div` 2) arbitrary

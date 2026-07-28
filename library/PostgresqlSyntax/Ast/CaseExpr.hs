module PostgresqlSyntax.Ast.CaseExpr where

import qualified HeadedMegaparsec as Parser
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.WhenClauseList
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

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
  parser = Parser.label "case expression" $ do
    keyword "case"
    Parser.space1
    Parser.endHead
    arg <- optional (parser <* Parser.space1)
    whenClauses <- parser
    Parser.space1
    default' <- optional elseClause
    keyword "end"
    pure (CaseExpr arg whenClauses default')
    where
      elseClause = do
        keyword "else"
        Parser.space1
        Parser.endHead
        a <- parser
        Parser.space1
        return a

instance Qc.Arbitrary CaseExpr where
  arbitrary = CaseExpr <$> Qc.scale (`div` 2) arbitrary <*> Qc.scale (`div` 2) arbitrary <*> Qc.scale (`div` 2) arbitrary

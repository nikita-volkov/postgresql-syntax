module PostgresqlSyntax.Ast.CaseExpr where

import qualified HeadedMegaparsec as Parser
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.WhenClauseList
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
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
    TextBuilders.optLexemes
      [ Just "CASE",
        fmap toTextBuilder a,
        Just (toTextBuilder b),
        fmap caseDefault c,
        Just "END"
      ]
    where
      caseDefault d = "ELSE " <> toTextBuilder d
  parser = Parser.label "case expression" $ do
    Parsers.keyword "case"
    Parsers.space1
    Parser.endHead
    arg <- optional (parser <* Parsers.space1)
    whenClauses <- parser
    Parsers.space1
    default' <- optional elseClause
    Parsers.keyword "end"
    pure (CaseExpr arg whenClauses default')
    where
      elseClause = do
        Parsers.keyword "else"
        Parsers.space1
        Parser.endHead
        a <- parser
        Parsers.space1
        return a

instance Qc.Arbitrary CaseExpr where
  shrink = Qc.genericShrink
  arbitrary = CaseExpr <$> Qc.terminatingMaybe (Qc.downscale arbitrary) <*> Qc.scale (`div` 2) arbitrary <*> Qc.terminatingMaybe (Qc.downscale arbitrary)

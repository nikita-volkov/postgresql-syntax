module PostgresqlSyntax.Ast.OnConflict where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.ConfExpr
import PostgresqlSyntax.Ast.OnConflictDo
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- opt_on_conflict:
--   | ON CONFLICT opt_conf_expr DO UPDATE SET set_clause_list where_clause
--   | ON CONFLICT opt_conf_expr DO NOTHING
--   | EMPTY
-- @
data OnConflict = OnConflict (Maybe ConfExpr) OnConflictDo
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst OnConflict where
  toTextBuilder (OnConflict a b) = "ON CONFLICT" <> TextBuilders.suffixMaybe toTextBuilder a <> " DO " <> toTextBuilder b
  parser = do
    Parsers.keyword "on"
    Parsers.space1
    Parsers.keyword "conflict"
    Parsers.space1
    Parser.endHead
    a <- optional (parser <* Parsers.space1)
    Parsers.keyword "do"
    Parsers.space1
    b <- parser
    return (OnConflict a b)

instance Qc.Arbitrary OnConflict where
  shrink = Qc.genericShrink
  arbitrary = OnConflict <$> Qc.terminatingMaybe arbitrary <*> arbitrary

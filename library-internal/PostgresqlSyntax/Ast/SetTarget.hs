module PostgresqlSyntax.Ast.SetTarget where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Algebra
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.Indirection
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- set_target:
--   | ColId opt_indirection
-- @
data SetTarget = SetTarget Ident (Maybe Indirection)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst SetTarget where
  toTextBuilder settings (SetTarget a b) = toTextBuilder settings a <> TextBuilders.suffixMaybe (toTextBuilder settings) b
  parser settings = do
    a <- colId settings
    Parser.endHead
    b <- optional (Parsers.space1 *> parser settings)
    return (SetTarget a b)

instance Qc.Arbitrary SetTarget where
  shrink = Qc.genericShrink
  arbitrary = SetTarget <$> arbitrary <*> Gens.terminatingMaybe arbitrary

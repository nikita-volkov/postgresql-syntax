module PostgresqlSyntax.Ast.Columnref where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.Ident (Ident, colId)
import PostgresqlSyntax.Ast.Indirection
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- columnref:
--   | ColId
--   | ColId indirection
-- @
data Columnref = Columnref Ident (Maybe Indirection)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst Columnref where
  toTextBuilder settings (Columnref a b) = toTextBuilder settings a <> foldMap (toTextBuilder settings) b
  parser settings = do
    a <- Parser.wrapToHead (colId settings)
    Parser.endHead
    b <- optional (Parsers.space *> parser settings)
    return (Columnref a b)

instance Qc.Arbitrary Columnref where
  shrink = Qc.genericShrink
  arbitrary = Columnref <$> arbitrary <*> Gens.terminatingMaybe arbitrary

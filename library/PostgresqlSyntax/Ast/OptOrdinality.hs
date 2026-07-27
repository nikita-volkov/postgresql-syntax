module PostgresqlSyntax.Ast.OptOrdinality where

import PostgresqlSyntax.Ast.Internal
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- opt_ordinality:
--   | WITH_LA ORDINALITY
--   | EMPTY
-- @
newtype OptOrdinality = OptOrdinality Bool
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst OptOrdinality where
  toTextBuilder (OptOrdinality a) = if a then "WITH ORDINALITY" else mempty
  parser = OptOrdinality <$> trueIfPresent (keyword "with" *> Parser.space1 *> keyword "ordinality")

instance Qc.Arbitrary OptOrdinality where
  arbitrary = OptOrdinality <$> arbitrary

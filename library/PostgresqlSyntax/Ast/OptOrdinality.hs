module PostgresqlSyntax.Ast.OptOrdinality where

import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)

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
  parser = OptOrdinality <$> trueIfPresent (keyword "with" *> space1 *> keyword "ordinality")

instance Arbitrary OptOrdinality where
  arbitrary = OptOrdinality <$> arbitrary

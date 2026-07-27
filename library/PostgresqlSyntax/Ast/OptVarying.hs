module PostgresqlSyntax.Ast.OptVarying where

import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Extras.HeadedMegaparsec (space1)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude

-- |
-- ==== References
-- @
-- opt_varying:
--   | VARYING
--   | EMPTY
-- @
newtype OptVarying = OptVarying Bool
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst OptVarying where
  toTextBuilder (OptVarying a) = if a then "VARYING" else mempty
  parser = OptVarying <$> (True <$ space1 <* keyword "varying" <|> pure False)

instance Arbitrary OptVarying where
  arbitrary = OptVarying <$> arbitrary

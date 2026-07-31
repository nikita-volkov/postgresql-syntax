module PostgresqlSyntax.Ast.OptVarying where

import PostgresqlSyntax.Algebra
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

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
  toTextBuilder _settings (OptVarying a) = if a then "VARYING" else mempty
  parser _settings = OptVarying <$> (True <$ Parsers.space1 <* Parsers.keyword "varying" <|> pure False)

instance Qc.Arbitrary OptVarying where
  shrink = Qc.genericShrink
  arbitrary = OptVarying <$> arbitrary

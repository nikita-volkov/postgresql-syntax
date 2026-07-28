module PostgresqlSyntax.Ast.OptVarying where

import PostgresqlSyntax.Ast.Internal
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import PostgresqlSyntax.IsAst
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
  toTextBuilder (OptVarying a) = if a then "VARYING" else mempty
  parser = OptVarying <$> (True <$ Parser.space1 <* keyword "varying" <|> pure False)

instance Qc.Arbitrary OptVarying where
  shrink = Qc.genericShrink
  arbitrary = OptVarying <$> arbitrary

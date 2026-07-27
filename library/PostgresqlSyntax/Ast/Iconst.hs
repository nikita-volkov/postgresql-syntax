module PostgresqlSyntax.Ast.Iconst where

import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import qualified PostgresqlSyntax.Extras.TextBuilder as TextBuilder
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- Iconst
-- @
newtype Iconst = Iconst Int64
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst Iconst where
  toTextBuilder (Iconst a) = TextBuilder.int64Dec a
  parser = Iconst <$> Parser.decimal

instance Qc.Arbitrary Iconst where
  arbitrary = Iconst <$> Qc.sized (\n -> Qc.choose (0, cap n))
    where
      cap n
        | n >= 62 = maxBound
        | otherwise = 2 ^ n

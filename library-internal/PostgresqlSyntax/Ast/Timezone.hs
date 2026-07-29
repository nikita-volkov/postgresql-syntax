module PostgresqlSyntax.Ast.Timezone where

import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- opt_timezone:
--   | WITH_LA TIME ZONE
--   | WITHOUT TIME ZONE
--   | EMPTY
-- @
newtype Timezone = Timezone Bool
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst Timezone where
  toTextBuilder (Timezone a) = if a then "WITHOUT TIME ZONE" else "WITH TIME ZONE"
  parser =
    Timezone
      <$> asum
        [ False <$ Parsers.keyphrase "with time zone",
          True <$ Parsers.keyphrase "without time zone"
        ]

instance Qc.Arbitrary Timezone where
  shrink = Qc.genericShrink
  arbitrary = Timezone <$> arbitrary

module PostgresqlSyntax.Ast.Timezone where

import PostgresqlSyntax.Ast.Internal
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
        [ False <$ keyphrase "with time zone",
          True <$ keyphrase "without time zone"
        ]

instance Qc.Arbitrary Timezone where
  shrink = Qc.genericShrink
  arbitrary = Timezone <$> arbitrary

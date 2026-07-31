module PostgresqlSyntax.Ast.TrimModifier where

import PostgresqlSyntax.Algebra
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
--   | TRIM '(' BOTH trim_list ')'
--   | TRIM '(' LEADING trim_list ')'
--   | TRIM '(' TRAILING trim_list ')'
-- @
data TrimModifier = BothTrimModifier | LeadingTrimModifier | TrailingTrimModifier
  deriving (Show, Generic, Eq, Ord, Data, Enum, Bounded)

instance IsAst TrimModifier where
  toTextBuilder _settings = \case
    BothTrimModifier -> "BOTH"
    LeadingTrimModifier -> "LEADING"
    TrailingTrimModifier -> "TRAILING"
  parser _settings =
    BothTrimModifier
      <$ Parsers.keyword "both"
        <|> LeadingTrimModifier
      <$ Parsers.keyword "leading"
        <|> TrailingTrimModifier
      <$ Parsers.keyword "trailing"

instance Qc.Arbitrary TrimModifier where
  shrink = Qc.genericShrink
  arbitrary = Qc.elements [minBound .. maxBound]

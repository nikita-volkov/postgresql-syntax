module PostgresqlSyntax.Ast.TrimModifier where

import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import PostgresqlSyntax.IsAst
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
  toTextBuilder settings = \case
    BothTrimModifier -> "BOTH"
    LeadingTrimModifier -> "LEADING"
    TrailingTrimModifier -> "TRAILING"
  parser settings =
    BothTrimModifier
      <$ Parsers.keyword "both"
        <|> LeadingTrimModifier
      <$ Parsers.keyword "leading"
        <|> TrailingTrimModifier
      <$ Parsers.keyword "trailing"

instance Qc.Arbitrary TrimModifier where
  shrink = Qc.genericShrink
  arbitrary = Qc.elements [minBound .. maxBound]

module PostgresqlSyntax.Ast.TrimModifier where

import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude

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
  toTextBuilder = \case
    BothTrimModifier -> "BOTH"
    LeadingTrimModifier -> "LEADING"
    TrailingTrimModifier -> "TRAILING"
  parser =
    BothTrimModifier <$ keyword "both"
      <|> LeadingTrimModifier <$ keyword "leading"
      <|> TrailingTrimModifier <$ keyword "trailing"

instance Arbitrary TrimModifier where
  arbitrary = elements [minBound .. maxBound]

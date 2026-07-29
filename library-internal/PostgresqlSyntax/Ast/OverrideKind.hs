module PostgresqlSyntax.Ast.OverrideKind where

import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- override_kind:
--   | USER
--   | SYSTEM_P
-- @
data OverrideKind = UserOverrideKind | SystemOverrideKind
  deriving (Show, Generic, Eq, Ord, Data, Enum, Bounded)

instance IsAst OverrideKind where
  toTextBuilder settings = \case
    UserOverrideKind -> "USER"
    SystemOverrideKind -> "SYSTEM"
  parser settings =
    asum
      [ UserOverrideKind <$ Parsers.keyword "user",
        SystemOverrideKind <$ Parsers.keyword "system"
      ]

instance Qc.Arbitrary OverrideKind where
  shrink = Qc.genericShrink
  arbitrary = Qc.elements [minBound .. maxBound]

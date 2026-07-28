module PostgresqlSyntax.Ast.OverrideKind where

import PostgresqlSyntax.Ast.Internal
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
  toTextBuilder = \case
    UserOverrideKind -> "USER"
    SystemOverrideKind -> "SYSTEM"
  parser =
    asum
      [ UserOverrideKind <$ keyword "user",
        SystemOverrideKind <$ keyword "system"
      ]

instance Qc.Arbitrary OverrideKind where
  shrink = Qc.genericShrink
  arbitrary = Qc.elements [minBound .. maxBound]

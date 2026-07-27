module PostgresqlSyntax.Ast.ForLockingStrength where

import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- for_locking_strength:
--   | FOR UPDATE
--   | FOR NO KEY UPDATE
--   | FOR SHARE
--   | FOR KEY SHARE
-- @
data ForLockingStrength
  = UpdateForLockingStrength
  | NoKeyUpdateForLockingStrength
  | ShareForLockingStrength
  | KeyForLockingStrength
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst ForLockingStrength where
  toTextBuilder = \case
    UpdateForLockingStrength -> "FOR UPDATE"
    NoKeyUpdateForLockingStrength -> "FOR NO KEY UPDATE"
    ShareForLockingStrength -> "FOR SHARE"
    KeyForLockingStrength -> "FOR KEY SHARE"
  parser =
    UpdateForLockingStrength <$ keyphrase "for update"
      <|> NoKeyUpdateForLockingStrength <$ keyphrase "for no key update"
      <|> ShareForLockingStrength <$ keyphrase "for share"
      <|> KeyForLockingStrength <$ keyphrase "for key share"

instance Qc.Arbitrary ForLockingStrength where
  arbitrary =
    Qc.elements
      [ UpdateForLockingStrength,
        NoKeyUpdateForLockingStrength,
        ShareForLockingStrength,
        KeyForLockingStrength
      ]

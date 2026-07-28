module PostgresqlSyntax.Ast.ForLockingStrength where

import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
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
    UpdateForLockingStrength
      <$ Parsers.keyphrase "for update"
        <|> NoKeyUpdateForLockingStrength
      <$ Parsers.keyphrase "for no key update"
        <|> ShareForLockingStrength
      <$ Parsers.keyphrase "for share"
        <|> KeyForLockingStrength
      <$ Parsers.keyphrase "for key share"

instance Qc.Arbitrary ForLockingStrength where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.elements
      [ UpdateForLockingStrength,
        NoKeyUpdateForLockingStrength,
        ShareForLockingStrength,
        KeyForLockingStrength
      ]

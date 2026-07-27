module PostgresqlSyntax.Ast.Character where

import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.OptVarying
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude

-- |
-- ==== References
-- @
-- character:
--   | CHARACTER opt_varying
--   | CHAR_P opt_varying
--   | VARCHAR
--   | NATIONAL CHARACTER opt_varying
--   | NATIONAL CHAR_P opt_varying
--   | NCHAR opt_varying
-- @
data Character
  = CharacterCharacter OptVarying
  | CharCharacter OptVarying
  | VarcharCharacter
  | NationalCharacterCharacter OptVarying
  | NationalCharCharacter OptVarying
  | NcharCharacter OptVarying
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst Character where
  toTextBuilder = \case
    CharacterCharacter a -> "CHARACTER" <> bool "" " VARYING" (coerce a :: Bool)
    CharCharacter a -> "CHAR" <> bool "" " VARYING" (coerce a :: Bool)
    VarcharCharacter -> "VARCHAR"
    NationalCharacterCharacter a -> "NATIONAL CHARACTER" <> bool "" " VARYING" (coerce a :: Bool)
    NationalCharCharacter a -> "NATIONAL CHAR" <> bool "" " VARYING" (coerce a :: Bool)
    NcharCharacter a -> "NCHAR" <> bool "" " VARYING" (coerce a :: Bool)
  parser =
    asum
      [ CharacterCharacter <$> (keyword "character" *> parser),
        CharCharacter <$> (keyword "char" *> parser),
        VarcharCharacter <$ keyword "varchar",
        NationalCharacterCharacter <$> (keyphrase "national character" *> parser),
        NationalCharCharacter <$> (keyphrase "national char" *> parser),
        NcharCharacter <$> (keyword "nchar" *> parser)
      ]

instance Arbitrary Character where
  arbitrary =
    oneof
      [ CharacterCharacter <$> arbitrary,
        CharCharacter <$> arbitrary,
        pure VarcharCharacter,
        NationalCharacterCharacter <$> arbitrary,
        NationalCharCharacter <$> arbitrary,
        NcharCharacter <$> arbitrary
      ]

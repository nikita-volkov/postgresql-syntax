module PostgresqlSyntax.Ast.Character where

import PostgresqlSyntax.Ast.OptVarying
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

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
      [ CharacterCharacter <$> (Parsers.keyword "character" *> parser),
        CharCharacter <$> (Parsers.keyword "char" *> parser),
        VarcharCharacter <$ Parsers.keyword "varchar",
        NationalCharacterCharacter <$> (Parsers.keyphrase "national character" *> parser),
        NationalCharCharacter <$> (Parsers.keyphrase "national char" *> parser),
        NcharCharacter <$> (Parsers.keyword "nchar" *> parser)
      ]

instance Qc.Arbitrary Character where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ CharacterCharacter <$> Qc.arbitrary,
        CharCharacter <$> Qc.arbitrary,
        pure VarcharCharacter,
        NationalCharacterCharacter <$> Qc.arbitrary,
        NationalCharCharacter <$> Qc.arbitrary,
        NcharCharacter <$> Qc.arbitrary
      ]

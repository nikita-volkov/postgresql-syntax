module PostgresqlSyntax.Ast.Op where

import qualified Data.Text as Text
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.Shrinks as Shrinks
import PostgresqlSyntax.IsAst
import qualified PostgresqlSyntax.Predicate as Predicate
import PostgresqlSyntax.Prelude
import qualified PostgresqlSyntax.Validation as Validation
import qualified Test.QuickCheck as Qc
import qualified TextBuilder

-- |
-- ==== References
-- @
-- Op
-- @
newtype Op = Op Text
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst Op where
  toTextBuilder _settings (Op a) = TextBuilder.text a
  parser _settings = do
    a <- Parsers.takeWhile1P Nothing Predicate.opChar
    case Validation.op a of
      Nothing -> return (Op a)
      Just err -> fail (Text.unpack err)

instance Qc.Arbitrary Op where
  shrink (Op a) = Op <$> filter (isNothing . Validation.op) (Shrinks.nonEmptyText a)
  arbitrary = Op <$> genOpText `Qc.suchThat` (isNothing . Validation.op)
    where
      genOpText = do
        len <- Qc.choose (1, 7)
        Text.pack <$> Qc.vectorOf len (Qc.elements opChars)
      opChars = "+-*/<>=~!@#%^&|`?"

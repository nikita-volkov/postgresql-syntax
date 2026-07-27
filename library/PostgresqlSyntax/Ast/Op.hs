module PostgresqlSyntax.Ast.Op where

import qualified Data.Text as Text
import PostgresqlSyntax.Extras.HeadedMegaparsec (takeWhile1P)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified PostgresqlSyntax.Predicate as Predicate
import qualified PostgresqlSyntax.Validation as Validation
import Test.QuickCheck (suchThat)
import TextBuilder (text)

-- |
-- ==== References
-- @
-- Op
-- @
newtype Op = Op Text
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst Op where
  toTextBuilder (Op a) = text a
  parser = do
    a <- takeWhile1P Nothing Predicate.opChar
    case Validation.op a of
      Nothing -> return (Op a)
      Just err -> fail (Text.unpack err)

instance Arbitrary Op where
  arbitrary = Op <$> genOpText `suchThat` (isNothing . Validation.op)
    where
      genOpText = do
        len <- choose (1, 7)
        Text.pack <$> vectorOf len (elements opChars)
      opChars = "+-*/<>=~!@#%^&|`?"

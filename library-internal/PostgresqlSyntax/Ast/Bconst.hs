module PostgresqlSyntax.Ast.Bconst where

import qualified Data.Text as Text
import qualified HeadedMegaparsec as Parser
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.Shrinks as Shrinks
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc
import qualified TextBuilder

-- |
-- ==== References
-- @
-- BCONST
-- @
newtype Bconst = Bconst Text
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst Bconst where
  toTextBuilder settings (Bconst a) = "B'" <> TextBuilder.text a <> "'"
  parser settings = Parser.label "bit literal" $ do
    Parsers.string' "b'"
    Parser.endHead
    a <- Parsers.takeWhile1P (Just "0 or 1") (\b -> b == '0' || b == '1')
    Parsers.char '\''
    return (Bconst a)

instance Qc.Arbitrary Bconst where
  shrink (Bconst a) = Bconst <$> Shrinks.nonEmptyText a
  arbitrary = do
    len <- Qc.choose (1, 100)
    Bconst . Text.pack <$> Qc.vectorOf len (Qc.elements "01")

module PostgresqlSyntax.Ast.Xconst where

import qualified Data.Text as Text
import qualified HeadedMegaparsec as Parser
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.Shrinks as Shrinks
import PostgresqlSyntax.IsAst
import qualified PostgresqlSyntax.Predicate as Predicate
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc
import qualified TextBuilder

-- |
-- ==== References
-- @
-- XCONST
-- @
newtype Xconst = Xconst Text
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst Xconst where
  toTextBuilder (Xconst a) = "X'" <> TextBuilder.text a <> "'"
  parser = Parser.label "hex literal" $ do
    Parsers.string' "x'"
    Parser.endHead
    a <- Parsers.takeWhile1P (Just "Hex digit") Predicate.hexDigit
    Parsers.char '\''
    return (Xconst a)

instance Qc.Arbitrary Xconst where
  shrink (Xconst a) = Xconst <$> Shrinks.nonEmptyText a
  arbitrary = do
    len <- Qc.choose (1, 100)
    Xconst . Text.pack <$> Qc.vectorOf len (Qc.elements "0123456789abcdefABCDEF")

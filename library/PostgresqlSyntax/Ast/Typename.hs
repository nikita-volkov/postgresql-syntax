module PostgresqlSyntax.Ast.Typename where

import Control.Applicative.Combinators (option)
import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.SimpleTypename
import PostgresqlSyntax.Ast.TypenameArrayDimensions
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import PostgresqlSyntax.Helpers.Parsers
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

-- |
-- Typename definition extended with custom question-marks for nullability specification.
--
-- To match the standard Postgres syntax simply interpret their presence as a parsing error.
--
-- ==== References
-- @
-- Typename:
--   | SimpleTypename opt_array_bounds
--   | SETOF SimpleTypename opt_array_bounds
--   | SimpleTypename ARRAY '[' Iconst ']'
--   | SETOF SimpleTypename ARRAY '[' Iconst ']'
--   | SimpleTypename ARRAY
--   | SETOF SimpleTypename ARRAY
-- @
data Typename
  = Typename
      -- | SETOF
      Bool
      SimpleTypename
      -- | Question mark
      Bool
      -- | Array dimensions possibly followed by a question mark
      (Maybe (TypenameArrayDimensions, Bool))
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst Typename where
  toTextBuilder (Typename a b _ d) =
    bool "" "SETOF " a <> toTextBuilder b <> foldMap (toTextBuilder . fst) d
  parser = do
    a <- option False (keyword "setof" *> Parser.space1 $> True)
    b <- parser
    Parser.endHead
    c <- trueIfPresent (Parser.space *> Parser.char '?')
    asum
      [ do
          d <- parser
          e <- trueIfPresent (Parser.space *> Parser.char '?')
          return (Typename a b c (Just (d, e))),
        return (Typename a b c Nothing)
      ]

instance Qc.Arbitrary Typename where
  shrink = Qc.genericShrink
  arbitrary = Typename <$> arbitrary <*> arbitrary <*> pure False <*> ((\a -> (,) a False) <$$> arbitrary)
    where
      (<$$>) = fmap . fmap

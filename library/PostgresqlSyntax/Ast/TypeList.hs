module PostgresqlSyntax.Ast.TypeList where

import PostgresqlSyntax.Ast.Typename
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
import PostgresqlSyntax.Helpers.Parsers
import PostgresqlSyntax.Helpers.TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- type_list:
--   | Typename
--   | type_list ',' Typename
-- @
newtype TypeList = TypeList (NonEmpty Typename)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst TypeList where
  toTextBuilder (TypeList a) = commaNonEmpty toTextBuilder a
  parser = TypeList <$> Parser.sep1 commaSeparator parser

instance Qc.Arbitrary TypeList where
  shrink = Qc.genericShrink
  arbitrary = TypeList <$> Qc.nonEmptyUpTo 6 Qc.arbitrary

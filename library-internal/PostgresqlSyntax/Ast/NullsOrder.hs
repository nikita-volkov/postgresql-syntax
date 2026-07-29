module PostgresqlSyntax.Ast.NullsOrder where

import qualified HeadedMegaparsec as Parser
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- opt_nulls_order:
--   | NULLS_LA FIRST_P
--   | NULLS_LA LAST_P
--   | EMPTY
-- @
data NullsOrder = FirstNullsOrder | LastNullsOrder
  deriving (Show, Generic, Eq, Ord, Data, Enum, Bounded)

instance IsAst NullsOrder where
  toTextBuilder settings = \case
    FirstNullsOrder -> "NULLS FIRST"
    LastNullsOrder -> "NULLS LAST"
  parser settings = Parsers.keyword "nulls" *> Parsers.space1 *> Parser.endHead *> (FirstNullsOrder <$ Parsers.keyword "first" <|> LastNullsOrder <$ Parsers.keyword "last")

instance Qc.Arbitrary NullsOrder where
  shrink = Qc.genericShrink
  arbitrary = Qc.elements [minBound .. maxBound]

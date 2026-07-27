module PostgresqlSyntax.Ast.NullsOrder where

import HeadedMegaparsec
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)

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
  toTextBuilder = \case
    FirstNullsOrder -> "NULLS FIRST"
    LastNullsOrder -> "NULLS LAST"
  parser = keyword "nulls" *> space1 *> endHead *> (FirstNullsOrder <$ keyword "first" <|> LastNullsOrder <$ keyword "last")

instance Arbitrary NullsOrder where
  arbitrary = elements [minBound .. maxBound]

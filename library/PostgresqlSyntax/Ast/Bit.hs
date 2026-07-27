module PostgresqlSyntax.Ast.Bit where

import PostgresqlSyntax.Ast.ExprList
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.OptVarying
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)

-- |
-- ==== References
-- @
-- Bit:
--   | BitWithLength
--   | BitWithoutLength
-- ConstBit:
--   | BitWithLength
--   | BitWithoutLength
-- BitWithLength:
--   | BIT opt_varying '(' expr_list ')'
-- BitWithoutLength:
--   | BIT opt_varying
-- @
data Bit = Bit OptVarying (Maybe ExprList)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst Bit where
  toTextBuilder (Bit a b) =
    optLexemes
      [ Just "BIT",
        bool Nothing (Just "VARYING") (coerce a :: Bool),
        fmap (renderInParens . toTextBuilder) b
      ]
  parser = do
    keyword "bit"
    a <- parser
    b <- optional (space1 *> inParens parser)
    return (Bit a b)

instance Arbitrary Bit where
  arbitrary = Bit <$> arbitrary <*> arbitrary

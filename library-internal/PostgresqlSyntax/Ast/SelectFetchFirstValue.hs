module PostgresqlSyntax.Ast.SelectFetchFirstValue where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.CExpr
import PostgresqlSyntax.Ast.Fconst
import qualified PostgresqlSyntax.Extras.TextBuilder as TextBuilder
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- select_fetch_first_value:
--   | c_expr
--   | '+' I_or_F_const
--   | '-' I_or_F_const
-- @
data SelectFetchFirstValue
  = ExprSelectFetchFirstValue CExpr
  | NumSelectFetchFirstValue Bool (Either Int64 Double)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst SelectFetchFirstValue where
  toTextBuilder settings = \case
    ExprSelectFetchFirstValue a -> toTextBuilder settings a
    NumSelectFetchFirstValue a b -> bool "+" "-" a <> intOrFloat b
    where
      intOrFloat = either TextBuilder.int64Dec TextBuilder.doubleDec
  parser settings =
    ExprSelectFetchFirstValue
      <$> parser settings
        <|> NumSelectFetchFirstValue
      <$> (plusOrMinus <* Parser.endHead <* Parsers.space)
      <*> iconstOrFconst
    where
      plusOrMinus = False <$ Parsers.char '+' <|> True <$ Parsers.char '-'
      iconstOrFconst = Right <$> (coerce <$> (parser settings :: Parser Fconst)) <|> Left <$> Parsers.decimal

instance Qc.Arbitrary SelectFetchFirstValue where
  shrink = Qc.genericShrink

  -- The magnitude is parsed via unsigned 'Parser.decimal'\/'Fconst' (the
  -- sign is this type's own separate @Bool@ field), so, like
  -- 'PostgresqlSyntax.Ast.IntervalSecond'\'s @nonNegative@, it must never be
  -- negative itself — otherwise e.g. @NumSelectFetchFirstValue True (Left
  -- (-1))@ renders as @-1@ with no space (a valid unsigned-magnitude
  -- rendering would be @- 1@ or just @-1@ for magnitude 1), doubling up
  -- into @--1@, which reparses as a line comment.
  arbitrary =
    Qc.oneof
      [ ExprSelectFetchFirstValue <$> Qc.arbitrary,
        NumSelectFetchFirstValue <$> Qc.arbitrary <*> Qc.oneof [Left <$> nonNegativeInt64, Right <$> nonNegativeDouble]
      ]
    where
      nonNegativeInt64 = Qc.sized (\n -> Qc.choose (0, cap n))
      nonNegativeDouble = abs <$> Qc.arbitrary
      cap n
        | n >= 62 = maxBound
        | otherwise = 2 ^ n

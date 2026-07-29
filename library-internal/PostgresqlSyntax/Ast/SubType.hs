module PostgresqlSyntax.Ast.SubType where

import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- sub_type:
--   | ANY
--   | SOME
--   | ALL
-- @
data SubType = AnySubType | SomeSubType | AllSubType
  deriving (Show, Generic, Eq, Ord, Data, Enum, Bounded)

instance IsAst SubType where
  toTextBuilder settings = \case
    AnySubType -> "ANY"
    SomeSubType -> "SOME"
    AllSubType -> "ALL"
  parser settings =
    asum
      [ AnySubType <$ Parsers.keyword "any",
        SomeSubType <$ Parsers.keyword "some",
        AllSubType <$ Parsers.keyword "all"
      ]

instance Qc.Arbitrary SubType where
  shrink = Qc.genericShrink
  arbitrary = Qc.elements [minBound .. maxBound]

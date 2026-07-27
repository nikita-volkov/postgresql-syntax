module PostgresqlSyntax.Ast.SubType where

import PostgresqlSyntax.Ast.Internal
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
  toTextBuilder = \case
    AnySubType -> "ANY"
    SomeSubType -> "SOME"
    AllSubType -> "ALL"
  parser =
    asum
      [ AnySubType <$ keyword "any",
        SomeSubType <$ keyword "some",
        AllSubType <$ keyword "all"
      ]

instance Qc.Arbitrary SubType where
  arbitrary = Qc.elements [minBound .. maxBound]

module PostgresqlSyntax.Ast.QualAllOp where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.AllOp
import PostgresqlSyntax.Ast.AnyOperator
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- qual_all_Op:
--   | all_Op
--   | OPERATOR '(' any_operator ')'
-- @
data QualAllOp
  = AllQualAllOp AllOp
  | AnyQualAllOp AnyOperator
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst QualAllOp where
  toTextBuilder settings = \case
    AllQualAllOp a -> toTextBuilder settings a
    AnyQualAllOp a -> "OPERATOR (" <> toTextBuilder settings a <> ")"
  parser settings =
    asum
      [ AnyQualAllOp <$> (Parsers.keyword "operator" *> Parsers.space *> Parsers.inParens (Parser.endHead *> parser settings)),
        AllQualAllOp <$> parser settings
      ]

instance Qc.Arbitrary QualAllOp where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ AllQualAllOp <$> Qc.arbitrary,
        AnyQualAllOp <$> Qc.arbitrary
      ]

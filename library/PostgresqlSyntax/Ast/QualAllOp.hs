module PostgresqlSyntax.Ast.QualAllOp where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.AllOp
import PostgresqlSyntax.Ast.AnyOperator
import PostgresqlSyntax.Ast.Internal
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
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
  toTextBuilder = \case
    AllQualAllOp a -> toTextBuilder a
    AnyQualAllOp a -> "OPERATOR (" <> toTextBuilder a <> ")"
  parser =
    asum
      [ AnyQualAllOp <$> (keyword "operator" *> Parser.space *> inParens (Parser.endHead *> parser)),
        AllQualAllOp <$> parser
      ]

instance Qc.Arbitrary QualAllOp where
  arbitrary =
    Qc.oneof
      [ AllQualAllOp <$> Qc.arbitrary,
        AnyQualAllOp <$> Qc.arbitrary
      ]

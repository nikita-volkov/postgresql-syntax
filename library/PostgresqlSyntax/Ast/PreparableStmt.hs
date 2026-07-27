module PostgresqlSyntax.Ast.PreparableStmt where

import PostgresqlSyntax.Ast.CallStmt
import PostgresqlSyntax.Ast.DeleteStmt
import PostgresqlSyntax.Ast.InsertStmt
import PostgresqlSyntax.Ast.SelectStmt
import PostgresqlSyntax.Ast.UpdateStmt
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- PreparableStmt:
--   |  SelectStmt
--   |  InsertStmt
--   |  UpdateStmt
--   |  DeleteStmt
--   |  CallStmt
-- @
data PreparableStmt
  = SelectPreparableStmt SelectStmt
  | InsertPreparableStmt InsertStmt
  | UpdatePreparableStmt UpdateStmt
  | DeletePreparableStmt DeleteStmt
  | CallPreparableStmt CallStmt
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst PreparableStmt where
  toTextBuilder = \case
    SelectPreparableStmt a -> toTextBuilder a
    InsertPreparableStmt a -> toTextBuilder a
    UpdatePreparableStmt a -> toTextBuilder a
    DeletePreparableStmt a -> toTextBuilder a
    CallPreparableStmt a -> toTextBuilder a
  parser =
    asum
      [ SelectPreparableStmt <$> parser,
        InsertPreparableStmt <$> parser,
        UpdatePreparableStmt <$> parser,
        DeletePreparableStmt <$> parser,
        CallPreparableStmt <$> parser
      ]

instance Qc.Arbitrary PreparableStmt where
  arbitrary =
    Qc.oneof
      [ SelectPreparableStmt <$> Qc.scale (`div` 2) Qc.arbitrary,
        InsertPreparableStmt <$> Qc.scale (`div` 2) Qc.arbitrary,
        UpdatePreparableStmt <$> Qc.scale (`div` 2) Qc.arbitrary,
        DeletePreparableStmt <$> Qc.scale (`div` 2) Qc.arbitrary,
        CallPreparableStmt <$> Qc.scale (`div` 2) Qc.arbitrary
      ]

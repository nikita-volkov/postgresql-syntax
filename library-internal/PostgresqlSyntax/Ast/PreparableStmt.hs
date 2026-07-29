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
  toTextBuilder settings = \case
    SelectPreparableStmt a -> toTextBuilder settings a
    InsertPreparableStmt a -> toTextBuilder settings a
    UpdatePreparableStmt a -> toTextBuilder settings a
    DeletePreparableStmt a -> toTextBuilder settings a
    CallPreparableStmt a -> toTextBuilder settings a
  parser settings =
    asum
      [ SelectPreparableStmt <$> parser settings,
        InsertPreparableStmt <$> parser settings,
        UpdatePreparableStmt <$> parser settings,
        DeletePreparableStmt <$> parser settings,
        CallPreparableStmt <$> parser settings
      ]

instance Qc.Arbitrary PreparableStmt where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ SelectPreparableStmt <$> Qc.arbitrary,
        InsertPreparableStmt <$> Qc.arbitrary,
        UpdatePreparableStmt <$> Qc.arbitrary,
        DeletePreparableStmt <$> Qc.arbitrary,
        CallPreparableStmt <$> Qc.arbitrary
      ]

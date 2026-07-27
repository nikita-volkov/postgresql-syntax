module PostgresqlSyntax.Ast.FuncAliasClause where

import PostgresqlSyntax.Ast.AliasClause
import HeadedMegaparsec
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.TableFuncElementList
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
-- func_alias_clause:
--   | alias_clause
--   | AS '(' TableFuncElementList ')'
--   | AS ColId '(' TableFuncElementList ')'
--   | ColId '(' TableFuncElementList ')'
--   | EMPTY
-- @
data FuncAliasClause
  = AliasFuncAliasClause AliasClause
  | AsFuncAliasClause TableFuncElementList
  | AsColIdFuncAliasClause Ident TableFuncElementList
  | ColIdFuncAliasClause Ident TableFuncElementList
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst FuncAliasClause where
  toTextBuilder = \case
    AliasFuncAliasClause a -> toTextBuilder a
    AsFuncAliasClause a -> "AS (" <> toTextBuilder a <> ")"
    AsColIdFuncAliasClause a b -> "AS " <> toTextBuilder a <> " (" <> toTextBuilder b <> ")"
    ColIdFuncAliasClause a b -> toTextBuilder a <> " (" <> toTextBuilder b <> ")"
  parser =
    asum
      [ do
          _ <- keyword "as"
          asum
            [ do
                space
                inParens $ do
                  endHead
                  AsFuncAliasClause <$> parser,
              do
                space1
                a <- colId
                asum
                  [ do
                      space
                      inParens $ do
                        endHead
                        asum
                          [ AsColIdFuncAliasClause a <$> wrapToHead parser,
                            AliasFuncAliasClause . AliasClause True a . Just <$> parser
                          ],
                    pure (AliasFuncAliasClause (AliasClause True a Nothing))
                  ]
            ],
        do
          a <- colId
          asum
            [ do
                space
                inParens $ do
                  endHead
                  asum
                    [ ColIdFuncAliasClause a <$> wrapToHead parser,
                      AliasFuncAliasClause . AliasClause False a . Just <$> parser
                    ],
              pure (AliasFuncAliasClause (AliasClause False a Nothing))
            ]
      ]

instance Arbitrary FuncAliasClause where
  arbitrary =
    oneof
      [ AliasFuncAliasClause <$> arbitrary,
        AsFuncAliasClause <$> scale (`div` 2) arbitrary,
        AsColIdFuncAliasClause <$> arbitrary <*> scale (`div` 2) arbitrary,
        ColIdFuncAliasClause <$> arbitrary <*> scale (`div` 2) arbitrary
      ]

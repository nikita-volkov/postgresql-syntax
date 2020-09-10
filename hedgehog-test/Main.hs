module Main where

import Prelude
import Hedgehog
import Hedgehog.Main
import qualified Main.Gen as Gen
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified PostgresqlSyntax.Ast as Ast
import qualified PostgresqlSyntax.Parsing as Parsing
import qualified PostgresqlSyntax.Rendering as Rendering
import qualified Data.Text as Text


main = defaultMain [
    checkParallel $ Group "Parsing a rendered AST produces the same AST" $ let
      p _name _amount _gen _parser _renderer =
        (,) _name $ withDiscards (fromIntegral _amount * 200) $ withTests _amount $ property $ do
          ast <- forAll _gen
          let
            sql = Rendering.toText (_renderer ast)
            in do
              footnote ("SQL: " <> Text.unpack sql)
              case Parsing.run _parser sql of
                Left err -> do
                  footnote err
                  failure
                Right ast' -> ast === ast'
      in [
          p "typename" 10000 Gen.typename Parsing.typename Rendering.typename
          ,
          p "tableRef" 10000 Gen.tableRef Parsing.tableRef Rendering.tableRef
          ,
          p "aExpr" 60000 Gen.aExpr Parsing.aExpr Rendering.aExpr
          ,
          p "preparableStmt" 30000 Gen.preparableStmt Parsing.preparableStmt Rendering.preparableStmt
        ]
  ]

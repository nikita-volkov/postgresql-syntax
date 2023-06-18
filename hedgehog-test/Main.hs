module Main where

import qualified Data.Text as Text
import Hedgehog
import qualified Hedgehog.Gen as Gen
import Hedgehog.Main
import qualified Hedgehog.Range as Range
import qualified Main.Gen as Gen
import qualified PostgresqlSyntax.Ast as Ast
import qualified PostgresqlSyntax.Parsing as Parsing
import qualified PostgresqlSyntax.Rendering as Rendering
import Prelude

main =
  defaultMain
    [ checkParallel $
        Group "Parsing a rendered AST produces the same AST" $
          let p name amount gen parser renderer =
                (,) name $
                  withDiscards (fromIntegral amount * 200) $
                    withTests amount $
                      property $ do
                        ast <- forAll gen
                        let sql = Rendering.toText (renderer ast)
                         in do
                              footnote ("SQL: " <> Text.unpack sql)
                              case Parsing.run parser sql of
                                Left err -> do
                                  footnote err
                                  failure
                                Right ast' -> ast === ast'
           in [ p "typename" 10000 Gen.typename Parsing.typename Rendering.typename,
                p "tableRef" 10000 Gen.tableRef Parsing.tableRef Rendering.tableRef,
                p "aExpr" 60000 Gen.aExpr Parsing.aExpr Rendering.aExpr,
                p "preparableStmt" 30000 Gen.preparableStmt Parsing.preparableStmt Rendering.preparableStmt
              ]
    ]

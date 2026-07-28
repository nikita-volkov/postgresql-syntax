module Main (main) where

import qualified Cases
import Prelude
import qualified Properties
import Test.Hspec

main :: IO ()
main =
  hspec $ parallel $ do
    describe "Properties" Properties.spec
    describe "Cases" Cases.spec

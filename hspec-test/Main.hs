module Main (main) where

import qualified Spec
import Test.Hspec
import Prelude

main :: IO ()
main = hspec $ parallel Spec.spec

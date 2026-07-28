-- |
-- Shrink helpers shared by 2+ AST node modules.
module PostgresqlSyntax.Helpers.Shrinks where

import qualified Data.Text as Text
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- Shrinks a 'Text' value by shrinking it as a 'String' (dropping\/simplifying
-- characters).
shrinkText :: Text -> [Text]
shrinkText = fmap Text.pack . Qc.shrink . Text.unpack

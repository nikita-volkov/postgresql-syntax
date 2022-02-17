module PostgresqlSyntax.CharSet where

import qualified Data.Text as Text
import qualified PostgresqlSyntax.KeywordSet as KeywordSet
import PostgresqlSyntax.Prelude

{-# NOINLINE symbolicBinOp #-}
symbolicBinOp :: HashSet Char
symbolicBinOp = KeywordSet.symbolicBinOp & toList & mconcat & Text.unpack & fromList

{-# NOINLINE hexDigit #-}
hexDigit :: HashSet Char
hexDigit = fromList "0123456789abcdefABCDEF"

{-# NOINLINE op #-}
op :: HashSet Char
op = fromList "+-*/<>=~!@#%^&|`?"

{-# NOINLINE prohibitionLiftingOp #-}
prohibitionLiftingOp :: HashSet Char
prohibitionLiftingOp = fromList "~!@#%^&|`?"

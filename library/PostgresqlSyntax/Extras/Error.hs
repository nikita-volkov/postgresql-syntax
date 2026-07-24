{-# LANGUAGE NamedFieldPuns #-}

-- |
-- Generic helpers for Error handling in HeadedMegaParsec.
module PostgresqlSyntax.Extras.Error where

import qualified Data.Set as Set
import qualified Data.Text as Text
import PostgresqlSyntax.Prelude hiding (cons, fromList, head, init, last, reverse, tail, uncons)
import Text.Megaparsec

-- | Render all Megaparsec parsing errors as a list of position ('Text.Megaparsec.SourcePos') and messages.
errorBundlePrettyStruct ::
  forall s e.
  ( VisualStream s,
    TraversableStream s,
    ShowErrorComponent e
  ) =>
  -- | Parse error bundle to display
  ParseErrorBundle s e ->
  -- | Textual rendition of the bundle
  NonEmpty (SourcePos, String)
errorBundlePrettyStruct ParseErrorBundle {bundleErrors, bundlePosState} =
  fst $ attachSourcePosAndMessage renderError bundleErrors bundlePosState
  where
    renderError epos e = (epos, parseErrorTextPretty e)

-- | A custom version of 'Text.Megaparsec.attachSourcePos' to provide the NonEmpty list of errors with their position while only traversing the errors list once.
attachSourcePosAndMessage ::
  (TraversableStream s) =>
  -- | Format function for a single 'ParseError' and its 'SourcePos'
  (SourcePos -> ParseError s e -> (SourcePos, String)) ->
  -- | The collection of items
  NonEmpty (ParseError s e) ->
  -- | Initial 'PosState'
  PosState s ->
  -- | The collection with 'SourcePos'es added and the final 'PosState'
  (NonEmpty (SourcePos, String), PosState s)
attachSourcePosAndMessage format xs pst0 =
  swap $ mapAccumL step pst0 xs
  where
    step pst a =
      let pst' = reachOffsetNoLine (errorOffset a) pst
       in (pst', format (pstateSourcePos pst') a)

-- | Provide an equivalent to megaparsec's 'Text.Megaparsec.chunk' in a context where we manipulate the texts directly.
chunkFailure ::
  (Token s ~ Char, MonadParsec e s m) =>
  -- | expected chunk
  Text ->
  -- | actual (given) chunk
  Text ->
  m a
chunkFailure expectedTxt givenTxt = failure (Just (errorItemConverter givenTxt)) (Set.fromList (pure $ errorItemConverter expectedTxt))
  where
    -- Both arguments are always non-empty in practice (keyword literals and
    -- 'PostgresqlSyntax.Parsing.anyKeyword' results), but the fallback keeps
    -- this total rather than partial.
    errorItemConverter t = Tokens $ case Text.uncons t of
      Just (h, rest) -> h :| Text.unpack rest
      Nothing -> ' ' :| []

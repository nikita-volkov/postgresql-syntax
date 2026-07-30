-- |
-- Generic helpers for Error handling in HeadedMegaparsec.
module PostgresqlSyntax.Extras.Error where

import PostgresqlSyntax.Prelude hiding (head)
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
errorBundlePrettyStruct ParseErrorBundle {..} =
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

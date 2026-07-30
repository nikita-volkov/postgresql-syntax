{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- |
-- Benchmark for the "keyword allocation cost in wide alternations" concern
-- raised in <https://github.com/nikita-volkov/postgresql-syntax/issues/21>.
--
-- Compares the shipped, cheap 'PostgresqlSyntax.Helpers.Parsers.keyword'
-- (a bare 'empty' on mismatch, no allocation beyond the consumed text)
-- against a richer alternative that reports a proper \"expected one of ...\"
-- error via 'Text.Megaparsec.failure' (as PR #20 proposed), the way it would
-- behave sitting inside a wide 'asum'\/'choice' of keyword alternatives.
--
-- Both implementations here pin the reported error offset via
-- 'Megaparsec.setOffset', matching the fix applied to the shipped
-- 'PostgresqlSyntax.Helpers.Parsers.keyword' to stay correct under megaparsec
-- >=9.8's stricter '(<|>)' error-merging. That fix is what makes the rich
-- variant produce complete "expecting ..." lists at all under current
-- megaparsec; see 'PostgresqlSyntax.Helpers.Parsers.keyword''s haddock.
module Main where

import Criterion.Main
import qualified Data.Char as Char
import qualified Data.Set as Set
import qualified Data.Text as Text
import HeadedMegaparsec (HeadedParsec, parse, toParsec)
import qualified PostgresqlSyntax
import qualified Test.QuickCheck as Qc
import qualified Text.Megaparsec as Megaparsec
import Prelude

-- * Shared token scanner (mirrors 'PostgresqlSyntax.Helpers.Parsers.anyKeyword')

firstIdentifierChar :: Char -> Bool
firstIdentifierChar x = Char.isAlpha x || x == '_' || x >= '\200' && x <= '\377'

notFirstIdentifierChar :: Char -> Bool
notFirstIdentifierChar x = Char.isAlphaNum x || x == '_' || x == '$' || x >= '\200' && x <= '\377'

anyKeywordRaw :: Megaparsec.Parsec Void Text.Text Text.Text
anyKeywordRaw =
  Megaparsec.label "keyword" $ do
    firstChar <- Megaparsec.satisfy firstIdentifierChar
    remainder <- Megaparsec.takeWhileP Nothing notFirstIdentifierChar
    return (Text.toLower (Text.cons firstChar remainder))

-- * The two candidate implementations

-- | Shipped implementation: fails with a bare, unlabeled 'empty' on mismatch.
cheapKeyword :: Text.Text -> HeadedParsec Void Text.Text Text.Text
cheapKeyword expected = parse $ do
  off <- Megaparsec.getOffset
  parsed <- anyKeywordRaw
  if expected == parsed then return parsed else Megaparsec.setOffset off *> empty

-- | PR #20-style implementation: fails with an explicit chunk expectation,
-- so failed alternatives can be unioned into a readable "expecting one of
-- ..." message. Allocates a 'NonEmpty' 'Char' and a 'Set' per failed branch.
richKeyword :: Text.Text -> HeadedParsec Void Text.Text Text.Text
richKeyword expected = parse $ do
  off <- Megaparsec.getOffset
  parsed <- anyKeywordRaw
  if expected == parsed then return parsed else Megaparsec.setOffset off *> chunkFailure expected parsed
  where
    chunkFailure expectedTxt givenTxt =
      Megaparsec.failure
        (Just (errorItemConverter givenTxt))
        (Set.fromList (pure (errorItemConverter expectedTxt)))
    errorItemConverter t = Megaparsec.Tokens $ case Text.uncons t of
      Just (h, rest) -> h :| Text.unpack rest
      Nothing -> ' ' :| []

runHeaded :: HeadedParsec Void Text.Text a -> Text.Text -> Either (Megaparsec.ParseErrorBundle Text.Text Void) a
runHeaded p = Megaparsec.runParser (toParsec p <* Megaparsec.eof) ""

-- * Keyword word lists (sized to mirror the shipped keyword sets)

-- | ~90 words: the size of a typical mid-size dispatch, mirroring
-- @PostgresqlSyntax.KeywordSet.reservedKeyword@.
reservedWords :: [Text.Text]
reservedWords = ["all", "analyse", "analyze", "and", "any", "array", "as", "asc", "asymmetric", "both", "case", "cast", "check", "collate", "column", "constraint", "create", "current_catalog", "current_date", "current_role", "current_time", "current_timestamp", "current_user", "default", "deferrable", "desc", "distinct", "do", "else", "end", "except", "false", "fetch", "for", "foreign", "from", "grant", "group", "having", "in", "initially", "intersect", "into", "lateral", "leading", "limit", "localtime", "localtimestamp", "not", "null", "offset", "on", "only", "or", "order", "placing", "primary", "references", "returning", "select", "session_user", "some", "symmetric", "table", "then", "to", "trailing", "true", "union", "unique", "user", "using", "variadic", "when", "where", "window", "with"]

-- | ~440 words: the full reserved-word set, worst case, mirroring
-- @PostgresqlSyntax.KeywordSet.keyword@.
allWords :: [Text.Text]
allWords = ["abort", "absolute", "access", "action", "add", "admin", "after", "aggregate", "all", "also", "alter", "always", "analyse", "analyze", "and", "any", "array", "as", "asc", "assertion", "assignment", "asymmetric", "at", "attach", "attribute", "authorization", "backward", "before", "begin", "between", "bigint", "binary", "bit", "boolean", "both", "by", "cache", "call", "called", "cascade", "cascaded", "case", "cast", "catalog", "chain", "char", "character", "characteristics", "check", "checkpoint", "class", "close", "cluster", "coalesce", "collate", "collation", "column", "columns", "comment", "comments", "commit", "committed", "concurrently", "configuration", "conflict", "connection", "constraint", "constraints", "content", "continue", "conversion", "copy", "cost", "create", "cross", "csv", "cube", "current", "current_catalog", "current_date", "current_role", "current_schema", "current_time", "current_timestamp", "current_user", "cursor", "cycle", "data", "database", "day", "deallocate", "dec", "decimal", "declare", "default", "defaults", "deferrable", "deferred", "definer", "delete", "delimiter", "delimiters", "depends", "desc", "detach", "dictionary", "disable", "discard", "distinct", "do", "document", "domain", "double", "drop", "each", "else", "enable", "encoding", "encrypted", "end", "enum", "escape", "event", "except", "exclude", "excluding", "exclusive", "execute", "exists", "explain", "expression", "extension", "external", "extract", "false", "family", "fetch", "filter", "first", "float", "following", "for", "force", "foreign", "forward", "freeze", "from", "full", "function", "functions", "generated", "global", "grant", "granted", "greatest", "group", "grouping", "groups", "handler", "having", "header", "hold", "hour", "identity", "if", "ilike", "immediate", "immutable", "implicit", "import", "in", "include", "including", "increment", "index", "indexes", "inherit", "inherits", "initially", "inline", "inner", "inout", "input", "insensitive", "insert", "instead", "int", "integer", "intersect", "interval", "into", "invoker", "is", "isnull", "isolation", "join", "key", "label", "language", "large", "last", "lateral", "leading", "leakproof", "least", "left", "level", "like", "limit", "listen", "load", "local", "localtime", "localtimestamp", "location", "lock", "locked", "logged", "mapping", "match", "materialized", "maxvalue", "method", "minute", "minvalue", "mode", "month", "move", "name", "names", "national", "natural", "nchar", "new", "next", "nfc", "nfd", "nfkc", "nfkd", "no", "none", "normalize", "normalized", "not", "nothing", "notify", "notnull", "nowait", "null", "nullif", "nulls", "numeric", "object", "of", "off", "offset", "oids", "old", "on", "only", "operator", "option", "options", "or", "order", "ordinality", "others", "out", "outer", "over", "overlaps", "overlay", "overriding", "owned", "owner", "parallel", "parser", "partial", "partition", "passing", "password", "placing", "plans", "policy", "position", "preceding", "precision", "prepare", "prepared", "preserve", "primary", "prior", "privileges", "procedural", "procedure", "procedures", "program", "publication", "quote", "range", "read", "real", "reassign", "recheck", "recursive", "ref", "references", "referencing", "refresh", "reindex", "relative", "release", "rename", "repeatable", "replace", "replica", "reset", "restart", "restrict", "returning", "returns", "revoke", "right", "role", "rollback", "rollup", "routine", "routines", "row", "rows", "rule", "savepoint", "schema", "schemas", "scroll", "search", "second", "security", "select", "sequence", "sequences", "serializable", "server", "session", "session_user", "set", "setof", "sets", "share", "show", "similar", "simple", "skip", "smallint", "snapshot", "some", "sql", "stable", "standalone", "start", "statement", "statistics", "stdin", "stdout", "storage", "stored", "strict", "strip", "subscription", "substring", "support", "symmetric", "sysid", "system", "table", "tables", "tablesample", "tablespace", "temp", "template", "temporary", "text", "then", "ties", "time", "timestamp", "to", "trailing", "transaction", "transform", "treat", "trigger", "trim", "true", "truncate", "trusted", "type", "types", "uescape", "unbounded", "uncommitted", "unencrypted", "union", "unique", "unknown", "unlisten", "unlogged", "until", "update", "user", "using", "vacuum", "valid", "validate", "validator", "value", "values", "varchar", "variadic", "varying", "verbose", "version", "view", "views", "volatile", "when", "where", "whitespace", "window", "with", "within", "without", "work", "wrapper", "write", "xml", "xmlattributes", "xmlconcat", "xmlelement", "xmlexists", "xmlforest", "xmlnamespaces", "xmlparse", "xmlpi", "xmlroot", "xmlserialize", "xmltable", "year", "yes", "zone"]

-- * Wide-alternation scenarios

-- | An alternation over every element of the given keyword list, dispatched
-- via the given per-keyword implementation. This is the shape 'keyword'
-- actually sits in throughout the grammar: many 'HeadedParsec' alternatives
-- tried in sequence via '(<|>)'\/'asum'.
wideAlternation :: (Text.Text -> HeadedParsec Void Text.Text Text.Text) -> [Text.Text] -> HeadedParsec Void Text.Text Text.Text
wideAlternation impl kws = asum (map impl kws)

main :: IO ()
main = do
  let lastOf xs = last xs
      firstOf (x : _) = x
      firstOf [] = error "empty keyword list"
      noMatchToken = "zzzznotakeyword" :: Text.Text

  corpus <- replicateM 500 (Qc.generate (Qc.resize 15 (Qc.arbitrary @PostgresqlSyntax.PreparableStmt)))
  let corpusTexts = map (PostgresqlSyntax.toText mempty) corpus

  defaultMain
    [ bgroup
        "keyword in wide alternation"
        [ bgroup
            (label <> ", " <> scenario)
            [ bench "cheap" $ whnf (runHeaded (wideAlternation cheapKeyword kws)) token,
              bench "rich" $ whnf (runHeaded (wideAlternation richKeyword kws)) token
            ]
        | (label, kws) <- [("~90 words", reservedWords), ("~440 words", allWords)],
          (scenario, token) <-
            [ ("first match", firstOf kws),
              ("last match", lastOf kws),
              ("no match", noMatchToken)
            ]
        ],
      bgroup
        "full grammar on generated corpus (reference, shipped keyword only)"
        [bench "500 generated preparableStmt" $ nf (map (either (const False) (const True) . PostgresqlSyntax.parse @PostgresqlSyntax.PreparableStmt mempty)) corpusTexts]
    ]

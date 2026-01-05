module PostgresqlSyntax.KeywordSet where

import Data.HashSet
import PostgresqlSyntax.Prelude hiding (fromList, toList)

{-# NOINLINE keyword #-}
{-
From https://github.com/postgres/postgres/blob/1aac32df89eb19949050f6f27c268122833ad036/src/include/parser/kwlist.h
-}
keyword :: HashSet Text
keyword = fromList ["abort", "absolute", "access", "action", "add", "admin", "after", "aggregate", "all", "also", "alter", "always", "analyse", "analyze", "and", "any", "array", "as", "asc", "assertion", "assignment", "asymmetric", "at", "attach", "attribute", "authorization", "backward", "before", "begin", "between", "bigint", "binary", "bit", "boolean", "both", "by", "cache", "call", "called", "cascade", "cascaded", "case", "cast", "catalog", "chain", "char", "character", "characteristics", "check", "checkpoint", "class", "close", "cluster", "coalesce", "collate", "collation", "column", "columns", "comment", "comments", "commit", "committed", "concurrently", "configuration", "conflict", "connection", "constraint", "constraints", "content", "continue", "conversion", "copy", "cost", "create", "cross", "csv", "cube", "current", "current_catalog", "current_date", "current_role", "current_schema", "current_time", "current_timestamp", "current_user", "cursor", "cycle", "data", "database", "day", "deallocate", "dec", "decimal", "declare", "default", "defaults", "deferrable", "deferred", "definer", "delete", "delimiter", "delimiters", "depends", "desc", "detach", "dictionary", "disable", "discard", "distinct", "do", "document", "domain", "double", "drop", "each", "else", "enable", "encoding", "encrypted", "end", "enum", "escape", "event", "except", "exclude", "excluding", "exclusive", "execute", "exists", "explain", "expression", "extension", "external", "extract", "false", "family", "fetch", "filter", "first", "float", "following", "for", "force", "foreign", "forward", "freeze", "from", "full", "function", "functions", "generated", "global", "grant", "granted", "greatest", "group", "grouping", "groups", "handler", "having", "header", "hold", "hour", "identity", "if", "ilike", "immediate", "immutable", "implicit", "import", "in", "include", "including", "increment", "index", "indexes", "inherit", "inherits", "initially", "inline", "inner", "inout", "input", "insensitive", "insert", "instead", "int", "integer", "intersect", "interval", "into", "invoker", "is", "isnull", "isolation", "join", "key", "label", "language", "large", "last", "lateral", "leading", "leakproof", "least", "left", "level", "like", "limit", "listen", "load", "local", "localtime", "localtimestamp", "location", "lock", "locked", "logged", "mapping", "match", "materialized", "maxvalue", "method", "minute", "minvalue", "mode", "month", "move", "name", "names", "national", "natural", "nchar", "new", "next", "nfc", "nfd", "nfkc", "nfkd", "no", "none", "normalize", "normalized", "not", "nothing", "notify", "notnull", "nowait", "null", "nullif", "nulls", "numeric", "object", "of", "off", "offset", "oids", "old", "on", "only", "operator", "option", "options", "or", "order", "ordinality", "others", "out", "outer", "over", "overlaps", "overlay", "overriding", "owned", "owner", "parallel", "parser", "partial", "partition", "passing", "password", "placing", "plans", "policy", "position", "preceding", "precision", "prepare", "prepared", "preserve", "primary", "prior", "privileges", "procedural", "procedure", "procedures", "program", "publication", "quote", "range", "read", "real", "reassign", "recheck", "recursive", "ref", "references", "referencing", "refresh", "reindex", "relative", "release", "rename", "repeatable", "replace", "replica", "reset", "restart", "restrict", "returning", "returns", "revoke", "right", "role", "rollback", "rollup", "routine", "routines", "row", "rows", "rule", "savepoint", "schema", "schemas", "scroll", "search", "second", "security", "select", "sequence", "sequences", "serializable", "server", "session", "session_user", "set", "setof", "sets", "share", "show", "similar", "simple", "skip", "smallint", "snapshot", "some", "sql", "stable", "standalone", "start", "statement", "statistics", "stdin", "stdout", "storage", "stored", "strict", "strip", "subscription", "substring", "support", "symmetric", "sysid", "system", "table", "tables", "tablesample", "tablespace", "temp", "template", "temporary", "text", "then", "ties", "time", "timestamp", "to", "trailing", "transaction", "transform", "treat", "trigger", "trim", "true", "truncate", "trusted", "type", "types", "uescape", "unbounded", "uncommitted", "unencrypted", "union", "unique", "unknown", "unlisten", "unlogged", "until", "update", "user", "using", "vacuum", "valid", "validate", "validator", "value", "values", "varchar", "variadic", "varying", "verbose", "version", "view", "views", "volatile", "when", "where", "whitespace", "window", "with", "within", "without", "work", "wrapper", "write", "xml", "xmlattributes", "xmlconcat", "xmlelement", "xmlexists", "xmlforest", "xmlnamespaces", "xmlparse", "xmlpi", "xmlroot", "xmlserialize", "xmltable", "year", "yes", "zone"]

{-# NOINLINE unreservedKeyword #-}
unreservedKeyword :: HashSet Text
unreservedKeyword = fromList ["abort", "absolute", "access", "action", "add", "admin", "after", "aggregate", "also", "alter", "always", "assertion", "assignment", "at", "attach", "attribute", "backward", "before", "begin", "by", "cache", "call", "called", "cascade", "cascaded", "catalog", "chain", "characteristics", "checkpoint", "class", "close", "cluster", "columns", "comment", "comments", "commit", "committed", "configuration", "conflict", "connection", "constraints", "content", "continue", "conversion", "copy", "cost", "csv", "cube", "current", "cursor", "cycle", "data", "database", "day", "deallocate", "declare", "defaults", "deferred", "definer", "delete", "delimiter", "delimiters", "depends", "detach", "dictionary", "disable", "discard", "document", "domain", "double", "drop", "each", "enable", "encoding", "encrypted", "enum", "escape", "event", "exclude", "excluding", "exclusive", "execute", "explain", "extension", "external", "family", "filter", "first", "following", "force", "forward", "function", "functions", "generated", "global", "granted", "groups", "handler", "header", "hold", "hour", "identity", "if", "immediate", "immutable", "implicit", "import", "include", "including", "increment", "index", "indexes", "inherit", "inherits", "inline", "input", "insensitive", "insert", "instead", "invoker", "isolation", "key", "label", "language", "large", "last", "leakproof", "level", "listen", "load", "local", "location", "lock", "locked", "logged", "mapping", "match", "materialized", "maxvalue", "method", "minute", "minvalue", "mode", "month", "move", "name", "names", "new", "next", "no", "nothing", "notify", "nowait", "nulls", "object", "of", "off", "oids", "old", "operator", "option", "options", "ordinality", "others", "over", "overriding", "owned", "owner", "parallel", "parser", "partial", "partition", "passing", "password", "plans", "policy", "preceding", "prepare", "prepared", "preserve", "prior", "privileges", "procedural", "procedure", "procedures", "program", "publication", "quote", "range", "read", "reassign", "recheck", "recursive", "ref", "referencing", "refresh", "reindex", "relative", "release", "rename", "repeatable", "replace", "replica", "reset", "restart", "restrict", "returns", "revoke", "role", "rollback", "rollup", "routine", "routines", "rows", "rule", "savepoint", "schema", "schemas", "scroll", "search", "second", "security", "sequence", "sequences", "serializable", "server", "session", "set", "sets", "share", "show", "simple", "skip", "snapshot", "sql", "stable", "standalone", "start", "statement", "statistics", "stdin", "stdout", "storage", "stored", "strict", "strip", "subscription", "support", "sysid", "system", "tables", "tablespace", "temp", "template", "temporary", "text", "ties", "transaction", "transform", "trigger", "truncate", "trusted", "type", "types", "unbounded", "uncommitted", "unencrypted", "unknown", "unlisten", "unlogged", "until", "update", "vacuum", "valid", "validate", "validator", "value", "varying", "version", "view", "views", "volatile", "whitespace", "within", "without", "work", "wrapper", "write", "xml", "year", "yes", "zone"]

{-# NOINLINE colNameKeyword #-}
colNameKeyword :: HashSet Text
colNameKeyword = fromList ["between", "bigint", "bit", "boolean", "char", "character", "coalesce", "dec", "decimal", "exists", "extract", "float", "greatest", "grouping", "inout", "int", "integer", "interval", "least", "national", "nchar", "none", "normalize", "nullif", "numeric", "out", "overlay", "position", "precision", "real", "row", "setof", "smallint", "substring", "time", "timestamp", "treat", "trim", "values", "varchar", "xmlattributes", "xmlconcat", "xmlelement", "xmlexists", "xmlforest", "xmlnamespaces", "xmlparse", "xmlpi", "xmlroot", "xmlserialize", "xmltable"]

{-# NOINLINE typeFuncNameKeyword #-}
typeFuncNameKeyword :: HashSet Text
typeFuncNameKeyword = fromList ["authorization", "binary", "collation", "concurrently", "cross", "current_schema", "freeze", "full", "ilike", "inner", "is", "isnull", "join", "left", "like", "natural", "notnull", "outer", "overlaps", "right", "similar", "tablesample", "verbose"]

{-# NOINLINE reservedKeyword #-}
reservedKeyword :: HashSet Text
reservedKeyword = fromList ["all", "analyse", "analyze", "and", "any", "array", "as", "asc", "asymmetric", "both", "case", "cast", "check", "collate", "column", "constraint", "create", "current_catalog", "current_date", "current_role", "current_time", "current_timestamp", "current_user", "default", "deferrable", "desc", "distinct", "do", "else", "end", "except", "false", "fetch", "for", "foreign", "from", "grant", "group", "having", "in", "initially", "intersect", "into", "lateral", "leading", "limit", "localtime", "localtimestamp", "not", "null", "offset", "on", "only", "or", "order", "placing", "primary", "references", "returning", "select", "session_user", "some", "symmetric", "table", "then", "to", "trailing", "true", "union", "unique", "user", "using", "variadic", "when", "where", "window", "with"]

{-# NOINLINE symbolicBinOp #-}
symbolicBinOp :: HashSet Text
symbolicBinOp = fromList ["+", "-", "*", "/", "%", "^", "<", ">", "=", "<=", ">=", "<>", "~~", "~~*", "!~~", "!~~*", "~", "~*", "!~", "!~*"]

{-# NOINLINE lexicalBinOp #-}
lexicalBinOp :: HashSet Text
lexicalBinOp = fromList ["and", "or"]

{-# NOINLINE colId #-}
colId :: HashSet Text
colId = unions [unreservedKeyword, colNameKeyword]

{-
type_function_name:
  | IDENT
  | unreserved_keyword
  | type_func_name_keyword
-}
{-# NOINLINE typeFunctionName #-}
typeFunctionName :: HashSet Text
typeFunctionName = unions [unreservedKeyword, typeFuncNameKeyword]

-- |
-- As per the following comment from the original scanner definition:
--
-- /*
--  * Likewise, if what we have left is two chars, and
--  * those match the tokens ">=", "<=", "=>", "<>" or
--  * "!=", then we must return the appropriate token
--  * rather than the generic Op.
--  */
{-# NOINLINE nonOp #-}
nonOp :: HashSet Text
nonOp = fromList [">=", "<=", "=>", "<>", "!="] <> mathOp

{-# NOINLINE mathOp #-}
mathOp :: HashSet Text
mathOp = fromList ["<>", ">=", "!=", "<=", "+", "-", "*", "/", "%", "^", "<", ">", "="]

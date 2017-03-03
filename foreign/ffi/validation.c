#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pg_query.h"


int validate(const char *sql, char *output) {

  PgQueryParseResult result = pg_query_parse(sql);
  PgQueryError *error = result.error;

  if (error) {
    sprintf(output, "%s at offset %d", error->message, error->cursorpos - 1);
  }

  pg_query_free_parse_result(result);

  return error ? 1 : 0;

}

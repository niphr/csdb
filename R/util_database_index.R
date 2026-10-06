# The index naming rules, and the S7 method assignments for get_indexes,
# get_index_columns, drop_index and add_index.
#
# The generics and the db_* class objects are in "util_database.R". R
# sources this directory in C collation order. This name sorts after that
# one, so every generic and class exists before the assignments below run.
#
# The one exception is the ensure_index generic. It is defined at the end of
# this file, beside its only methods and its only caller.

# index naming
#
# The UUID namespace that physical_name() hashes under. The value is
# arbitrary and fixed. Changing it renames every index and every primary key
# constraint csdb manages.
INDEX_NAME_NAMESPACE <- "3f2a6c1e-7d54-4b98-9a0f-6c5d2e8b41a7"

# The longest identifier PostgreSQL keeps. NAMEDATALEN is 64 bytes, and one
# byte holds the terminator, so 63 characters survive.
INDEX_NAME_MAX_CHARS <- 63L

# The front of every physical index name.
INDEX_NAME_PREFIX <- "ix_"

# The front of every primary key constraint name.
PK_NAME_PREFIX <- "pk_"

# How many hexadecimal characters of the digest the name carries.
INDEX_NAME_DIGEST_CHARS <- 16L

# The ordered name components of a table, for index naming.
#
# The components, and NOT a joined string. Joining them loses the boundary
# between them, and the boundary is what tells two tables apart.
# `Id(schema = "a", table = "b.c")` and `Id(schema = "a.b", table = "c")` both
# join to `a.b.c`. A joined identity therefore gives them one index name.
#
# The identity MUST be the same at every call site. A name built at creation
# otherwise does not match the name used at drop, and the index becomes
# unreachable.
#
# csdb passes the DBI::Id at every site that needs an identity, and never the
# text form. Those sites are DBTable_v9$add_indexes(), $drop_indexes(),
# $confirm_indexes(), and the drop inside the default upsert method below.
# They all read
# table_name_short_for_mssql_fully_specified_for_postgres, so they all give
# one component vector, for every table name.
#
# Text is accepted for a caller that holds only the text form. Text splits on
# every dot, so `"anon.tab"` and `Id(schema = "anon", table = "tab")` give one
# vector. That agreement is what a text caller relies on, and a test pins it.
#
# The split keeps every empty component, because R drops a trailing empty
# string and `"a.b."` would otherwise read as `"a.b"`.
#
# Text with a dot names exactly the identity of its dot-separated pieces.
# That is a definition and not a guess. It costs one thing: `"a.b.c"` names
# three components. It is therefore NOT the text form of
# `Id(schema = "a", table = "b.c")`, which names two. No text can tell those
# two apart, because `"anon.tab"` is equally the text form of
# `Id(table = "anon.tab")`. csdb answers this by never using text as an
# identity, and not by a rule inside this function.
#
# table    Text, or a DBI::Id.
# returns  A character vector of the ordered name components.
index_table_identity <- function(table) {
  if (methods::is(table, "Id")) {
    return(as.character(table@name))
  }
  if (!is.character(table) || length(table) != 1L || is.na(table)) {
    stop("table must be one character string, or a DBI::Id.", call. = FALSE)
  }
  dots <- gregexpr(".", table, fixed = TRUE)[[1]]
  n <- if (attr(dots, "match.length")[1] == -1L) 0L else length(dots)
  parts <- strsplit(table, ".", fixed = TRUE)[[1]]
  length(parts) <- n + 1L
  parts[is.na(parts)] <- ""
  return(parts)
}

# The dotted text form of a table identity.
#
# Two drop_index methods paste `table` straight into SQL with glue::glue().
# glue coerces with as.character(), and that raises
# `no method for coercing this S4 class to a vector` on a DBI::Id. csdb now
# hands those methods the Id, so they convert it here first.
#
# The result is byte-identical to
# table_name_short_for_mssql_fully_specified_for_postgres_text for every
# identity, so neither method changes what it emits for a table it already
# handled.
#
# table    Text, or a DBI::Id.
# returns  One character string.
index_table_text <- function(table) {
  return(paste(index_table_identity(table), collapse = "."))
}

# The name a declared index or primary key carries in the database.
#
# csdb used the caller's logical name as the database name, verbatim.
# `indexes = list(ind1 = "a")` created an index called `ind1`. A PostgreSQL
# index name is unique per SCHEMA, so every table in one schema that declared
# `ind1` asked for one name. `CREATE INDEX IF NOT EXISTS` answers a taken name
# with a notice, not an error. The first table won the name, and every later
# table silently got nothing.
#
# Measured on the norsyss_data1 database on 2026-08-15. The table
# `anon_norsyss_data` had 87 partitions in schema `anon`. All 87 declared
# `ind1` and `ind2`. One partition held `ind2`, and none held `ind1`.
#
# The name built here carries the table identity, so one table cannot take
# another table's name. It has three parts:
#
#   <prefix>  `ix_` for an index, `pk_` for a primary key constraint. It
#             keeps a letter at the front. PostgreSQL rejects an unquoted
#             identifier that starts with a digit, and neither the db_postgres
#             add_index method nor add_constraint() quotes the name.
#   <slug>    The identity and the logical name, lowercased, with every other
#             character replaced by an underscore. This part is readable only.
#             It is cut from the LEFT when it is too long. That keeps the
#             logical name, because the logical name sits at the end.
#   <digest>  16 hexadecimal characters of a version 5 UUID over the identity
#             and the logical name. This part carries the distinctness.
#
# The result is at most 63 characters, the PostgreSQL identifier limit.
# PostgreSQL truncates a longer name and reports it with a notice, not an
# error. Two long names can truncate to one name. `IF NOT EXISTS` then skips
# the second statement with another notice, and the second table gets
# nothing. csdb therefore applies the limit here, rather than leave it to the
# server.
#
# The name is lowercase. PostgreSQL folds an unquoted identifier to lowercase
# and SQLite does not. A lowercase name therefore reads the same in the source
# and in both catalogues. Measured on norsyss_data1: 92 lowercase `pk_`
# constraint names and 0 uppercase, while the source wrote `PK_`.
#
# The key the digest covers is unambiguous. It length-prefixes the component
# count, then every table name component, then the logical name, so no two
# different inputs build one key. The rule it replaced for primary keys,
# `PK_{table}`, deleted `.`, `[` and `]`. Under that rule, schema `a` with
# table `bc` and schema `ab` with table `c` both gave `PK_abc`. The key does
# not hold the prefix, so the prefix alone separates an index from a primary
# key constraint.
#
# The name is collision-resistant and not injective. 16 hexadecimal characters
# hold 64 bits, and the version nibble of a version 5 UUID is fixed, so 60 bits
# vary. Two different keys give one name when those 60 bits agree. The key
# construction removes every structural collision; the digest width bounds what
# is left.
#
# table    The table identity. Text, or a DBI::Id.
# logical  One logical name: an index name, or "pk" for the primary key.
# prefix   The front of the name, INDEX_NAME_PREFIX or PK_NAME_PREFIX.
# returns  One lowercase character string of at most 63 characters.
physical_name <- function(table, logical, prefix) {
  if (!is.character(logical) || length(logical) != 1L || is.na(logical)) {
    stop("index must be one character string.", call. = FALSE)
  }
  parts <- index_table_identity(table)
  identity <- paste(parts, collapse = ".")

  # Every field is length-prefixed, and the first field is the component
  # count. A reader can therefore recover the exact input, so no two different
  # inputs build one key. Without the per-component prefix the pair
  # (Id("a", "b.c"), "ind1") and the pair (Id("a.b", "c"), "ind1") share a key.
  fields <- c(as.character(length(parts)), parts, logical)
  key <- paste0(paste0(nchar(fields), ":", fields), collapse = "")
  digest <- gsub(
    "-",
    "",
    uuid::UUIDfromName(INDEX_NAME_NAMESPACE, key),
    fixed = TRUE
  )
  digest <- substr(digest, 1L, INDEX_NAME_DIGEST_CHARS)

  slug_max <- INDEX_NAME_MAX_CHARS -
    nchar(prefix) -
    1L -
    INDEX_NAME_DIGEST_CHARS
  slug <- tolower(paste0(identity, "_", logical))
  # Every character outside the class becomes an underscore, so the slug is
  # ASCII and one character is one byte. The 63 above is a byte limit.
  slug <- gsub("[^a-z0-9]+", "_", slug)
  slug <- gsub("^_+|_+$", "", slug)
  if (nchar(slug) > slug_max) {
    slug <- substring(slug, nchar(slug) - slug_max + 1L)
    slug <- sub("^_+", "", slug)
  }

  if (nzchar(slug)) {
    return(paste0(prefix, slug, "_", digest))
  } else {
    return(paste0(prefix, digest))
  }
}

# The name a declared index carries in the database. See physical_name().
#
# table    The table identity. Text, or a DBI::Id.
# index    One logical index name, from names(self$indexes).
# returns  One lowercase character string of at most 63 characters.
index_physical_name <- function(table, index) {
  return(
    physical_name(table = table, logical = index, prefix = INDEX_NAME_PREFIX)
  )
}

# The name of the primary key constraint of a table. See physical_name().
#
# add_constraint() and drop_constraint() both call this with the table
# identity they receive, so a drop finds the name that the add created.
#
# table    The table identity. Text, or a DBI::Id.
# returns  One lowercase character string of at most 63 characters.
pk_physical_name <- function(table) {
  return(physical_name(table = table, logical = "pk", prefix = PK_NAME_PREFIX))
}

# get_indexes methods
S7::method(get_indexes, db_mssql) <- function(connection, table) {
  index_name <- NULL
  table_name <- NULL

  table_rows <- connection |>
    DBI::dbGetQuery(
      "select o.name as table_name, i.name as index_name from sys.objects o join sys.sysindexes i on o.object_id = i.id where o.is_ms_shipped = 0 and i.rowcnt > 0 order by o.name"
    ) |>
    dplyr::filter(
      !is.na(index_name) & !stringr::str_detect(index_name, "^PK")
    ) |>
    setDT()
  retval <- table_rows[table_name %in% table]$index_name
  return(retval)
}

S7::method(get_indexes, db_postgres) <- function(connection, table) {
  index_name <- NULL
  table_name <- NULL

  sql <- "
    select tablename, indexname
    from pg_indexes
  "

  table_rows <- connection |>
    DBI::dbGetQuery(sql) |>
    dplyr::filter(!is.na(indexname) & !stringr::str_detect(indexname, "^pk")) |>
    setDT()
  retval <- table_rows[tablename %in% table]$indexname
  return(retval)
}

# List the indexes a SQLite table carries, in creation order.
#
# Three details are load-bearing:
#
#  1. `tbl_name` is a string VALUE in sqlite_master, not an identifier, so it
#     is bound as a parameter. DBI::dbQuoteIdentifier() would emit `` `tab` ``
#     and match nothing.
#  2. `name NOT LIKE 'sqlite\_%' ESCAPE '\'` removes the index a PRIMARY KEY
#     creates on its own, `sqlite_autoindex_<table>_1`. Without the filter
#     this returns the autoindex beside every index csdb manages. A caller
#     that counts or compares the whole vector then reads one index too many.
#
#     The ESCAPE clause is mandatory, not decoration. `_` is a
#     single-character wildcard in SQL LIKE, so the unescaped
#     `NOT LIKE 'sqlite_%'` also hides every user index whose name begins
#     "sqlite" followed by any character at all. An index named `sqliteIdx`
#     would then never be found, and every caller would read it as missing.
#     Written `'sqlite\\_%' ESCAPE '\\'` in R, so a literal backslash reaches
#     SQLite.
#  3. `ORDER BY rowid` is creation order, which is the order add_indexes()
#     iterates names(self$indexes) in. A caller that compares the whole vector
#     with identical() needs that order.
#
# The return value is `$name`, a plain character vector. A one-column
# data.frame, or a vector carrying names or any other attribute, fails an
# identical() test that a caller has every reason to make.
#
# The comment block is deliberately plain `#` rather than roxygen `#'`:
# roxygen2 cannot name an S7 method registered against an S4 class.
S7::method(get_indexes, db_sqlite) <- function(connection, table) {
  sql <- paste0(
    "SELECT name FROM sqlite_master ",
    "WHERE type = 'index' AND tbl_name = ? ",
    "AND name NOT LIKE 'sqlite\\_%' ESCAPE '\\' ",
    "ORDER BY rowid"
  )
  retval <- DBI::dbGetQuery(connection, sql, params = list(table))$name
  return(retval)
}

# get_index_columns methods
#
# Every method answers one question. Which columns does `index` cover on
# `table`, in index order?
#
# Column verification is defined for SQLite and for PostgreSQL, and for no
# other backend. The db_default method returns NULL, which means "this backend
# has no catalogue reader". On such a backend add_indexes() creates the index
# and does NOT verify it, and confirm_indexes() falls back to existence by
# name. SQL Server and MySQL both dispatch to db_default and both get that
# weaker guarantee.
#
# There are three answers, and they are distinct:
#
#   NULL          The backend has no catalogue reader. Nothing was measured.
#   character(0)  No index of that name exists on that table.
#   a character   The column names, in index order.
#
# The distinction is what lets add_indexes() raise on a measured absence and
# stay quiet on an unmeasured one. `CREATE INDEX IF NOT EXISTS` reporting
# success proves nothing about which columns were indexed, and nothing about
# which table holds the name.
#
# The comment block is deliberately plain `#` rather than roxygen `#'`:
# roxygen2 cannot name an S7 method registered against an S4 class.
S7::method(get_index_columns, db_default) <- function(
  connection,
  table,
  index
) {
  return(NULL)
}

# Read the columns of one SQLite index.
#
# Two queries run here, and both are needed.
#
# A SQLite index name is unique per database, not per table, so
# `pragma_index_info` answers for an index on ANY table. The first query reads
# `tbl_name` from sqlite_master and rejects an index that belongs to another
# table. Without it, a name that another table already took would read as a
# correct index here. That is the exact failure this release removes.
#
# `pragma_index_info` is the table-valued form of the pragma. It accepts a
# bound parameter, where `PRAGMA index_info(...)` does not. `seqno` is the
# position of the column inside the index, so ORDER BY seqno gives the
# declared order.
S7::method(get_index_columns, db_sqlite) <- function(connection, table, index) {
  parts <- index_table_identity(table)
  table_bare <- parts[length(parts)]
  owner <- DBI::dbGetQuery(
    connection,
    "SELECT tbl_name FROM sqlite_master WHERE type = 'index' AND name = ?",
    params = list(index)
  )$tbl_name
  if (length(owner) == 0L || !identical(owner[1], table_bare)) {
    return(character(0))
  }
  return(
    DBI::dbGetQuery(
      connection,
      "SELECT name FROM pragma_index_info(?) ORDER BY seqno",
      params = list(index)
    )$name
  )
}

# Read the columns of one PostgreSQL index.
#
# The index relation carries its own columns in pg_attribute. They are
# numbered from 1 in index order, so ORDER BY attnum gives the declared order.
# `attname` is the base column name for a plain column reference.
#
# `table` arrives as the fully specified text, `schema.table`. pg_class holds
# the bare name and pg_namespace holds the schema, so the text is split on the
# last dot. An empty schema drops the pg_namespace test rather than match on
# the empty string, which would match nothing.
#
# csdb's test suite runs on SQLite alone, so the db_sqlite method above is the
# one under test. This method was verified by hand against the norsyss_data1
# server on 2026-08-15. Two tables in one schema, both declaring the index name
# ind1, each returned isoyearweek for its own index and nothing for the other.
# No automated test covers it.
S7::method(get_index_columns, db_postgres) <- function(
  connection,
  table,
  index
) {
  parts <- index_table_identity(table)
  table_bare <- parts[length(parts)]
  schema <- if (length(parts) > 1L) parts[length(parts) - 1L] else ""

  sql <- paste0(
    "select a.attname as column_name ",
    "from pg_class ic ",
    "join pg_namespace n on n.oid = ic.relnamespace ",
    "join pg_index ix on ix.indexrelid = ic.oid ",
    "join pg_class tc on tc.oid = ix.indrelid ",
    "join pg_attribute a on a.attrelid = ic.oid and a.attnum > 0 ",
    "where ic.relkind = 'i' and ic.relname = ? and tc.relname = ?"
  )
  params <- list(index, table_bare)
  if (nzchar(schema)) {
    sql <- paste0(sql, " and n.nspname = ?")
    params <- c(params, list(schema))
  }
  sql <- paste0(sql, " order by a.attnum")

  return(DBI::dbGetQuery(connection, sql, params = params)$column_name)
}

# drop_index methods
S7::method(drop_index, db_default) <- function(connection, table, index) {
  # DBTable_v9 hands this method a DBI::Id, and glue::glue() cannot coerce
  # one. index_table_text() gives the same string the caller used to pass.
  table <- index_table_text(table)
  return(try(
    DBI::dbExecute(
      connection,
      glue::glue("ALTER TABLE `{table}` DROP INDEX `{index}`")
    ),
    TRUE
  ))
}

S7::method(drop_index, db_mssql) <- function(connection, table, index) {
  table <- index_table_text(table)
  return(try(
    DBI::dbExecute(
      connection,
      glue::glue("DROP INDEX {table}.{index}")
    ),
    TRUE
  ))
}

# Drop a PostgreSQL index.
#
# The statement names the SCHEMA. `DROP INDEX IF EXISTS ind1` resolves the
# name through search_path, and csdb creates every index on a fully specified
# table. The index lands in that table's schema, so an unqualified drop finds
# it only when the schema is on the path. The try() below then hides the miss,
# and the index stays.
#
# The method took `table` and ignored it until 2026.8.16. It uses it now: the
# schema is the PENULTIMATE component of the table identity, and not every
# component except the last. The two differ on `catalog.schema.table`, where
# the penultimate component is `schema` alone. That is what PostgreSQL wants
# here, because an index lives in a schema and a DROP INDEX names
# `schema.index`. A catalog-qualified index name is not valid there.
#
# The schema and the index are quoted separately, and never pasted in raw.
# `DROP INDEX IF EXISTS an.on.ix_1` names schema `an` and index `on.ix_1`,
# which is a different index, and the try() below hides the miss.
S7::method(drop_index, db_postgres) <- function(connection, table, index) {
  parts <- index_table_identity(table)
  schema <- if (length(parts) > 1L) parts[length(parts) - 1L] else ""
  index_quoted <- as.character(DBI::dbQuoteIdentifier(connection, index))
  target <- if (nzchar(schema)) {
    paste0(
      as.character(DBI::dbQuoteIdentifier(connection, schema)),
      ".",
      index_quoted
    )
  } else {
    index_quoted
  }

  return(try(
    DBI::dbExecute(
      connection,
      glue::glue("DROP INDEX IF EXISTS {target}")
    ),
    TRUE
  ))
}

# Drop a SQLite index.
#
# In SQLite an index belongs to the schema, not to the table, so the statement
# names the index alone: `DROP INDEX <index> ON <table>` is a syntax error.
# `IF EXISTS` makes dropping an absent index a no-op, which is why there is no
# try() here where the other three backends have one.
#
# `table` is accepted and ignored, because drop_index() is one generic and the
# other three methods need it.
#
# The comment block is deliberately plain `#` rather than roxygen `#'`:
# roxygen2 cannot name an S7 method registered against an S4 class.
S7::method(drop_index, db_sqlite) <- function(connection, table, index) {
  return(DBI::dbExecute(
    connection,
    paste0(
      "DROP INDEX IF EXISTS ",
      DBI::dbQuoteIdentifier(connection, index)
    )
  ))
}

# add_index methods
#
# A method here either creates the index or raises. It never returns the
# failure as a value.
#
# The db_default and db_postgres methods wrapped their DBI::dbExecute() call
# in try(..., T) until 2026.8.16. Every failure then came back as a
# `try-error` object that no caller reads. The PostgreSQL upsert method asked
# for an index named `"ind" + random_uuid()`. That expression raises in R, so
# the method built every temporary table with no index. The caller could not
# tell any of that apart from success.
#
# One failure stayed silent after that change, and it was the largest one. A
# PostgreSQL index name is unique per SCHEMA. `CREATE INDEX IF NOT EXISTS`
# answers a taken name with a notice, not an error. Measured on the
# norsyss_data1 database on 2026-08-15: `anon_norsyss_data` had 87 partitions.
# All 87 asked for the same two index names. One partition held `ind2`, and
# none held `ind1`. Removing try() does not reach that. index_physical_name()
# does. The name now carries the table identity, so two tables in one schema
# no longer ask for one name.
#
# db_mssql still wraps its call. Its SQL is invalid and its only caller passes
# no index name, so removing the wrapper there needs a SQL Server to verify
# against. That work is deferred, and the wrapper marks it.
#
# The comment block is deliberately plain `#` rather than roxygen `#'`:
# roxygen2 cannot name an S7 method registered against an S4 class.
S7::method(add_index, db_default) <- function(connection, table, index, keys) {
  # DBTable_v9 hands this method a DBI::Id, and glue::glue() cannot coerce
  # one. index_table_text() gives the same string the caller used to pass.
  table <- index_table_text(table)
  keys <- glue::glue_collapse(keys, sep = ", ")

  sql <- glue::glue(
    "
    ALTER TABLE `{table}` ADD INDEX `{index}` ({keys})
    ;"
  )
  return(DBI::dbExecute(connection, sql))
}

S7::method(add_index, db_mssql) <- function(connection, table, index, keys) {
  keys <- glue::glue_collapse(keys, sep = ", ")

  return(try(
    DBI::dbExecute(
      connection,
      glue::glue("CREATE INDEX {index} IF NOT EXISTS ON {table} ({keys});")
    ),
    TRUE
  ))
}

# Create a PostgreSQL index.
#
# `table` MUST arrive as a DBI::Id, or as one unqualified name. The method
# quotes it here rather than take it pre-quoted. A caller that hands over
# `"anon.tab"` as text gets one identifier called `anon.tab`, which is a
# different table.
#
# Every identifier is quoted: the index, the table and each key column. csdb
# built this statement by pasting all three in raw until 2026.8.16. A name
# holding a dot, a space or an upper case letter then produced SQL that
# PostgreSQL read as something else, or rejected. `anon.my tab` is a syntax
# error, and `anon.MyTab` silently folds to `anon.mytab`.
#
# Quoting the index name changes nothing that csdb generates.
# index_physical_name() emits `[a-z][a-z0-9_]*`, and the upsert method below
# emits `ind` plus random_uuid(), which is also lower case and alphanumeric.
# PostgreSQL folds an unquoted name of that shape to itself.
S7::method(add_index, db_postgres) <- function(connection, table, index, keys) {
  keys_quoted <- paste0(
    DBI::dbQuoteIdentifier(connection, keys),
    collapse = ", "
  )

  return(DBI::dbExecute(
    connection,
    paste0(
      "CREATE INDEX IF NOT EXISTS ",
      DBI::dbQuoteIdentifier(connection, index),
      " ON ",
      DBI::dbQuoteIdentifier(connection, table),
      " (",
      keys_quoted,
      ")"
    )
  ))
}

# Create a SQLite index.
#
# The table name MUST be unqualified. SQLite lets the INDEX name carry a
# schema, but never the table: `CREATE INDEX ind ON main.tab (a)` is
# `near ".": syntax error`. Under this package's SQLite arm the value arriving
# is already the bare table name, so the method simply does not re-qualify it.
#
# `IF NOT EXISTS` makes re-adding an existing index a no-op, and, crucially,
# leaves `PRAGMA schema_version` untouched when it does nothing.
#
# The comment block is deliberately plain `#` rather than roxygen `#'`:
# roxygen2 cannot name an S7 method registered against an S4 class.
S7::method(add_index, db_sqlite) <- function(connection, table, index, keys) {
  keys_quoted <- paste0(
    DBI::dbQuoteIdentifier(connection, keys),
    collapse = ", "
  )

  return(DBI::dbExecute(
    connection,
    paste0(
      "CREATE INDEX IF NOT EXISTS ",
      DBI::dbQuoteIdentifier(connection, index),
      " ON ",
      DBI::dbQuoteIdentifier(connection, table),
      " (",
      keys_quoted,
      ")"
    )
  ))
}

# ensure_index: create one index on an existing table when its name is absent
#
# DBTable_v9 created the declared indexes only when it created the table. A
# table that existed already never got them. The 106 partitions of
# anon_norsyss_data_staging on norsyss_data1 were the case that needed this.
#
# Every method looks for the PHYSICAL name on THIS table, and creates the
# index only when the name is absent. It never drops and never renames. An
# index under an old name, for example a bare `ind1` from before 2026.10.4,
# is a different name, so the table then holds both.
#
# Every method returns one of three values:
#
#   TRUE   The catalogue had no such index, and the method sent the create.
#   FALSE  The catalogue had the index. The method sent no DDL.
#   NA     The method cannot tell. It sent a statement that does its own
#          check on the server, or it did nothing at all.
#
# The comment block is deliberately plain `#` rather than roxygen `#'`:
# roxygen2 cannot name an S7 method registered against an S4 class.
ensure_index <- S7::new_generic("ensure_index", "connection")

# MySQL and every other backend: csdb checks nothing and creates nothing.
S7::method(ensure_index, db_default) <- function(
  connection,
  table,
  index,
  keys
) {
  return(NA)
}

# SQLite: get_index_columns() reads sqlite_master by name AND by table, then
# add_index() sends `CREATE INDEX IF NOT EXISTS`.
S7::method(ensure_index, db_sqlite) <- function(
  connection,
  table,
  index,
  keys
) {
  columns <- get_index_columns(
    connection = connection,
    table = table,
    index = index
  )
  if (length(columns) > 0L) {
    return(FALSE)
  }
  add_index(connection = connection, table = table, index = index, keys = keys)
  return(TRUE)
}

# PostgreSQL: read pg_indexes by index name, table name and schema, then send
# `CREATE INDEX IF NOT EXISTS` only when the row is absent. A present index
# therefore costs one SELECT and no DDL.
#
# The create is the db_postgres add_index method, named explicitly. The method
# then sends PostgreSQL SQL whatever connection class it receives, and a test
# can read that SQL from a DBI::ANSI() connection.
S7::method(ensure_index, db_postgres) <- function(
  connection,
  table,
  index,
  keys
) {
  parts <- index_table_identity(table)
  sql <- "select indexname from pg_indexes where indexname = ? and tablename = ?"
  params <- list(index, parts[length(parts)])
  if (length(parts) > 1L) {
    sql <- paste0(sql, " and schemaname = ?")
    params <- c(params, list(parts[length(parts) - 1L]))
  }
  if (nrow(DBI::dbGetQuery(connection, sql, params = params)) > 0L) {
    return(FALSE)
  }
  S7::method(add_index, db_postgres)(
    connection = connection,
    table = table,
    index = index,
    keys = keys
  )
  return(TRUE)
}

# SQL Server: one statement. SQL Server has no `CREATE INDEX IF NOT EXISTS`,
# so the statement reads sys.indexes by the index name and the object id of
# the table, and creates the index only when that row is absent. The result
# of the check stays on the server, so the method returns NA.
#
# This statement was not run against a SQL Server. A test reads its text.
S7::method(ensure_index, db_mssql) <- function(connection, table, index, keys) {
  table_quoted <- quote_table_identity(connection, table)
  sql <- paste0(
    "IF NOT EXISTS (SELECT 1 FROM sys.indexes WHERE name = N",
    DBI::dbQuoteString(connection, index),
    " AND object_id = OBJECT_ID(N",
    DBI::dbQuoteString(connection, table_quoted),
    ")) CREATE INDEX ",
    DBI::dbQuoteIdentifier(connection, index),
    " ON ",
    table_quoted,
    " (",
    paste0(DBI::dbQuoteIdentifier(connection, keys), collapse = ", "),
    ");"
  )
  DBI::dbExecute(connection, sql)
  return(NA)
}

# Create every declared index that is missing on an existing table.
#
# DBTable_v9$create_table() calls this when the table exists and its fields
# match. The private flag declared_indexes_ensured holds that to one run per
# object per R process, whatever called create_table().
#
# A failure gives a warning and does not raise. An index is a performance
# property and not a correctness one, so a read or a write MUST NOT stop
# because an index could not be created. The warning names the table, the
# logical name and the physical name.
#
# After a create, the catalogue is read back where the backend has a reader.
# `IF NOT EXISTS` reports success when another table holds the name, and the
# read-back is what catches that.
#
# connection  The DBI connection.
# table       The table identity, the DBI::Id field
#             table_name_short_for_mssql_fully_specified_for_postgres.
# indexes     The declared indexes: a named list of column vectors.
# table_name  The table name, for messages.
# returns     NULL, invisibly.
ensure_declared_indexes <- function(connection, table, indexes, table_name) {
  for (i in names(indexes)) {
    physical <- index_physical_name(table = table, index = i)
    tryCatch(
      {
        created <- ensure_index(
          connection = connection,
          table = table,
          index = physical,
          keys = indexes[[i]]
        )
        if (isTRUE(created)) {
          message(glue::glue("Added missing index {i} to table {table_name}"))
          columns <- get_index_columns(
            connection = connection,
            table = table,
            index = physical
          )
          if (!is.null(columns) && length(columns) == 0L) {
            stop(
              "the catalogue holds no such index on this table after the create",
              call. = FALSE
            )
          }
        }
      },
      error = function(e) {
        warning(
          glue::glue(
            "Index {i} was not created on table {table_name}. ",
            "Its name there is {physical}. {conditionMessage(e)}"
          ),
          call. = FALSE
        )
        return(invisible(NULL))
      }
    )
  }
  return(invisible(NULL))
}

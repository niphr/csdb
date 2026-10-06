# A password that holds `;`, `{`, `}`, `=` or a quote reaches the ODBC
# driver unchanged.
#
# odbc pastes the arguments of DBI::dbConnect() into one connection string of
# `key=value` pairs, separated by `;`. odbc 1.7.0 builds it with
# paste(names(args), args, sep = "=", collapse = ";"). A raw `;` ends the
# password, and the rest reads as more pairs. odbc_connect_args() puts the
# password in braces and doubles every `}` inside it, which is the ODBC rule.
#
# The blocks below build the string as odbc does, and read it back with the
# ODBC rule. No database is needed.

odbc_hostile_password <- "a;b}c{d=e\"f'g"

# Read an ODBC connection string back into a named list.
#
# A value that starts with `{` runs to the first `}` that is not doubled, and
# `}}` inside it reads as `}`. Any other value runs to the next `;`.
parse_odbc_string <- function(s) {
  chars <- strsplit(s, "", fixed = TRUE)[[1]]
  n <- length(chars)
  out <- list()
  i <- 1L
  while (i <= n) {
    eq <- i
    while (eq <= n && chars[eq] != "=") {
      eq <- eq + 1L
    }
    key <- paste0(chars[seq_len(eq - i) + i - 1L], collapse = "")
    j <- eq + 1L
    value <- character(0)
    if (j <= n && chars[j] == "{") {
      j <- j + 1L
      repeat {
        if (j > n) {
          stop("unterminated braced value for ", key)
        }
        if (chars[j] == "}") {
          if (j < n && chars[j + 1L] == "}") {
            value <- c(value, "}")
            j <- j + 2L
            next
          }
          j <- j + 1L
          break
        }
        value <- c(value, chars[j])
        j <- j + 1L
      }
      if (j <= n && chars[j] != ";") {
        stop("text after the closing brace of ", key)
      }
    } else {
      while (j <= n && chars[j] != ";") {
        value <- c(value, chars[j])
        j <- j + 1L
      }
    }
    out[[key]] <- paste0(value, collapse = "")
    i <- j + 1L
  }
  return(out)
}

# The string that odbc 1.7.0 builds from the arguments.
odbc_string <- function(args) {
  return(paste(names(args), args, sep = "=", collapse = ";"))
}

odbc_config <- function(driver, sslmode = "x", trusted_connection = "x") {
  return(list(
    driver = driver,
    server = "h",
    port = 5432L,
    db = "d",
    schema = "s",
    user = "u",
    password = odbc_hostile_password,
    trusted_connection = trusted_connection,
    sslmode = sslmode,
    role_create_table = "x"
  ))
}

test_that("a hostile password round-trips through every ODBC branch", {
  cases <- list(
    list(cfg = odbc_config("PostgreSQL Unicode"), key = "password"),
    list(
      cfg = odbc_config("PostgreSQL Unicode", sslmode = "require"),
      key = "password"
    ),
    list(cfg = odbc_config("ODBC Driver 17 for SQL Server"), key = "pwd"),
    list(cfg = odbc_config("Some Other Driver"), key = "password")
  )
  for (case in cases) {
    args <- odbc_connect_args(case$cfg)
    parsed <- parse_odbc_string(odbc_string(args))
    # The string holds exactly the pairs that were passed, so no part of the
    # password became a pair of its own.
    expect_identical(names(parsed), names(args))
    expect_identical(parsed[[case$key]], odbc_hostile_password)
    # odbc reads an AsIs value as already quoted, and prints no message.
    expect_s3_class(args[[case$key]], "AsIs")
  }
})

test_that("odbc_quote_value() applies the ODBC brace rule", {
  expect_identical(
    unclass(odbc_quote_value(odbc_hostile_password)),
    "{a;b}}c{d=e\"f'g}"
  )
  expect_identical(unclass(odbc_quote_value("plain")), "{plain}")
  expect_identical(unclass(odbc_quote_value("}{}")), "{}}{}}}")
  expect_null(odbc_quote_value(NULL))
})

test_that("each ODBC branch passes the arguments it always did", {
  pg <- odbc_connect_args(odbc_config("PostgreSQL Unicode"))
  expect_identical(
    names(pg),
    c("driver", "server", "port", "uid", "password", "database")
  )
  expect_identical(pg$uid, "u")

  pg_ssl <- odbc_connect_args(
    odbc_config("PostgreSQL Unicode", sslmode = "require")
  )
  expect_identical(
    names(pg_ssl),
    c("driver", "server", "port", "uid", "password", "database", "sslmode")
  )

  ms <- odbc_connect_args(odbc_config("ODBC Driver 17 for SQL Server"))
  expect_identical(
    names(ms),
    c("driver", "server", "port", "uid", "pwd", "encoding")
  )

  ms_trusted <- odbc_connect_args(
    odbc_config("ODBC Driver 17 for SQL Server", trusted_connection = "yes")
  )
  expect_identical(
    names(ms_trusted),
    c("driver", "server", "port", "trusted_connection")
  )

  other <- odbc_connect_args(odbc_config("Some Other Driver"))
  expect_identical(
    names(other),
    c("driver", "server", "port", "user", "password", "encoding")
  )

  # A NULL password stays NULL, as DBI::dbConnect() received it before.
  cfg <- odbc_config("PostgreSQL Unicode")
  cfg["password"] <- list(NULL)
  expect_null(odbc_connect_args(cfg)$password)
})

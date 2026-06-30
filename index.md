Features

01

### Connection management

`DBConnection_v9` wraps an ODBC connection with lazy connect, explicit
disconnect, and automatic reconnect via `$autoconnection`.

02

### Bulk table operations

`DBTable_v9` handles insert, upsert, and key-based row deletion against
PostgreSQL and SQL Server tables, with configurable indexes.

03

### Field validation

Pluggable validators check field types and field contents before data
reaches the database, with built-in schemas for the csverse
`csfmt_rts_data` format.

## Overview

[csdb](https://niphr.github.io/csdb/) provides an abstracted system for
easily working with databases with large datasets.

Read the introduction vignette
[here](https://niphr.github.io/csdb/articles/csdb.html) or run
[`help(package="csdb")`](https://niphr.github.io/csdb/reference).

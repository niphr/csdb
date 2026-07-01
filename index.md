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

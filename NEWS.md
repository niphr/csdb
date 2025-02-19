# Version 2024.10.25

- `role_create_table` is now included for dbconnection_v9/dbtable_v9, so that the role can be changed when creating tables in PostgreSQL.

# Version 2024.3.27

- csdb now supports PostgreSQL databases as well as MS SQL Server.

# Version 2024.3.11

- Including use_count as an argument in nrow in DBTable_v9, which is slower but more accurate.

# Version 2024.3.7

- Including confirm_insert_via_nrow in DBTable_v9. Checks nrow() before insert and after insert. If nrow() has not increased sufficiently, then attempt an upsert.


# Version 2023.12.28

- Including validator_field_types_csfmt_rts_data_v2 and validator_field_contents_csfmt_rts_data_v2.

# Version 2023.12.26

- Including georegion in validator_field_contents_csfmt_rts_data_v1.

# Version 2023.4.14

- `get_table_names_and_info` is now ordered according to `table_name`.

# Version 2023.4.12

- `get_table_names_and_nrow` is now changed to `get_table_names_and_info` and also includes size_total_gb, size_data_gb, size_index_gb.
- `info` is now included as a method for `DBTable_v9` 

# Version 2023.4.4

- `confirm_indexes` is now added to `DBTable_v9`, which confirms that the names and number of indexes in the database are the same as in the R code. It does not confirm the contents of the indexes!
- `nrow` is now added to `DBTable_v9`, which is an application of the new `get_table_names_and_nrow` function.
- `get_table_names_and_nrow` added as an exported function, that will get all the table names and the nrows from a dbconnection.

# Version 2023.4.2

- `create_table` now automatically adds the indexes.

# Version 2023.3.31

- Removing info messages from `drop_rows_where`.

# Version 2023.3.8

- connect() in DBConnection_v9 is smarter, more robust with error checking and making fewer useless calls to the db. Tries to connect twice now before throwing an error.
- autoconnection is now more robust in DBConnection_v9.

# Version 2023.2.17

- Package is created.

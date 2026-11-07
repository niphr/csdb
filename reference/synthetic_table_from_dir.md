# Build a SyntheticTable from a directory

Reads `profile.yaml` from `dir` when it exists, and builds a
[SyntheticTable](https://niphr.github.io/csdb/reference/SyntheticTable.md)
whose datasets live in `dir`.

## Usage

``` r
synthetic_table_from_dir(dir, name, field_types, keys = NULL, ...)
```

## Arguments

- dir:

  The directory of the table. It holds `profile.yaml` (optional),
  `datasets.yaml` and `handmade/<name>.csv`.

- name:

  The name of the table.

- field_types:

  A named character vector of csdb field types.

- keys:

  A character vector of key column names, or `NULL`.

- ...:

  Other arguments to `SyntheticTable$new()`, such as
  `validator_field_contents`, `invariants` or `generator`. Do not give
  `profile` or `dir`.

## Value

A
[SyntheticTable](https://niphr.github.io/csdb/reference/SyntheticTable.md).

## Examples

``` r
dir <- tempfile()
dir.create(dir)
st <- synthetic_table_from_dir(dir, "people", c(id = "INTEGER"), "id")
st$list_datasets()
#> Empty data.table (0 rows and 5 cols): name,kind,n,seed,description
```

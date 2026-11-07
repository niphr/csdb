# The synthetic data contract of one table

A `SyntheticTable` holds the contract of one table and its fixed
synthetic datasets. Every data.table that a `SyntheticTable` returns
passed `$validate()`.

A generator is a `function(profile, n, seed, world)` that returns a
data.table. `generator = NULL` uses
[`simulate_from_profile()`](https://niphr.github.io/csdb/reference/simulate_from_profile.md)
on `profile`, or on
[`profile_schema()`](https://niphr.github.io/csdb/reference/profile_schema.md)
of `field_types` and `keys` when `profile` is `NULL`. `n = NULL` means
the generator's own size. A generator MAY then choose its row count, for
example a complete grid of series and weeks.
[`simulate_from_profile()`](https://niphr.github.io/csdb/reference/simulate_from_profile.md)
gives 1000 rows for `n = NULL`.

`$validate(d, world)` checks `d` in this order, and stops at the first
check that fails. The error names the check.

1.  `columns`: `d` is a data.table, and its column names and order equal
    `names(field_types)`.

2.  `types`: each column has the R type of its field type. These are
    `TEXT` character, `INTEGER` integer, `DOUBLE` double, `BOOLEAN`
    logical, `DATE` Date and `DATETIME` POSIXct.

3.  `keys`: no two rows have the same values of `keys`.

4.  `validator_field_contents`: `validator_field_contents(d)` is `TRUE`.

5.  `invariant '<name>'`: each invariant returns `TRUE`. A character
    return is the error message.

An invariant is a `function(d)` or a `function(d, world)`. The class
calls an invariant with two or more formal arguments as `f(d, world)`,
and any other invariant as `f(d)`. `$random()`, `$dataset()` and
`$add_dataset()` give the invariants their `world`.

The directory `dir` holds the datasets of the table:

- `datasets.yaml`: one entry for each recipe, with `name`, `kind`, `n`,
  `seed`, `description`, `overrides`, `world_md5`, `rows` and `md5`. `n`
  is empty for `n = NULL`. `world_md5` is absent for `world = NULL`.
  `rows` is the row count of the data.

- `handmade/<name>.csv`: one handmade dataset. Each column is read as
  text and converted to its field type. An empty cell is `NA`.

- `profile.yaml`: optional.
  [`synthetic_table_from_dir()`](https://niphr.github.io/csdb/reference/synthetic_table_from_dir.md)
  reads it. `$new()` does not read it.

A dataset name is lower snake case. The name `default` is the generator
output at `seed = 1` and `n = 1000` or `n = NULL`.

The md5 of a recipe comes from
[`tools::md5sum()`](https://rdrr.io/r/tools/md5sum.html) of the data
that
[`data.table::fwrite()`](https://rdrr.io/pkg/data.table/man/fwrite.html)
writes with `eol = "\n"`. The columns are in `field_types` order, the
rows are sorted by every column, dates are ISO 8601 and `NA` is empty.

The md5 of a world comes from
[`tools::md5sum()`](https://rdrr.io/r/tools/md5sum.html) of the text
that [`yaml::as.yaml()`](https://yaml.r-lib.org/reference/as.yaml.html)
gives for the world, written with LF line ends. The names are sorted in
C-locale order. Each value is text: a date or a datetime in ISO 8601,
any other value by
[`as.character()`](https://rdrr.io/r/base/character.html). The order of
the values within a name counts. Every element of the world counts, also
an element that names no column of the table.

`$dataset(name, world)` regenerates a recipe with `world`. When the md5
of `world` equals `world_md5`, it checks the data against the recipe. It
stops with an error that names the dataset when the row count differs
from `rows`, or the md5 differs from `md5`. For any other `world`, the
recipe is validated, its md5 is not checked, and a message says so. Two
`NULL` worlds are equal. A recipe without `rows` has no row check.

## Active bindings

- `name`:

  The name of the table.

- `field_types`:

  The field types of the table.

- `keys`:

  The key columns of the table.

- `dir`:

  The directory of the datasets, or `NULL`.

## Methods

### Public methods

- [`SyntheticTable$new()`](#method-SyntheticTable-initialize)

- [`SyntheticTable$validate()`](#method-SyntheticTable-validate)

- [`SyntheticTable$random()`](#method-SyntheticTable-random)

- [`SyntheticTable$dataset()`](#method-SyntheticTable-dataset)

- [`SyntheticTable$list_datasets()`](#method-SyntheticTable-list_datasets)

- [`SyntheticTable$add_dataset()`](#method-SyntheticTable-add_dataset)

- [`SyntheticTable$check_hashes()`](#method-SyntheticTable-check_hashes)

- [`SyntheticTable$clone()`](#method-SyntheticTable-clone)

------------------------------------------------------------------------

### `SyntheticTable$new()`

Create a new SyntheticTable.

#### Usage

    SyntheticTable$new(
      name,
      field_types,
      keys = NULL,
      validator_field_contents = validator_field_contents_blank,
      invariants = list(),
      profile = NULL,
      generator = NULL,
      dir = NULL
    )

#### Arguments

- `name`:

  The name of the table.

- `field_types`:

  A named character vector of csdb field types.

- `keys`:

  A character vector of key column names, or `NULL`.

- `validator_field_contents`:

  A function of a data.table that returns `TRUE` for valid contents.

- `invariants`:

  A named list of `function(d)` or `function(d, world)`. Each returns
  `TRUE`, or a message that says why `d` fails.

- `profile`:

  A `csdb_profile`, or `NULL`.

- `generator`:

  A `function(profile, n, seed, world)` that returns a data.table, or
  `NULL` for
  [`simulate_from_profile()`](https://niphr.github.io/csdb/reference/simulate_from_profile.md).

- `dir`:

  The directory of the datasets, or `NULL` for no datasets.

------------------------------------------------------------------------

### `SyntheticTable$validate()`

Check a data.table against the contract.

#### Usage

    SyntheticTable$validate(d, world = NULL)

#### Arguments

- `d`:

  A data.table.

- `world`:

  `NULL`, or the list that each `function(d, world)` invariant receives.

#### Returns

`d`, invisibly. The method stops at the first check that fails, and the
error names it.

------------------------------------------------------------------------

### `SyntheticTable$random()`

Generate and validate new data.

#### Usage

    SyntheticTable$random(n = 1000L, seed = NULL, world = NULL)

#### Arguments

- `n`:

  The number of rows, or `NULL` for the generator's own size.

- `seed`:

  The seed. `NULL` draws a seed and reports it in a message, so you can
  replay a failure.

- `world`:

  `NULL`, or a list that the generator receives.

#### Returns

A validated data.table.

------------------------------------------------------------------------

### `SyntheticTable$dataset()`

Get a fixed dataset by name.

#### Usage

    SyntheticTable$dataset(name, world = NULL)

#### Arguments

- `name`:

  The name of the dataset.

- `world`:

  `NULL`, or a list that the generator receives. The md5 check runs only
  when the md5 of `world` equals the `world_md5` of the recipe.

#### Returns

A validated data.table.

------------------------------------------------------------------------

### `SyntheticTable$list_datasets()`

List the fixed datasets.

#### Usage

    SyntheticTable$list_datasets()

#### Returns

A data.table with `name`, `kind` (`recipe` or `handmade`), `n`, `seed`
and `description`. A handmade dataset has `NA` in `n`, `seed` and
`description`.

------------------------------------------------------------------------

### `SyntheticTable$add_dataset()`

Add a recipe, its row count and its md5 to `datasets.yaml`.

#### Usage

    SyntheticTable$add_dataset(
      name,
      n,
      seed,
      description,
      overrides = list(),
      world = NULL
    )

#### Arguments

- `name`:

  The name of the dataset, lower snake case and new.

- `n`:

  The number of rows, or `NULL` for the generator's own size.

- `seed`:

  The seed.

- `description`:

  One string that says what the dataset is for.

- `overrides`:

  A named list that maps a column to one fixed value. The value replaces
  the column after generation.

- `world`:

  `NULL`, or a list that the generator receives. The recipe stores its
  md5 as `world_md5`.

#### Returns

The validated data.table, invisibly.

------------------------------------------------------------------------

### `SyntheticTable$check_hashes()`

Regenerate each recipe that `world` made, and compare its row count and
its md5.

#### Usage

    SyntheticTable$check_hashes(world = NULL)

#### Arguments

- `world`:

  `NULL`, or the world of the recipes to check.

#### Returns

A data.table with `name` and `pass`. `pass` is `NA` for a recipe that
another world made.

------------------------------------------------------------------------

### `SyntheticTable$clone()`

The objects of this class are cloneable with this method.

#### Usage

    SyntheticTable$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
st <- SyntheticTable$new(
  name = "people",
  field_types = c(id = "INTEGER", sex = "TEXT"),
  keys = "id",
  invariants = list(
    sex_known = function(d) if (all(!is.na(d$sex))) TRUE else "sex is missing."
  )
)
st$random(n = 5, seed = 1)
```

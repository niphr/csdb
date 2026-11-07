# Synthetic data from a profile

csdb makes synthetic data for one table in two steps. A profile
summarises the real table with aggregate SQL, under disclosure control.
A simulator then draws new rows from the profile. A `SyntheticTable`
holds the contract of the table and a catalogue of fixed datasets, so
that a test can name the data it uses.

This vignette works at the level of one table. Two other vignettes build
on it:

- cs9,
  [`vignette("synthetic-data", package = "cs9")`](https://niphr.github.io/cs9/articles/synthetic-data.html):
  synthetic tables, synthetic mode and switchable sources in a
  surveillance system.
- norsyss.cs9, `vignettes/synthetic-development.Rmd` in the private
  repository `niphr/norsyss.cs9`: one late task of a real pipeline, run
  on synthetic inputs alone.

Every chunk here runs on SQLite.

## A table to profile

The table holds 600 made-up consultations. Each row is one visit of one
person.

``` r
library(csdb)
#> csdb 2026.11.7
#> https://niphr.github.io/csdb/
library(data.table)
#> 
#> Attaching package: 'data.table'
#> The following object is masked from 'package:base':
#> 
#>     %notin%

set.seed(1)
visits <- data.table(
  id = sample(1:150, 600, replace = TRUE),
  date = as.Date("2024-01-01") + sample(0:120, 600, replace = TRUE),
  sex = sample(c("female", "male"), 600, replace = TRUE),
  age = sample(0:95, 600, replace = TRUE),
  consultations = rpois(600, 3),
  municipality = sample(sprintf("m%03d", 1:40), 600, replace = TRUE)
)
visits[1:3, date := as.Date("2024-09-30")]

tab <- DBTable_v9$new(
  dbconfig = list(driver = "SQLite", db = tempfile(fileext = ".sqlite")),
  table_name = "visits",
  field_types = c(
    id = "INTEGER",
    date = "DATE",
    sex = "TEXT",
    age = "INTEGER",
    consultations = "INTEGER",
    municipality = "TEXT"
  ),
  keys = NULL,
  validator_field_types = validator_field_types_blank,
  validator_field_contents = validator_field_contents_blank
)
invisible(tab$create_table())
#> Creating table visits
tab$insert_data(visits)
```

Three rows move to 2024-09-30, a week with no other visit. Disclosure
control has to deal with them below.

## Sensitivity levels

Each column gets one of four sensitivity levels. A column that `columns`
does not name is `schema_only`.

| Level | What the profile publishes |
|----|----|
| `full` | the values and their counts; above `max_levels`, bins for a number and `n_distinct` for text |
| `coarsen` | a date by ISO week, a number by bins |
| `schema_only` | the type and the share of missing values |
| `exclude` | nothing beyond the field type |

[`profile_table()`](https://niphr.github.io/csdb/reference/profile_table.md)
reads the query, the field types and the keys from the table.
[`profile_query()`](https://niphr.github.io/csdb/reference/profile_query.md)
does the same for any SQL query.

``` r
p <- profile_table(
  tab,
  columns = c(
    id = "exclude",
    date = "full",
    sex = "full",
    age = "coarsen",
    consultations = "full"
  ),
  k = 5,
  max_levels = 10,
  group_sizes = list(rows_per_id = "id")
)
p$columns$sex
#> $type
#> [1] "TEXT"
#> 
#> $sensitivity
#> [1] "full"
#> 
#> $missing_share
#> [1] 0
#> 
#> $levels
#> [1] "female" "male"  
#> 
#> $n
#> [1] 290 310
p$columns$municipality
#> $type
#> [1] "TEXT"
#> 
#> $sensitivity
#> [1] "schema_only"
#> 
#> $missing_share
#> [1] 0
```

`municipality` is `schema_only`, because `columns` does not name it. The
profile holds no value of it.

## Suppression by merge

The profile never writes a count below `k`. On an ordered dimension, a
cell below `k` merges with its neighbours instead of being removed:

- Days merge into ISO weeks, then calendar months, then calendar
  quarters. The column takes the finest resolution at which every cell
  holds `k`.
- Bins of a number merge into runs of adjacent bins. A merged bin keeps
  the outer breaks of its run.
- Group sizes merge into ranges of row counts. No exact smallest or
  largest row count is published.

The three late visits are the only visits in their week, their month and
their quarter. Each of those cells holds 3, below `k = 5`. So `date`
comes out by quarter, and the third quarter joins the second in one run:

``` r
names(p$columns$date)
#> [1] "type"          "sensitivity"   "missing_share" "quarters"     
#> [5] "quarters_to"   "n"
data.table(
  from = p$columns$date$quarters,
  to = p$columns$date$quarters_to,
  n = p$columns$date$n
)
#>          from         to     n
#>        <Date>     <Date> <int>
#> 1: 2024-01-01 2024-01-01   456
#> 2: 2024-04-01 2024-07-01   144
```

`consultations` has more than `max_levels` distinct values, so it is
published as bins. A number column also records `nonnegative`, which is
`TRUE` when its minimum is at least 0. The simulator then draws no value
below 0.

``` r
p$columns$consultations$breaks
#> [1] -0.5  0.5  1.5  2.5  3.5  4.5  5.5  6.5 10.5
p$columns$consultations$nonnegative
#> [1] TRUE
p$group_sizes$rows_per_id
#> $column
#> [1] "id"
#> 
#> $size_from
#> [1] 1 3 4 5 6 7 8
#> 
#> $size_to
#> [1] 2 3 4 5 6 7
#> 
#> $n
#> [1] 33 23 30 27 20  7  6
```

An unordered cell, such as a level of a `TEXT` column, cannot merge. A
cell below `k` is removed, and the removed counts are summed into
`suppressed_n` when that sum is at least `k`.

## The profile refuses a torn read

[`profile_query()`](https://niphr.github.io/csdb/reference/profile_query.md)
reads the data with several aggregate queries, in one read snapshot
where the driver gives one. It then checks that the counts of each
column, plus its missing values, add up to `n_rows`. A profile that
fails the check is torn, and the function refuses it.

A concurrent write without a snapshot tears a profile. This chunk
simulates one. It mocks the internal reader of the missing counts, so
that 10 rows are inserted through the same connection just after that
read. Every later read then sees 610 rows, and `n_rows` is 600:

``` r
con <- tab$dbconnection$autoconnection
real_read <- csdb:::sql_missing_counts
torn <- testthat::with_mocked_bindings(
  sql_missing_counts = function(con, sql, columns) {
    out <- real_read(con, sql, columns)
    DBI::dbExecute(con, "INSERT INTO visits SELECT * FROM visits LIMIT 10")
    return(out)
  },
  tryCatch(
    profile_query(
      con,
      "SELECT * FROM visits",
      columns = c(sex = "full"),
      field_types = c(sex = "TEXT")
    ),
    error = function(e) conditionMessage(e)
  ),
  .package = "csdb"
)
stopifnot(grepl("Torn profile", torn))
cat(torn, "
")
#> Torn profile: the counts of column sex sum to 610 (610 in cells and 0 missing), but n_rows is 600. The data changed while profile_query() read it.
DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM visits")$n
#> [1] 600
```

The function ends its transaction with a rollback, so the 10 inserted
rows are gone again.

## Write and read the profile

A profile is a YAML file.
[`read_profile()`](https://niphr.github.io/csdb/reference/read_profile.md)
gives back an identical object.

``` r
f <- write_profile(p, tempfile(fileext = ".yaml"))
cat(readLines(f)[1:16], sep = "\n")
#> provenance:
#>   format: 1
#>   source: ~
#>   query: |-
#>     SELECT *
#>     FROM `visits`
#>   driver: SQLiteConnection
#>   profiled_at: '2026-11-07T16:39:33Z'
#>   k: 5
#>   noise: ~
#>   max_levels: 10
#>   csdb_version: 2026.11.7
#> field_types:
#>   id: INTEGER
#>   date: DATE
#>   sex: TEXT
identical(read_profile(f), p)
#> [1] TRUE
```

The provenance names the query, the driver, `k`, `noise`, `max_levels`
and the csdb version. It holds no user name.

## What a profile may contain

A profile of real data is a release of aggregates. It MUST have the
approval of the data owner before it leaves the restricted environment.
That holds for a profile in a package directory, in a test fixture and
in a git repository.

Suppression keeps every published count at `k` or above. It does not
decide whether the set of aggregates is safe to publish. That decision
is the data owner’s. With `noise = list(epsilon = e)`, the profile adds
Laplace noise of scale `1 / e` to each count before suppression.

## Simulate and compare

[`simulate_from_profile()`](https://niphr.github.io/csdb/reference/simulate_from_profile.md)
draws new rows from the published summaries. It reads the profile only,
never the profiled rows.

``` r
sim <- simulate_from_profile(p, n = 600, seed = 1)
head(sim)
#>       id       date    sex   age consultations    municipality
#>    <int>     <Date> <char> <int>         <int>          <char>
#> 1:     1 2024-03-20 female    62             3  municipality_3
#> 2:     1 2024-03-02 female    20             4 municipality_10
#> 3:     2 2024-02-17 female    30             6  municipality_3
#> 4:     3 2024-02-20   male    50             5  municipality_7
#> 5:     3 2024-01-23   male    79             1  municipality_9
#> 6:     3 2024-04-02   male    34             4  municipality_9
```

[`profile_compare()`](https://niphr.github.io/csdb/reference/profile_compare.md)
checks how far the simulation is from the real profile. Profile the
simulation with the same `columns`, `k` and `max_levels` first:

``` r
s <- profile_data(
  sim,
  field_types = p$field_types,
  columns = c(
    id = "exclude",
    date = "full",
    sex = "full",
    age = "coarsen",
    consultations = "full"
  ),
  k = 5,
  max_levels = 10,
  group_sizes = list(rows_per_id = "id")
)
profile_compare(p, s)
#>            column      property    distance  threshold   pass
#>            <char>        <char>       <num>      <num> <lgcl>
#>  1:          date missing_share 0.000000000 0.05000000   TRUE
#>  2:          date      quarters 0.010000000 0.08666667   TRUE
#>  3:           sex missing_share 0.000000000 0.05000000   TRUE
#>  4:           sex        levels 0.003333333 0.09166667   TRUE
#>  5:           age missing_share 0.000000000 0.05000000   TRUE
#>  6:           age          bins 0.053333333 0.14008333   TRUE
#>  7: consultations missing_share 0.000000000 0.05000000   TRUE
#>  8: consultations          bins 0.063333333 0.10833333   TRUE
#>  9:  municipality missing_share 0.000000000 0.05000000   TRUE
#> 10:            id   group_sizes 0.067543246 0.17045045   TRUE
```

Each row has its own rule for the threshold:

- A table of counts, such as `levels`, `quarters`, `bins` or
  `group_sizes`: the function draws `n_boot` multinomial tables of the
  synthetic size from the real shares. It merges each draw with the `k`
  of `real`. The threshold is the 95th percentile of their distances,
  plus `margin`.
- `missing_share`: `n_boot` binomial draws of `synth$n_rows` rows at the
  real share. The threshold is the 95th percentile of their distances,
  plus `margin`. A share of `"suppressed"` on either side gives no row.
- `n_distinct`: the distance is 0 inside the range that `synth$n_rows`
  uniform draws from the real values can reach. The threshold is
  `margin`.
- `dependency`: the distance is 0 when both profiles agree on `holds`,
  otherwise 1. The threshold is 0.
- A summary of `real` that `synth` lacks, or holds in a form that does
  not align: distance 1, threshold `margin`.

For a table of counts, a smaller simulation gets a wider threshold. No
draw models the noise that `noise` adds to a profile.

## A SyntheticTable

A `SyntheticTable` holds the contract of one table: its field types, its
keys, `validator_field_contents` and named invariants. Every data.table
that it returns passed `$validate()`.

An invariant is a `function(d)` or a `function(d, world)`. It returns
`TRUE`, or a message that says why `d` fails.

``` r
dir <- tempfile("visits-")
dir.create(dir)
write_profile(p, file.path(dir, "profile.yaml"))

ft <- p$field_types
invariants <- list(
  age_nonnegative = function(d) {
    if (all(d$age >= 0, na.rm = TRUE)) TRUE else "age is negative."
  }
)
st <- synthetic_table_from_dir(
  dir,
  name = "visits",
  field_types = ft,
  invariants = invariants
)
print(st$random(n = 5, seed = 3))
#>       id       date    sex   age consultations    municipality
#>    <int>     <Date> <char> <int>         <int>          <char>
#> 1:     1 2024-02-15   male    77             2 municipality_10
#> 2:     1 2024-02-18 female    89             3  municipality_7
#> 3:     1 2024-02-20 female    87             4  municipality_3
#> 4:     1 2024-03-19   male    33             2  municipality_5
#> 5:     2 2024-03-16   male    56             2  municipality_7
```

`$random()` with `seed = NULL` draws a seed and reports it in a message,
so a failure can be replayed.

## Named datasets

A test SHOULD name the dataset it uses. `$add_dataset()` stores a recipe
in `datasets.yaml`. A recipe holds `n`, `seed`, `overrides`, the md5 of
the world as `world_md5`, the row count as `rows`, and the md5 of the
data. The name `default` is the generator output at `seed = 1` and
`n = 1000`. A generator that chooses its own size uses `n = NULL` there.

``` r
world <- list(sex = c("female", "male"))
st$add_dataset(
  "default",
  n = 1000,
  seed = 1,
  description = "The generator output at seed 1.",
  world = world
)
st$add_dataset(
  "all_female",
  n = 200,
  seed = 2,
  description = "Every row is female.",
  overrides = list(sex = "female"),
  world = world
)
cat(readLines(file.path(dir, "datasets.yaml")), sep = "\n")
#> - name: default
#>   kind: recipe
#>   'n': 1000
#>   seed: 1
#>   description: The generator output at seed 1.
#>   overrides: []
#>   world_md5: 33e42b7138d2f5c7ae76e6e5ff72ff42
#>   rows: 1000
#>   md5: 9fd7b6d59c58c5bbe02b75bb22dc347b
#> - name: all_female
#>   kind: recipe
#>   'n': 200
#>   seed: 2
#>   description: Every row is female.
#>   overrides:
#>     sex: female
#>   world_md5: 33e42b7138d2f5c7ae76e6e5ff72ff42
#>   rows: 200
#>   md5: 03a2fe75d8d608e93a7abeaa73426476
```

A handmade dataset is a CSV file in `handmade/`. Each column is read as
text and converted to its field type. An empty cell is `NA`.

``` r
dir.create(file.path(dir, "handmade"))
writeLines(
  c(
    "id,date,sex,age,consultations,municipality",
    "1,2024-01-01,female,40,2,m001",
    "2,2024-01-02,male,,1,m002"
  ),
  file.path(dir, "handmade", "two_visits.csv")
)
st$list_datasets()
#>          name     kind     n  seed                     description
#>        <char>   <char> <int> <int>                          <char>
#> 1:    default   recipe  1000     1 The generator output at seed 1.
#> 2: all_female   recipe   200     2            Every row is female.
#> 3: two_visits handmade    NA    NA                            <NA>
print(st$dataset("two_visits"))
#>       id       date    sex   age consultations municipality
#>    <int>     <Date> <char> <int>         <int>       <char>
#> 1:     1 2024-01-01 female    40             2         m001
#> 2:     2 2024-01-02   male    NA             1         m002
```

`$dataset()` regenerates a recipe. When the md5 of the world equals
`world_md5`, it checks the row count against `rows` and the md5 against
`md5`:

``` r
nrow(st$dataset("all_female", world = world))
#> [1] 200
st$check_hashes(world = world)
#>          name   pass
#>        <char> <lgcl>
#> 1:    default   TRUE
#> 2: all_female   TRUE
```

## The hash check after a generator change

A change of the generator changes the data of every recipe. The hash
check then stops, and the error names the table and the dataset. Here a
new generator draws 1 to 10 consultations in place of the profile:

``` r
changed <- function(profile, n, seed, world) {
  d <- simulate_from_profile(profile, n = n, seed = seed, world = world)
  d[, consultations := sample(1:10, .N, replace = TRUE)]
  return(d)
}
st2 <- synthetic_table_from_dir(
  dir,
  name = "visits",
  field_types = ft,
  invariants = invariants,
  generator = changed
)
st2$dataset("all_female", world = world)
#> Error:
#> ! SyntheticTable visits, dataset 'all_female': the regenerated data does not match the md5 in datasets.yaml. The generator, the profile or the overrides changed.
```

After a deliberate change, delete the recipe from `datasets.yaml` and
add it again with `$add_dataset()`. A test that reads the dataset then
sees the new data.

For a `world` other than the recipe’s, `$dataset()` validates the data,
does not check the md5, and says so in a message.

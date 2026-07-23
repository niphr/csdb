## Submission

Update to version 2026.5.13.

Bug fix: the PostgreSQL methods (`create_table`, `keep_rows_where`,
`drop_table`) now quote `role_create_table` via `DBI::dbQuoteIdentifier()` when
emitting `SET ROLE`. The role name was previously interpolated raw, which broke
on identifiers containing hyphens, mixed case or reserved words, and was an
injection vector where the value came from an environment variable.

## Test environments

* local Windows 11, R 4.4.2 (`R CMD check --no-manual --as-cran`)
* GitHub Actions, ubuntu-latest, R release (`--no-manual --as-cran`)

## R CMD check results

0 errors | 0 warnings | 0 notes

The local run additionally reports "unable to verify current time". That is the
offline clock check on a network without access to the time service, not a
package problem; it does not appear on CI.

## Downstream dependencies

None.

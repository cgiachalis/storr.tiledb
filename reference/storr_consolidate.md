# Consolidate Storr Fragments

Perform fragment consolidation for better query performance.

## Usage

``` r
storr_consolidate(uri, what = "all", vacuum = FALSE, async = FALSE,
  context = NULL)
```

## Arguments

- uri:

  The URI path of storr.

- what:

  Which array should be consolidated? Defaults to `"all"` arrays.

- vacuum:

  Should the old fragments (consolidated) be deleted? Default is
  `FALSE`.

- async:

  Should it consolidate asynchronously? Default is `FALSE`.

- context:

  Optional
  [tiledb_ctx](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_ctx.html)
  object.

## Value

When `async = FALSE`, it returns `TRUE` for successful consolidation.
For `async = TRUE`, it returns a
[`mirai::mirai()`](https://mirai.r-lib.org/reference/mirai.html) object
immediately; once resolved, it returns `TRUE` indicating consolidation
success.

## Details

The function supports selective consolidation ("keys"/"data") or full
("all"). The process can be run synchronously or asynchronously and has
an optional argument to delete old fragments afterwards (vacuum).

## See also

[`storr_vacuum()`](https://cgiachalis.github.io/storr.tiledb/reference/storr_vacuum.md)

## Examples

``` r
# URI path
uri <- tempfile()
sto <- storr_tiledb(uri, init = TRUE)

storr_consolidate(uri)
#> [1] TRUE
```

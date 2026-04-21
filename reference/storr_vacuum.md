# Vacuum Storr Fragments

Remove old consolidated fragments to free space.

## Usage

``` r
storr_vacuum(uri, what = "all", async = FALSE, context = NULL)
```

## Arguments

- uri:

  The URI path of storr.

- what:

  Which array to vacuum? Defaults to `"all"` arrays.

- async:

  Should it vacuum asynchronously? Default is `FALSE`.

- context:

  Optional
  [tiledb_ctx](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_ctx.html)
  object.

## Value

When `async = FALSE`, it returns `TRUE` for successful vacuuming. For
`async = TRUE`, it returns a
[`mirai::mirai()`](https://mirai.r-lib.org/reference/mirai.html) object
immediately; once resolved, it returns `TRUE` indicating vacuum success.

## Details

The function supports selective vacuuming ("keys"/"data") or full
("all") and it can be run synchronously or asynchronously.

## See also

[`storr_consolidate()`](https://cgiachalis.github.io/storr.tiledb/reference/storr_consolidate.md)

## Examples

``` r
# URI path
uri <- tempfile()
sto <- storr_tiledb(uri, init = TRUE)

storr_vacuum(uri)
#> [1] TRUE
```

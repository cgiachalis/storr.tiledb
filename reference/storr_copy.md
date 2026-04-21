# Copy Storr to another URI

Copy Storr to another URI

## Usage

``` r
storr_copy(uri, to_uri, context = NULL)
```

## Arguments

- uri:

  The URI path of storr.

- to_uri:

  Destination URI path to copy the storr to.

- context:

  Optional
  [tiledb_ctx](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_ctx.html)
  object.

## Value

The new uri path, invisibly.

## See also

Other storr-utilities:
[`storr_move()`](https://cgiachalis.github.io/storr.tiledb/reference/storr_move.md),
[`storr_rename()`](https://cgiachalis.github.io/storr.tiledb/reference/storr_rename.md)

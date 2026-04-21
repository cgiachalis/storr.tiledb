# Move Storr to another URI

Move Storr to another URI

## Usage

``` r
storr_move(uri, newuri, context = NULL)
```

## Arguments

- uri:

  The URI path of storr.

- newuri:

  Destination URI path to move the storr to.

- context:

  Optional
  [tiledb_ctx](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_ctx.html)
  object.

## Value

The new uri path, invisibly.

## See also

Other storr-utilities:
[`storr_copy()`](https://cgiachalis.github.io/storr.tiledb/reference/storr_copy.md),
[`storr_rename()`](https://cgiachalis.github.io/storr.tiledb/reference/storr_rename.md)

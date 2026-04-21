# Rename Storr URI

It renames the driver's basename, i.e., 'path/oldname' to
'path/newname'.

## Usage

``` r
storr_rename(uri, newname, context = NULL)
```

## Arguments

- uri:

  The URI path of storr.

- newname:

  Suffix to rename storr URI path.

- context:

  Optional
  [tiledb_ctx](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_ctx.html)
  object.

## Value

The new uri path, invisibly.

## See also

Other storr-utilities:
[`storr_copy()`](https://cgiachalis.github.io/storr.tiledb/reference/storr_copy.md),
[`storr_move()`](https://cgiachalis.github.io/storr.tiledb/reference/storr_move.md)

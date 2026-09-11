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

## Examples

``` r
uri <- tempfile()
sto <- storr_tiledb(uri, init = TRUE)

# set key-values
sto$set("a", 1)
sto$set("b", 2)

# Rename storr
newuri <- storr_rename(uri, newname = "new-storr")

sto2 <- storr_tiledb(newuri)

sto2$list()
#> [1] "a" "b"
```

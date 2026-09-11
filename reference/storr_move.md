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

## Examples

``` r
uri <- tempfile()
sto <- storr_tiledb(uri, init = TRUE)

# set key-values
sto$set("a", 1)
sto$set("b", 2)

# Move storr to new URI
to_uri <- tempfile()
storr_move(uri, newuri = to_uri)

sto2 <- storr_tiledb(to_uri)

sto2$list()
#> [1] "a" "b"
```

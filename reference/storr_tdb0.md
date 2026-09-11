# A Storr using TileDB Engine

A convenient wrapper for standard storr interface, e.g.,
`storr::storr(driver_tiledb())` with key difference that corrects for
serialization format if it is other than `R` native serialization.

## Usage

``` r
storr_tdb0(uri, default_namespace = "objects", context = NULL)
```

## Arguments

- uri:

  The URI path of storr.

- default_namespace:

  The default namespace: `"objects"`.

- context:

  Optional
  [tiledb_ctx](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_ctx.html)
  object.

## Value

A 'storr' object.

## Details

When the standard interface is needed with default object serialization,
then `storr::storr(driver_tiledb())` and `storr_tdb0()` are equivalent.
On the other hand, when the driver is configured with `qs2` or `qdata`
serialization format, the `storr_tdb0()` will be adjusted to use the
correct serialization function.

## See also

[`storr_tiledb()`](https://cgiachalis.github.io/storr.tiledb/reference/storr_tiledb.md)
and
[`driver_tiledb()`](https://cgiachalis.github.io/storr.tiledb/reference/driver_tiledb.md)

## Examples

``` r
uri <- tempfile()
driver_tiledb_create(uri)

# Instantiate standard 'storr'
sto <- storr_tdb0(uri)

sto$set("x", 1)

sto$get("x")
#> [1] 1
```

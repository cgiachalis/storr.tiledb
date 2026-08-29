# Storr Fragments

Functional interface that initialises a
[StorrFragments](https://cgiachalis.github.io/storr.tiledb/reference/StorrFragments.md)
instance to work with TileDB Fragments.

## Usage

``` r
storr_fragments(uri, context = NULL)
```

## Arguments

- uri:

  The URI path of storr.

- context:

  Optional
  [tiledb_ctx](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_ctx.html)
  object.

## Value

An object of class `StorrFragments`, `R6`.

## Details

The class includes fragment consolidation and vacuuming methods but also
provides access to
[TileDBFragments](https://cgiachalis.github.io/R6.tiledb/reference/TileDBFragments.html)
instances for `keys` and `data` arrays where you can further inspect and
manage the fragments for the specify arrays.

## Class Methods Summary

For complete definitions see the **Methods** section in
[StorrFragments](https://cgiachalis.github.io/storr.tiledb/reference/StorrFragments.md).

**Active Fields**

- **fob_keys** -
  [TileDBFragments](https://cgiachalis.github.io/R6.tiledb/reference/TileDBFragments.html)
  instance for the `keys` array.

- **fob_data** -
  [TileDBFragments](https://cgiachalis.github.io/R6.tiledb/reference/TileDBFragments.html)
  instance for the `data` array.

- **size** - VFS directory size for the TileDB store.

**Fragment Management**

- **consolidate()** - Consolidate fragments (options: `all`, `keys`,
  `data`); supports `vacuum` and sync/async modes.

- **vacuum()** - Remove old consolidated fragments (options: `all`,
  `keys`, `data`); supports sync/async.

- **reload_finfo()** - Refresh fragment-info caches for both arrays.

**Inspection**

- **frag_num()** - Total number of fragments (keys + data).

- **to_vacuum_num()** - Number of fragments pending vacuum.

## See also

[`storr_consolidate()`](https://cgiachalis.github.io/storr.tiledb/reference/storr_consolidate.md)
and
[`storr_vacuum()`](https://cgiachalis.github.io/storr.tiledb/reference/storr_vacuum.md)

## Examples

``` r
# URI path
uri <- tempfile()
sto <- storr_tiledb(uri, init = TRUE)

fosto <- storr_fragments(uri)

fosto$frag_num()
#> [1] 0
```

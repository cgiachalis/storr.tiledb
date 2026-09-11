# Low Level TileDB Utilities

Internal low-level TileDB functions that are required to run
asynchronous processes with `mirai` framework.

## Usage

``` r
.libtiledb_array_consolidate(ctx, uri, cfgptr = NULL)

.libtiledb_array_vacuum(ctx, uri, cfgptr = NULL)
```

## Arguments

- ctx:

  TileDB context pointer.

- uri:

  TileDB URI path.

- cfgptr:

  TileDB configuration pointer.

## Details

These functions are for internal use and exported to avoid `:::` usage
within mirai calls.

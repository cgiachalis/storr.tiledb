# TileDB Driver Schemas

Tune driver performance and storage characteristics: compression
algorithms, compression levels, tile capacity, cell order, and tile
order settings. This is useful for creating storage drivers for use
cases that need different trade-offs (speed vs. compression, memory vs.
disk).

## Usage

``` r
driver_schemas(uri = NULL, ctx = NULL, none_filter = FALSE)
```

## Arguments

- uri:

  Optional URI path to `TileDB` driver. If not given, the default
  template array schemas will be used.

- ctx:

  Optional
  [tiledb_ctx](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_ctx.html)
  object.

- none_filter:

  `TRUE` for no filters, `FALSE` for default filters. Applied on default
  schemas and not on schemas extracted from a URI path.

## Value

An object of class
[TileDBDriverSchemas](https://cgiachalis.github.io/storr.tiledb/reference/TileDBDriverSchemas.md).

## Details

Use `driver_schemas()` to:

- Create schemas with optional filters

- Dynamically apply compression to individual attributes

- Persist those customizations into the underlying TileDB arrays

## Examples

``` r
ctx <- new_context()
sto_schemas <- driver_schemas(ctx = ctx, none_filter = TRUE)


# Set up a ZSTD filter with high compression
flt <- tiledb::tiledb_filter("ZSTD", ctx = ctx)
flt <- tiledb::tiledb_filter_set_option(flt,"COMPRESSION_LEVEL", 22)
fl_list <- tiledb::tiledb_filter_list(flt, ctx = ctx)

# Apply filter list to 'value' attribute (CAS storage data)
sto_schemas$SchemaData$attr_value <- fl_list

# 'data' schema is modified now
sto_schemas$SchemaData$schema()
#> tiledb_array_schema(
#>     domain=tiledb_domain(c(
#>         tiledb_dim(name="hash", domain=c(NULL,NULL), tile=NULL, type="ASCII", filter_list=tiledb_filter_list(c(tiledb_filter("NONE"))))
#>     )),
#>     attrs=c(
#>         tiledb_attr(name="value", type="ASCII", ncells=NA, nullable=FALSE, filter_list=tiledb_filter_list(c(tiledb_filter_set_option(tiledb_filter("ZSTD"),"COMPRESSION_LEVEL",22))))
#>     ),
#>     cell_order="COL_MAJOR", tile_order="COL_MAJOR", capacity=10000, sparse=TRUE, allows_dups=FALSE,
#>     coords_filter_list=tiledb_filter_list(c(tiledb_filter("NONE"))),
#>     offsets_filter_list=tiledb_filter_list(c(tiledb_filter("NONE"))),
#>     validity_filter_list=tiledb_filter_list(c(tiledb_filter("NONE")))
#> )
```

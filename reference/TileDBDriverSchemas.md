# Generate a `TileDBDriverSchemas` Object

An R6 class that represents the storr's CAS schemas and provides access
to both
[SchemaKeys](https://cgiachalis.github.io/storr.tiledb/reference/SchemaKeys.md)
and
[SchemaData](https://cgiachalis.github.io/storr.tiledb/reference/SchemaData.md)
objects.

Users can use the schema objects to access and modify individual
schemas' filter lists and configuration via active fields.

For creating a `TileDBDriverSchemas` object, use the convenient wrapper
[`driver_schemas()`](https://cgiachalis.github.io/storr.tiledb/reference/driver_schemas.md).

### Structure

`TileDBDriverSchemas` holds `SchemaKeys` and `SchemaData` as active
bindings that can be accessed and modifiled in-place. Then, the modified
`TileDBDriverSchemas` can be passed to driver creation method.

    SchemaBase (abstract foundation)
    ├── SchemaKeys (keys/index schema)
    └── SchemaData (data/payload schema)

    TileDBDriverSchemas (factory/container)

## Value

A `TileDBDriverSchemas`, `R6` object.

## Active bindings

- `SchemaKeys`:

  Get
  [`SchemaKeys()`](https://cgiachalis.github.io/storr.tiledb/reference/SchemaKeys.md)
  object.

- `SchemaData`:

  Get
  [`SchemaData()`](https://cgiachalis.github.io/storr.tiledb/reference/SchemaData.md)
  object.

## Methods

### Public methods

- [`TileDBDriverSchemas$new()`](#method-TileDBDriverSchemas-initialize)

- [`TileDBDriverSchemas$print()`](#method-TileDBDriverSchemas-print)

- [`TileDBDriverSchemas$clone()`](#method-TileDBDriverSchemas-clone)

------------------------------------------------------------------------

### `TileDBDriverSchemas$new()`

Create a new `TileDBDriverSchemas` object.

#### Usage

    TileDBDriverSchemas$new(uri = NULL, ctx = NULL, none_filter = FALSE)

#### Arguments

- `uri`:

  Optional URI path to `TileDB` driver. If not given, the default
  schemas array will be used.

- `ctx`:

  Optional
  [tiledb_ctx](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_ctx.html)
  object.

- `none_filter`:

  `TRUE` for no filters, `FALSE` for default filters. Applied on default
  schemas and not on schemas extracted from a URI path.

------------------------------------------------------------------------

### `TileDBDriverSchemas$print()`

Print class.

#### Usage

    TileDBDriverSchemas$print()

------------------------------------------------------------------------

### `TileDBDriverSchemas$clone()`

The objects of this class are cloneable with this method.

#### Usage

    TileDBDriverSchemas$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

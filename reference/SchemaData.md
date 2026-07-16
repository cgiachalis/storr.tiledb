# Generate a `SchemaData` Object

An R6 class that represents the 'data' schema for CAS storage and
provides active fields to get/set filter lists for each
dimension/attribute.

This class should not be used directly, but it can be accessed via
[`driver_schemas()`](https://cgiachalis.github.io/storr.tiledb/reference/driver_schemas.md).

## Value

A `SchemaData`, `R6` object.

## Super class

[`SchemaBase`](https://cgiachalis.github.io/storr.tiledb/reference/SchemaBase.md)
-\> `SchemaData`

## Active bindings

- `dim_hash`:

  Get or set a filter list.

- `attr_value`:

  Get or set a filter list.

## Methods

### Public methods

- [`SchemaData$new()`](#method-SchemaData-initialize)

- [`SchemaData$clone()`](#method-SchemaData-clone)

Inherited methods

- [`SchemaBase$print()`](https://cgiachalis.github.io/storr.tiledb/reference/SchemaBase.html#method-print)
- [`SchemaBase$schema()`](https://cgiachalis.github.io/storr.tiledb/reference/SchemaBase.html#method-schema)

------------------------------------------------------------------------

### `SchemaData$new()`

Create a new `SchemaData` object.

#### Usage

    SchemaData$new(uri = NULL, ctx = NULL, none_filter = FALSE)

#### Arguments

- `uri`:

  Optional URI path to array for extracting its schema from. If not
  given, the default schema will be loaded.

- `ctx`:

  Optional
  [tiledb_ctx](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_ctx.html)
  object.

- `none_filter`:

  `TRUE` for no filters, `FALSE` for default filters. Applied on default
  schemas and not on schemas extracted from a URI path.

------------------------------------------------------------------------

### `SchemaData$clone()`

The objects of this class are cloneable with this method.

#### Usage

    SchemaData$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

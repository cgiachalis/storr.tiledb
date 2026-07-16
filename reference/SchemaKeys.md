# Generate a `SchemaKeys` Object

An R6 class that represents the 'keys' schema for CAS storage and
provides active fields to get/set filter lists for each
dimension/attribute.

This class should not be used directly, but it can be accessed via
[`driver_schemas()`](https://cgiachalis.github.io/storr.tiledb/reference/driver_schemas.md).

## Value

A `SchemaKeys`, `R6` object.

## Super class

[`SchemaBase`](https://cgiachalis.github.io/storr.tiledb/reference/SchemaBase.md)
-\> `SchemaKeys`

## Active bindings

- `dim_namespace`:

  Get or set a filter list.

- `dim_key`:

  Get or set a filter list.

- `attr_hash`:

  Get or set a filter list.

- `attr_expires_at`:

  Get or set a filter list.

- `attr_notes`:

  Get or set a filter list.

## Methods

### Public methods

- [`SchemaKeys$new()`](#method-SchemaKeys-initialize)

- [`SchemaKeys$clone()`](#method-SchemaKeys-clone)

Inherited methods

- [`SchemaBase$print()`](https://cgiachalis.github.io/storr.tiledb/reference/SchemaBase.html#method-print)
- [`SchemaBase$schema()`](https://cgiachalis.github.io/storr.tiledb/reference/SchemaBase.html#method-schema)

------------------------------------------------------------------------

### `SchemaKeys$new()`

Create a new `SchemaKeys` object.

#### Usage

    SchemaKeys$new(uri = NULL, ctx = NULL, none_filter = FALSE)

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

### `SchemaKeys$clone()`

The objects of this class are cloneable with this method.

#### Usage

    SchemaKeys$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

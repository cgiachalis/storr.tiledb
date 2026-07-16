# Generate a `SchemaBase` Object

A virtual base class to be inherited by specialised R6 schema classes.

Provides common schema functionality for 'keys' and 'data' schemas for
CAS storage. This class manages filter lists for coordinates, offsets,
and validity data and controls tile capacity, cell order, and tile order
settings.

The schema creation comes either from existing CAS storage URI path or
the default templates.

This class should not be used directly.

## Value

A `SchemaBase`, `R6` object.

## Active bindings

- `coords_flist`:

  Get or set the filter list for the array's coordinates.

- `offsets_flist`:

  Get or set the filter list for the array's variable- length attribute
  offsets.

- `validity_flist`:

  Get or set the filter list for the array's validity.

- `capacity`:

  Get or set the array capacity of each tile.

- `cell_order`:

  Get or set the cell order layout of the array.

- `tile_order`:

  Get or set the tile order layout of the array.

## Methods

### Public methods

- [`SchemaBase$new()`](#method-SchemaBase-initialize)

- [`SchemaBase$schema()`](#method-SchemaBase-schema)

- [`SchemaBase$print()`](#method-SchemaBase-print)

- [`SchemaBase$clone()`](#method-SchemaBase-clone)

------------------------------------------------------------------------

### `SchemaBase$new()`

Create a new `SchemaBase` object.

#### Usage

    SchemaBase$new(uri = NULL, ctx = NULL, none_filter = FALSE)

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

### `SchemaBase$schema()`

Get TileDB Schema.

#### Usage

    SchemaBase$schema()

------------------------------------------------------------------------

### `SchemaBase$print()`

Print Schema class.

#### Usage

    SchemaBase$print()

------------------------------------------------------------------------

### `SchemaBase$clone()`

The objects of this class are cloneable with this method.

#### Usage

    SchemaBase$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

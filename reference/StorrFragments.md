# Generate a `StorrFragments` Object

A class for working with Storr TileDB Fragments.

## Value

An object of class `StorrFragments`, `R6`.

## Active bindings

- `fob_keys`:

  Retrieve the
  [R6.tiledb::TileDBFragments](https://cgiachalis.github.io/R6.tiledb/reference/TileDBFragments.html)
  instance for `keys` array.

- `fob_data`:

  Retrieve the
  [R6.tiledb::TileDBFragments](https://cgiachalis.github.io/R6.tiledb/reference/TileDBFragments.html)
  instance for `data` array.

- `size`:

  Return directory size.

## Methods

### Public methods

- [`StorrFragments$new()`](#method-StorrFragments-new)

- [`StorrFragments$consolidate()`](#method-StorrFragments-consolidate)

- [`StorrFragments$vacuum()`](#method-StorrFragments-vacuum)

- [`StorrFragments$frag_num()`](#method-StorrFragments-frag_num)

- [`StorrFragments$to_vacuum_num()`](#method-StorrFragments-to_vacuum_num)

- [`StorrFragments$reload_finfo()`](#method-StorrFragments-reload_finfo)

- [`StorrFragments$print()`](#method-StorrFragments-print)

------------------------------------------------------------------------

### Method `new()`

Create a new `StorrFragments` instance.

#### Usage

    StorrFragments$new(uri, ctx = NULL)

#### Arguments

- `uri`:

  URI path for TileDB Storr.

- `ctx`:

  Optional
  [`tiledb::tiledb_ctx()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_ctx.html)
  object.

------------------------------------------------------------------------

### Method `consolidate()`

Consolidates the 'storr' fragments.

Consolidation in TileDB merges multiple array fragments into a single
fragment to improve query performance by reducing the number of files
that need to be read during queries.

The consolidation process is not deleting the old fragments. To clear
the consolidated fragments after the process, set `vacuum = TRUE` which
will invoke the vacuum process. Alternatively, use the class method
`$vacuum()`.

#### Usage

    StorrFragments$consolidate(what = c("all", "keys", "data"), cfg = NULL,
      vacuum = FALSE, async = FALSE)

#### Arguments

- `what`:

  Which array should be consolidated? Defaults to `"all"` arrays.

- `cfg`:

  A configuration object
  [`tiledb::tiledb_config()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_config.html)
  to override context configuration. When `NULL` (default) the
  configuration parameters will be retrieved from object's context.

- `vacuum`:

  Should the old fragments (consolidated) be deleted? Default is
  `FALSE`.

- `async`:

  Should it consolidate asynchronously? Default is `FALSE`.

#### Returns

When `async = FALSE`, it returns `TRUE` for successful consolidation.
For `async = TRUE`, it returns a
[`mirai::mirai()`](https://mirai.r-lib.org/reference/mirai.html) object
immediately; once resolved, it returns `TRUE` indicating consolidation
success.

------------------------------------------------------------------------

### Method `vacuum()`

Vacuum Storr fragments

This operation deletes the old fragments (consolidated).

#### Usage

    StorrFragments$vacuum(what = c("all", "keys", "data"), cfg = NULL,
      async = FALSE)

#### Arguments

- `what`:

  Which array to vacuum? Defaults to `"all"` arrays.

- `cfg`:

  A configuration object
  [`tiledb::tiledb_config()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_config.html)
  to override context configuration. When `NULL` (default) the
  configuration parameters will be retrieved from object's context.

- `async`:

  Should it vacuum asynchronously? Default is `FALSE`.

#### Returns

When `async = FALSE`, it returns `TRUE` for successful vacuuming. For
`async = TRUE`, it returns a
[`mirai::mirai()`](https://mirai.r-lib.org/reference/mirai.html) object
immediately; once resolved, it returns `TRUE` indicating vacuum success.

------------------------------------------------------------------------

### Method `frag_num()`

Get the number of fragments.

#### Usage

    StorrFragments$frag_num()

#### Returns

A numeric value.

------------------------------------------------------------------------

### Method `to_vacuum_num()`

Return the number of fragments to vacuum.

#### Usage

    StorrFragments$to_vacuum_num()

#### Returns

A numeric value.

------------------------------------------------------------------------

### Method `reload_finfo()`

Refresh the Storr's Fragment Info objects.

#### Usage

    StorrFragments$reload_finfo()

#### Returns

The object, invisibly.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print Fragments class.

#### Usage

    StorrFragments$print()

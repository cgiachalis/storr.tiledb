# Generate a `StorrTimeTravel` Object

A
[TileDBStorr](https://cgiachalis.github.io/storr.tiledb/reference/TileDBStorr.md)
is a time-travel variant of `TileDBStorr` designed to query data at
specific points in time and in read-only mode with no write
capabilities.

This class is not intended to be used directly and the preferred usage
is through
[`storr_timetravel()`](https://cgiachalis.github.io/storr.tiledb/reference/storr_timetravel.md).

## Value

A `StorrTimeTravel`, `R6` object.

## Public fields

- `default_namespace`:

  The default namespace.

- `traits`:

  Driver traits.

## Active bindings

- `timestamp`:

  Set or retrieve a `TileDB` timestamp range that the resource will be
  opened at. Effective in `"READ"` mode only.

## Methods

### Public methods

- [`StorrTimeTravel$new()`](#method-StorrTimeTravel-initialize)

- [`StorrTimeTravel$get()`](#method-StorrTimeTravel-get)

- [`StorrTimeTravel$mget()`](#method-StorrTimeTravel-mget)

- [`StorrTimeTravel$get_hash()`](#method-StorrTimeTravel-get_hash)

- [`StorrTimeTravel$mget_hash()`](#method-StorrTimeTravel-mget_hash)

- [`StorrTimeTravel$get_value()`](#method-StorrTimeTravel-get_value)

- [`StorrTimeTravel$mget_value()`](#method-StorrTimeTravel-mget_value)

- [`StorrTimeTravel$get_keymeta()`](#method-StorrTimeTravel-get_keymeta)

- [`StorrTimeTravel$mget_keymeta()`](#method-StorrTimeTravel-mget_keymeta)

- [`StorrTimeTravel$exists()`](#method-StorrTimeTravel-exists)

- [`StorrTimeTravel$exists_object()`](#method-StorrTimeTravel-exists_object)

- [`StorrTimeTravel$keys_with_expiration()`](#method-StorrTimeTravel-keys_with_expiration)

- [`StorrTimeTravel$expired_keys()`](#method-StorrTimeTravel-expired_keys)

- [`StorrTimeTravel$has_expired_keys()`](#method-StorrTimeTravel-has_expired_keys)

- [`StorrTimeTravel$list()`](#method-StorrTimeTravel-list)

- [`StorrTimeTravel$list_hashes()`](#method-StorrTimeTravel-list_hashes)

- [`StorrTimeTravel$list_namespaces()`](#method-StorrTimeTravel-list_namespaces)

- [`StorrTimeTravel$export()`](#method-StorrTimeTravel-export)

- [`StorrTimeTravel$index_export()`](#method-StorrTimeTravel-index_export)

- [`StorrTimeTravel$export_tdb()`](#method-StorrTimeTravel-export_tdb)

------------------------------------------------------------------------

### `StorrTimeTravel$new()`

Initialise `StorrTimeTravel`.

#### Usage

    StorrTimeTravel$new(driver, default_namespace)

#### Arguments

- `driver`:

  A
  [TimeTravelDriver](https://cgiachalis.github.io/storr.tiledb/reference/TimeTravelDriver.md)
  object.

- `default_namespace`:

  The default namespace.

------------------------------------------------------------------------

### `StorrTimeTravel$get()`

Get an object given a key-namespace pair.

#### Usage

    StorrTimeTravel$get(key, namespace = self$default_namespace)

#### Arguments

- `key`:

  A scalar character of key name.

- `namespace`:

  A scalar character of namespace name.

#### Returns

The `R` object if available.

------------------------------------------------------------------------

### `StorrTimeTravel$mget()`

Get multiple objects.

The arguments `key` and `namespace` can be recycled if any of them is a
scalar character and the other is a vector. No other recycling rule is
permitted.

#### Usage

    StorrTimeTravel$mget(key, namespace = self$default_namespace,
      missing = NULL)

#### Arguments

- `key`:

  A character vector of key names.

- `namespace`:

  A character vector of namespaces.

- `missing`:

  Value to use for missing elements.

#### Returns

A list of `R` objects.

------------------------------------------------------------------------

### `StorrTimeTravel$get_hash()`

Get hash value.

#### Usage

    StorrTimeTravel$get_hash(key, namespace = self$default_namespace)

#### Arguments

- `key`:

  A scalar character of key name.

- `namespace`:

  A scalar character of namespace name.

#### Returns

The hash value.

------------------------------------------------------------------------

### `StorrTimeTravel$mget_hash()`

Get hash values.

The arguments `key` and `namespace` can be recycled if any of them is a
scalar character and the other is a vector. No other recycling rule is
permitted.

#### Usage

    StorrTimeTravel$mget_hash(key, namespace = self$default_namespace)

#### Arguments

- `key`:

  A character vector of key names.

- `namespace`:

  A character vector of namespaces.

#### Returns

A vector of hashes.

------------------------------------------------------------------------

### `StorrTimeTravel$get_value()`

Get an object given its hash.

#### Usage

    StorrTimeTravel$get_value(hash)

#### Arguments

- `hash`:

  The hash value of the object.

#### Returns

The `R` object if available.

------------------------------------------------------------------------

### `StorrTimeTravel$mget_value()`

Get multiple objects given their hashes.

#### Usage

    StorrTimeTravel$mget_value(hash, missing = NULL)

#### Arguments

- `hash`:

  A vector of hash values."

- `missing`:

  Value to use for missing elements.

#### Returns

A list of `R` objects.

------------------------------------------------------------------------

### `StorrTimeTravel$get_keymeta()`

Get key's metadata.

#### Usage

    StorrTimeTravel$get_keymeta(key, namespace = self$default_namespace)

#### Arguments

- `key`:

  The key name to get metadata values from.

- `namespace`:

  The namespace to look the key within.

#### Returns

A named list with the key-metadata: `"expires_at"` and `"notes".`

------------------------------------------------------------------------

### `StorrTimeTravel$mget_keymeta()`

Get multiple key metadata.

The arguments `key` and `namespace` can be recycled if any of them is a
scalar character and the other is a vector. No other recycling rule is
permitted.

#### Usage

    StorrTimeTravel$mget_keymeta(key, namespace = self$default_namespace,
      missing = NULL)

#### Arguments

- `key`:

  A character vector with keys to get metadata values from.

- `namespace`:

  A character vector of namespaces to look the keys within.

- `missing`:

  Fill value for missing keys. Default is `NULL`.

#### Returns

A list with key metadata for each key-namespace pair. For not found
pairs will return the `missing` value.

------------------------------------------------------------------------

### `StorrTimeTravel$exists()`

Check a key-namespace pair exists.

The arguments `key` and `namespace` can be recycled if any of them is a
scalar character and the other is a vector. No other recycling rule is
permitted.

#### Usage

    StorrTimeTravel$exists(key, namespace = self$default_namespace)

#### Arguments

- `key`:

  A character vector of key names.

- `namespace`:

  A character vector of namespaces.

#### Returns

A logical vector indicating which key-namespace pair exists.

------------------------------------------------------------------------

### `StorrTimeTravel$exists_object()`

Check a serialised object exists given a hash.

#### Usage

    StorrTimeTravel$exists_object(hash)

#### Arguments

- `hash`:

  A vector of hash values.

#### Returns

A logical vector indicating which object exists.

------------------------------------------------------------------------

### `StorrTimeTravel$keys_with_expiration()`

Get the key-namespace pairs with expiration timestamps.

#### Usage

    StorrTimeTravel$keys_with_expiration(namespace = self$default_namespace,
      datetimes = TRUE)

#### Arguments

- `namespace`:

  A character vector of namespaces or `NULL` for all namespaces.

- `datetimes`:

  Should the `expires_at` column be returned? Default is `TRUE`.

#### Returns

An object of class `data.table`.

------------------------------------------------------------------------

### `StorrTimeTravel$expired_keys()`

Get the expired key-namespace pairs.

#### Usage

    StorrTimeTravel$expired_keys(namespace = self$default_namespace,
      datetimes = TRUE)

#### Arguments

- `namespace`:

  A character vector of namespaces or `NULL` for all namespaces.

- `datetimes`:

  Should the `expires_at` column be returned? Default is `TRUE`.

#### Returns

An object of class `data.table`.

------------------------------------------------------------------------

### `StorrTimeTravel$has_expired_keys()`

Check for expired key-namespace pairs.

#### Usage

    StorrTimeTravel$has_expired_keys(namespace = self$default_namespace)

#### Arguments

- `namespace`:

  A character vector of namespaces or `NULL` for all namespaces.

#### Returns

`TRUE` for expired keys, `FALSE` otherwise.

------------------------------------------------------------------------

### `StorrTimeTravel$list()`

List all keys stored in a namespace.

#### Usage

    StorrTimeTravel$list(namespace = self$default_namespace)

#### Arguments

- `namespace`:

  A scalar character of namespace name.

#### Returns

A sorted character vector with keys.

------------------------------------------------------------------------

### `StorrTimeTravel$list_hashes()`

List all hashes stored in the storr.

#### Usage

    StorrTimeTravel$list_hashes()

#### Returns

A sorted character vector with hashes.

------------------------------------------------------------------------

### `StorrTimeTravel$list_namespaces()`

List all namespaces in the storr.

#### Usage

    StorrTimeTravel$list_namespaces()

#### Returns

A sorted character vector with namespaces.

------------------------------------------------------------------------

### `StorrTimeTravel$export()`

Export objects from storr.

Use list() to export to a brand new list, or use as.list(object) for a
shorthand.

#### Usage

    StorrTimeTravel$export(dest, list = NULL,
      namespace = self$default_namespace, skip_missing = FALSE)

#### Arguments

- `dest`:

  A destination to export objects to. It can be a storr, list, or
  environment.

- `list`:

  Names of objects to import (or `NULL` for all objects) . If given it
  must be a character vector. If named, the names of the character
  vector will be the names of the objects as created in the storr.

- `namespace`:

  Namespace to get objects from, and to put objects into. If `NULL`,
  then this will export namespaces from this (source) storr into the
  destination; if there is more than one namespace, this is only
  possible if `dest` is a storr (otherwise there will be an error).

- `skip_missing`:

  Logical, indicating if missing keys (specified in `list`) should be
  skipped over, rather than being treated as an error (the default).

#### Returns

`dest` object, invisibly.

------------------------------------------------------------------------

### `StorrTimeTravel$index_export()`

Generate a `data.table` with an index of objects present in a storr.

#### Usage

    StorrTimeTravel$index_export(namespace = NULL)

#### Arguments

- `namespace`:

  A character vector of namespaces or `NULL` for all namespaces.

#### Returns

An object of class `data.table`.

------------------------------------------------------------------------

### `StorrTimeTravel$export_tdb()`

Export objects from storr to another TileDB storr.

#### Usage

    StorrTimeTravel$export_tdb(key = character(0),
      namespace = self$default_namespace, uri_dest, context_dest = NULL)

#### Arguments

- `key`:

  A character vector of source keys.

- `namespace`:

  A character vector of namespaces or `NULL` for all namespaces.

- `uri_dest`:

  The URI path of destination storr.

- `context_dest`:

  Optional
  [tiledb_ctx](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_ctx.html)
  object for destination storr.

#### Returns

A logical `TRUE` indicating successful export, invisibly.

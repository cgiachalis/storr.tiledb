# Generate a `TileDBStorr` Object

An R6 class that represents a storr interface for TileDB driver.

`TileDBStorr` replicates the `storr` interface but also enhances it with
additional new features:

- notes and key expiration timestamps

- asynchronous writes

Note that the following methods from `storr` are not supported by
`TileDBStorr`: `$archive_import`, `$archive_export`, `$check` and
`$repair`.

This class is not intended to be used directly and the preferred usage
is through
[`storr_tiledb()`](https://cgiachalis.github.io/storr.tiledb/reference/storr_tiledb.md).

## Value

A `TileDBStorr`, `R6` object.

## Public fields

- `envir`:

  The object hash table.

- `envir_metadata`:

  The key metadata hash table.

- `default_namespace`:

  The default namespace.

- `traits`:

  Driver traits.

- `hash_raw`:

  The hash function.

- `serialize_object`:

  The serialisation function.

## Active bindings

- `async_info`:

  `mirai` information

- `size`:

  Return Storr size

## Methods

### Public methods

- [`TileDBStorr$new()`](#method-TileDBStorr-initialize)

- [`TileDBStorr$destroy()`](#method-TileDBStorr-destroy)

- [`TileDBStorr$flush_cache()`](#method-TileDBStorr-flush_cache)

- [`TileDBStorr$set()`](#method-TileDBStorr-set)

- [`TileDBStorr$mset()`](#method-TileDBStorr-mset)

- [`TileDBStorr$set_async()`](#method-TileDBStorr-set_async)

- [`TileDBStorr$mset_async()`](#method-TileDBStorr-mset_async)

- [`TileDBStorr$set_by_value()`](#method-TileDBStorr-set_by_value)

- [`TileDBStorr$mset_by_value()`](#method-TileDBStorr-mset_by_value)

- [`TileDBStorr$set_by_value_async()`](#method-TileDBStorr-set_by_value_async)

- [`TileDBStorr$mset_by_value_async()`](#method-TileDBStorr-mset_by_value_async)

- [`TileDBStorr$set_value()`](#method-TileDBStorr-set_value)

- [`TileDBStorr$mset_value()`](#method-TileDBStorr-mset_value)

- [`TileDBStorr$get()`](#method-TileDBStorr-get)

- [`TileDBStorr$mget()`](#method-TileDBStorr-mget)

- [`TileDBStorr$get_hash()`](#method-TileDBStorr-get_hash)

- [`TileDBStorr$mget_hash()`](#method-TileDBStorr-mget_hash)

- [`TileDBStorr$hash_object()`](#method-TileDBStorr-hash_object)

- [`TileDBStorr$get_value()`](#method-TileDBStorr-get_value)

- [`TileDBStorr$mget_value()`](#method-TileDBStorr-mget_value)

- [`TileDBStorr$set_keymeta()`](#method-TileDBStorr-set_keymeta)

- [`TileDBStorr$mset_keymeta()`](#method-TileDBStorr-mset_keymeta)

- [`TileDBStorr$set_keymeta_async()`](#method-TileDBStorr-set_keymeta_async)

- [`TileDBStorr$mset_keymeta_async()`](#method-TileDBStorr-mset_keymeta_async)

- [`TileDBStorr$get_keymeta()`](#method-TileDBStorr-get_keymeta)

- [`TileDBStorr$mget_keymeta()`](#method-TileDBStorr-mget_keymeta)

- [`TileDBStorr$clr_keymeta()`](#method-TileDBStorr-clr_keymeta)

- [`TileDBStorr$clr_keymeta_async()`](#method-TileDBStorr-clr_keymeta_async)

- [`TileDBStorr$fill()`](#method-TileDBStorr-fill)

- [`TileDBStorr$duplicate()`](#method-TileDBStorr-duplicate)

- [`TileDBStorr$clear()`](#method-TileDBStorr-clear)

- [`TileDBStorr$exists()`](#method-TileDBStorr-exists)

- [`TileDBStorr$exists_object()`](#method-TileDBStorr-exists_object)

- [`TileDBStorr$del()`](#method-TileDBStorr-del)

- [`TileDBStorr$keys_with_expiration()`](#method-TileDBStorr-keys_with_expiration)

- [`TileDBStorr$expired_keys()`](#method-TileDBStorr-expired_keys)

- [`TileDBStorr$has_expired_keys()`](#method-TileDBStorr-has_expired_keys)

- [`TileDBStorr$clear_expired_keys()`](#method-TileDBStorr-clear_expired_keys)

- [`TileDBStorr$list()`](#method-TileDBStorr-list)

- [`TileDBStorr$list_hashes()`](#method-TileDBStorr-list_hashes)

- [`TileDBStorr$list_namespaces()`](#method-TileDBStorr-list_namespaces)

- [`TileDBStorr$gc()`](#method-TileDBStorr-gc)

- [`TileDBStorr$import()`](#method-TileDBStorr-import)

- [`TileDBStorr$export()`](#method-TileDBStorr-export)

- [`TileDBStorr$index_export()`](#method-TileDBStorr-index_export)

- [`TileDBStorr$index_import()`](#method-TileDBStorr-index_import)

- [`TileDBStorr$export_tdb()`](#method-TileDBStorr-export_tdb)

------------------------------------------------------------------------

### `TileDBStorr$new()`

Initialise `TileDBStorr`.

#### Usage

    TileDBStorr$new(driver, default_namespace, async = FALSE)

#### Arguments

- `driver`:

  A TileDB driver, see
  [`driver_tiledb()`](https://cgiachalis.github.io/storr.tiledb/reference/driver_tiledb.md).

- `default_namespace`:

  The default namespace.

- `async`:

  Should the [mirai](https://mirai.r-lib.org/reference/mirai.html)
  daemons be enabled for async functions? Default is `FALSE`.

------------------------------------------------------------------------

### `TileDBStorr$destroy()`

Destroy (delete) 'storr'.

#### Usage

    TileDBStorr$destroy()

#### Returns

`NULL`, invisibly.

------------------------------------------------------------------------

### `TileDBStorr$flush_cache()`

Flush the cache of `R` objects.

It removes all items from the hash tables (R objects and their
metadata).

#### Usage

    TileDBStorr$flush_cache()

#### Returns

The object, invisibly.

------------------------------------------------------------------------

### `TileDBStorr$set()`

Set a key value pair.

#### Usage

    TileDBStorr$set(key, value, namespace = self$default_namespace, expires_at,
      notes, use_cache = getOption("storr.tiledb.cache", TRUE))

#### Arguments

- `key`:

  A scalar character of key name.

- `value`:

  An R object to store.

- `namespace`:

  A scalar character of namespace name.

- `expires_at`:

  A date-time object of class `POSIXct` (optional).

- `notes`:

  A scalar string of notes (optional).

- `use_cache`:

  Should the cache be used? Default is `TRUE`.

#### Returns

The hash value, invisibly.

------------------------------------------------------------------------

### `TileDBStorr$mset()`

Set multiple key value pairs.

The arguments `key` and `namespace` can be recycled if any of them is a
scalar character and the other is a vector. No other recycling rule is
permitted.

#### Usage

    TileDBStorr$mset(key, value, namespace = self$default_namespace, expires_at,
      notes, use_cache = getOption("storr.tiledb.cache", TRUE))

#### Arguments

- `key`:

  A character vector of key names.

- `value`:

  A vector of values to store.

- `namespace`:

  A character vector of namespaces.

- `expires_at`:

  A vector of date-times of class `POSIXct` (optional).

- `notes`:

  A character vector of notes (optional).

- `use_cache`:

  Should the cache be used? Default is `TRUE`.

#### Returns

A vector of hash values, invisibly.

------------------------------------------------------------------------

### `TileDBStorr$set_async()`

Set a key value pair asynchronously.

#### Usage

    TileDBStorr$set_async(key, value, namespace = self$default_namespace,
      expires_at, notes, use_cache = getOption("storr.tiledb.cache", TRUE),
      cfg = NULL)

#### Arguments

- `key`:

  A scalar character of key name.

- `value`:

  An R object to store.

- `namespace`:

  A scalar character of namespace name.

- `expires_at`:

  A date-time object of class `POSIXct` (optional).

- `notes`:

  A scalar string of notes (optional).

- `use_cache`:

  Should the cache be used? Default is `TRUE`.

- `cfg`:

  Pass a
  [`tiledb::config()`](https://tiledb-inc.github.io/TileDB-R/reference/generics.html)
  object to override context's configuration.

#### Returns

Invisibly, a named list with two elements:

- `mirai`: a named list of two
  [`mirai()`](https://mirai.r-lib.org/reference/mirai.html) objects,
  `obj` and `key`; `obj` refers to object table and `key` to key table.
  Both return logical `TRUE` if an evaluation is successful.

- `hash`: the hash value

------------------------------------------------------------------------

### `TileDBStorr$mset_async()`

Set multiple key value pairs asynchronously.

The arguments `key` and `namespace` can be recycled if any of them is a
scalar character and the other is a vector. No other recycling rule is
permitted.

#### Usage

    TileDBStorr$mset_async(key, value, namespace = self$default_namespace,
      expires_at, notes, use_cache = getOption("storr.tiledb.cache", TRUE),
      cfg = NULL)

#### Arguments

- `key`:

  A character vector of key names.

- `value`:

  A vector of values to store.

- `namespace`:

  A character vector of namespaces.

- `expires_at`:

  A vector of date-times of class `POSIXct` (optional).

- `notes`:

  A character vector of notes (optional).

- `use_cache`:

  Should the cache be used? Default is `TRUE`.

- `cfg`:

  Pass a
  [`tiledb::config()`](https://tiledb-inc.github.io/TileDB-R/reference/generics.html)
  object to override context's configuration.

#### Returns

Invisibly, a named list with two elements:

- `mirai`: a named list of two
  [`mirai()`](https://mirai.r-lib.org/reference/mirai.html) objects,
  `obj` and `key`; `obj` refers to object table and `key` to key table.
  Both return logical `TRUE` if an evaluation is successful.

- `hash`: a vector with hash values

------------------------------------------------------------------------

### `TileDBStorr$set_by_value()`

Set a key value pair using its hash as key.

#### Usage

    TileDBStorr$set_by_value(value, namespace = self$default_namespace,
      expires_at, notes, use_cache = getOption("storr.tiledb.cache", TRUE))

#### Arguments

- `value`:

  An R object to store.

- `namespace`:

  A scalar character of namespace name.

- `expires_at`:

  A date-time object of class `POSIXct` (optional).

- `notes`:

  A scalar string of notes (optional).

- `use_cache`:

  Should the cache be used? Default is `TRUE`.

#### Returns

The hash value, invisibly.

------------------------------------------------------------------------

### `TileDBStorr$mset_by_value()`

Set multiple key value pairs using their hashes as keys.

#### Usage

    TileDBStorr$mset_by_value(value, namespace = self$default_namespace,
      expires_at, notes, use_cache = getOption("storr.tiledb.cache", TRUE))

#### Arguments

- `value`:

  A vector of values to store.

- `namespace`:

  A character vector of namespaces.

- `expires_at`:

  A vector of date-times of class `POSIXct` (optional).

- `notes`:

  A character vector of notes (optional).

- `use_cache`:

  Should the cache be used? Default is `TRUE`.

#### Returns

A vector of hash values, invisibly.

------------------------------------------------------------------------

### `TileDBStorr$set_by_value_async()`

Set a key value pair using its hash as key, asynchronously.

#### Usage

    TileDBStorr$set_by_value_async(value, namespace = self$default_namespace,
      expires_at, notes, use_cache = getOption("storr.tiledb.cache", TRUE),
      cfg = NULL)

#### Arguments

- `value`:

  An R object to store.

- `namespace`:

  A scalar character of namespace name.

- `expires_at`:

  A date-time object of class `POSIXct` (optional).

- `notes`:

  A scalar string of notes (optional).

- `use_cache`:

  Should the cache be used? Default is `TRUE`.

- `cfg`:

  Pass a
  [`tiledb::config()`](https://tiledb-inc.github.io/TileDB-R/reference/generics.html)
  object to override context's configuration.

#### Returns

Invisibly, a named list with two elements:

- `mirai`: a named list of two
  [`mirai()`](https://mirai.r-lib.org/reference/mirai.html) objects,
  `obj` and `key`; `obj` refers to object table and `key` to key table.
  Both return logical `TRUE` if an evaluation is successful.

- `hash`: the hash value

------------------------------------------------------------------------

### `TileDBStorr$mset_by_value_async()`

Set multiple key value pairs using their hashes as keys, asynchronously.

The arguments `key` and `namespace` can be recycled if any of them is a
scalar character and the other is a vector. No other recycling rule is
permitted.

#### Usage

    TileDBStorr$mset_by_value_async(value, namespace = self$default_namespace,
      expires_at, notes, use_cache = getOption("storr.tiledb.cache", TRUE),
      cfg = NULL)

#### Arguments

- `value`:

  A vector of values to store.

- `namespace`:

  A character vector of namespaces.

- `expires_at`:

  A vector of date-times of class `POSIXct` (optional).

- `notes`:

  A character vector of notes (optional).

- `use_cache`:

  Should the cache be used? Default is `TRUE`.

- `cfg`:

  Pass a
  [`tiledb::config()`](https://tiledb-inc.github.io/TileDB-R/reference/generics.html)
  object to override context's configuration.

#### Returns

Invisibly, a named list with two elements:

- `mirai`: a named list of two
  [`mirai()`](https://mirai.r-lib.org/reference/mirai.html) objects,
  `obj` and `key`; `obj` refers to object table and `key` to key table.
  Both return logical `TRUE` if an evaluation is successful.

- `hash`: a vector with hash values

------------------------------------------------------------------------

### `TileDBStorr$set_value()`

Add an R object without key.

This is used internally.

#### Usage

    TileDBStorr$set_value(value, use_cache = getOption("storr.tiledb.cache",
      TRUE))

#### Arguments

- `value`:

  An R object to store.

- `use_cache`:

  Should the cache be used? Default is `TRUE`.

#### Returns

The hash value, invisibly.

------------------------------------------------------------------------

### `TileDBStorr$mset_value()`

Add a vector of R objects.

This is used internally.

#### Usage

    TileDBStorr$mset_value(values, use_cache = getOption("storr.tiledb.cache",
      TRUE))

#### Arguments

- `values`:

  A vector of values to store.

- `use_cache`:

  Should the cache be used? Default is `TRUE`.

#### Returns

A vector of hash values, invisibly.

------------------------------------------------------------------------

### `TileDBStorr$get()`

Get an object given a key-namespace pair.

#### Usage

    TileDBStorr$get(key, namespace = self$default_namespace,
      use_cache = getOption("storr.tiledb.cache", TRUE))

#### Arguments

- `key`:

  A scalar character of key name.

- `namespace`:

  A scalar character of namespace name.

- `use_cache`:

  Should the cache be used? Default is `TRUE`.

#### Returns

The `R` object if available.

------------------------------------------------------------------------

### `TileDBStorr$mget()`

Get multiple objects.

The arguments `key` and `namespace` can be recycled if any of them is a
scalar character and the other is a vector. No other recycling rule is
permitted.

#### Usage

    TileDBStorr$mget(key, namespace = self$default_namespace,
      use_cache = getOption("storr.tiledb.cache", TRUE), missing = NULL)

#### Arguments

- `key`:

  A character vector of key names.

- `namespace`:

  A character vector of namespaces.

- `use_cache`:

  Should the cache be used? Default is `TRUE`.

- `missing`:

  Value to use for missing elements.

#### Returns

A list of `R` objects.

------------------------------------------------------------------------

### `TileDBStorr$get_hash()`

Get hash value.

#### Usage

    TileDBStorr$get_hash(key, namespace = self$default_namespace)

#### Arguments

- `key`:

  A scalar character of key name.

- `namespace`:

  A scalar character of namespace name.

#### Returns

The hash value.

------------------------------------------------------------------------

### `TileDBStorr$mget_hash()`

Get hash values.

The arguments `key` and `namespace` can be recycled if any of them is a
scalar character and the other is a vector. No other recycling rule is
permitted.

#### Usage

    TileDBStorr$mget_hash(key, namespace = self$default_namespace)

#### Arguments

- `key`:

  A character vector of key names.

- `namespace`:

  A character vector of namespaces.

#### Returns

A vector of hashes.

------------------------------------------------------------------------

### `TileDBStorr$hash_object()`

Create a hash digest for an R object.

#### Usage

    TileDBStorr$hash_object(object)

#### Arguments

- `object`:

  An R object.

#### Returns

A character string of a fixed length containing the requested digest
(hash) of the supplied R object.

------------------------------------------------------------------------

### `TileDBStorr$get_value()`

Get an object given its hash.

#### Usage

    TileDBStorr$get_value(hash, use_cache = getOption("storr.tiledb.cache",
      TRUE))

#### Arguments

- `hash`:

  The hash value of the object.

- `use_cache`:

  Should the cache be used? Default is `TRUE`.

#### Returns

The `R` object if available.

------------------------------------------------------------------------

### `TileDBStorr$mget_value()`

Get multiple objects given their hashes.

#### Usage

    TileDBStorr$mget_value(hash, use_cache = getOption("storr.tiledb.cache",
      TRUE), missing = NULL)

#### Arguments

- `hash`:

  A vector of hash values."

- `use_cache`:

  Should the cache be used? Default is `TRUE`.

- `missing`:

  Value to use for missing elements.

#### Returns

A list of `R` objects.

------------------------------------------------------------------------

### `TileDBStorr$set_keymeta()`

Set key metadata.

#### Usage

    TileDBStorr$set_keymeta(key, namespace = self$default_namespace, expires_at,
      notes, use_cache = getOption("storr.tiledb.cache", TRUE))

#### Arguments

- `key`:

  A scalar character of key name.

- `namespace`:

  A scalar character of namespace name.

- `expires_at`:

  A date-time object of class `POSIXct` (optional).

- `notes`:

  A scalar string of notes (optional).

- `use_cache`:

  Should the cache be used to retrieve the metadata? Default is `TRUE`.
  If a key:namespace not found in the cache, it will be fetched from
  database. Note that when setting `FALSE`, the cache will always be
  cleared for this key-namespace; this is to avoid mismatch between
  cache and database when reading back with `use_cache = TRUE`.

#### Returns

The `key:namespace` string, invisibly. If both arguments `"expires_at"`
and `"notes"` are missing, then nothing is set and a zero length
character vector is returned.

------------------------------------------------------------------------

### `TileDBStorr$mset_keymeta()`

Set multiple key metadata.

The arguments `key` and `namespace` can be recycled if any of them is a
scalar character and the other is a vector. No other recycling rule is
permitted.

#### Usage

    TileDBStorr$mset_keymeta(key, namespace = self$default_namespace, expires_at,
      notes, use_cache = getOption("storr.tiledb.cache", TRUE))

#### Arguments

- `key`:

  A character vector of key names.

- `namespace`:

  A character vector of namespaces.

- `expires_at`:

  A vector of date-times of class `POSIXct` (optional).

- `notes`:

  A character vector of notes (optional).

- `use_cache`:

  Should the cache be used to retrieve the metadata? Default is `TRUE`.
  If a key:namespace not found in the cache, it will be fetched from
  database. Note that when setting `FALSE`, the cache will always be
  cleared for this key-namespace; this is to avoid mismatch between
  cache and database when reading back with `use_cache = TRUE`.

#### Returns

The `key:namespace` character vector of the recycled length, invisibly.
If both arguments `"expires_at"` and `"notes"` are missing, then nothing
is set and a zero length character vector is returned.

------------------------------------------------------------------------

### `TileDBStorr$set_keymeta_async()`

Set key metadata asynchronously.

#### Usage

    TileDBStorr$set_keymeta_async(key, namespace = self$default_namespace,
      expires_at, notes, use_cache = getOption("storr.tiledb.cache", TRUE),
      cfg = NULL)

#### Arguments

- `key`:

  A scalar character of key name.

- `namespace`:

  A scalar character of namespace name.

- `expires_at`:

  A date-time object of class `POSIXct` (optional).

- `notes`:

  A scalar string of notes (optional).

- `use_cache`:

  Should the cache be used to retrieve the metadata? Default is `TRUE`.
  If a key:namespace not found in the cache, it will be fetched from
  database. Note that when setting `FALSE`, the cache will always be
  cleared for this key-namespace; this is to avoid mismatch between
  cache and database when reading back with `use_cache = TRUE`.

- `cfg`:

  Pass a
  [`tiledb::config()`](https://tiledb-inc.github.io/TileDB-R/reference/generics.html)
  object to override context's configuration.

#### Returns

A named list with two elements (invisibly):

- `mirai`: a mirai object

- `keyns`: The `key:namespace` string

If both arguments `"expires_at"` and `"notes"` are missing, then nothing
is set and a zero length character vector is returned.

------------------------------------------------------------------------

### `TileDBStorr$mset_keymeta_async()`

Set multiple key metadata.

The arguments `key` and `namespace` can be recycled if any of them is a
scalar character and the other is a vector. No other recycling rule is
permitted.

#### Usage

    TileDBStorr$mset_keymeta_async(key, namespace = self$default_namespace,
      expires_at, notes, use_cache = getOption("storr.tiledb.cache", TRUE),
      cfg = NULL)

#### Arguments

- `key`:

  A character vector of key names.

- `namespace`:

  A character vector of namespaces.

- `expires_at`:

  A vector of date-times of class `POSIXct` (optional).

- `notes`:

  A character vector of notes (optional).

- `use_cache`:

  Should the cache be used to retrieve the metadata? Default is `TRUE`.
  If a key:namespace not found in the cache, it will be fetched from
  database. Note that when setting `FALSE`, the cache will always be
  cleared for this key-namespace; this is to avoid mismatch between
  cache and database when reading back with `use_cache = TRUE`.

- `cfg`:

  Pass a
  [`tiledb::config()`](https://tiledb-inc.github.io/TileDB-R/reference/generics.html)
  object to override context's configuration.

#### Returns

A named list with two elements (invisibly):

- `mirai`: a mirai object

- `keyns`: The `key:namespace` character vector of the recycled length

If both arguments `"expires_at"` and `"notes"` are missing, then nothing
is set and a zero length character vector is returned.

------------------------------------------------------------------------

### `TileDBStorr$get_keymeta()`

Get key's metadata.

#### Usage

    TileDBStorr$get_keymeta(key, namespace = self$default_namespace,
      use_cache = getOption("storr.tiledb.cache", TRUE))

#### Arguments

- `key`:

  The key name to get metadata values from.

- `namespace`:

  The namespace to look the key within.

- `use_cache`:

  Should it be retrieved from cache? Default is `TRUE`.

#### Returns

A named list with the key-metadata: `"expires_at"` and `"notes".`

------------------------------------------------------------------------

### `TileDBStorr$mget_keymeta()`

Get multiple key metadata.

The arguments `key` and `namespace` can be recycled if any of them is a
scalar character and the other is a vector. No other recycling rule is
permitted.

#### Usage

    TileDBStorr$mget_keymeta(key, namespace = self$default_namespace,
      use_cache = getOption("storr.tiledb.cache", TRUE), missing = NULL)

#### Arguments

- `key`:

  A character vector with keys to get metadata values from.

- `namespace`:

  A character vector of namespaces to look the keys within.

- `use_cache`:

  Should it be retrieved from cache? Default is `TRUE`.

- `missing`:

  Fill value for missing keys. Default is `NULL`.

#### Returns

A list with key metadata for each key-namespace pair. For not found
pairs will return the `missing` value.

------------------------------------------------------------------------

### `TileDBStorr$clr_keymeta()`

Remove key metadata.

This method is a convenient wrapper around `set_keymeta()` and
`mset_keymeta()` and sets the key metadata fields to `NA` values, i.e.,
`as.POSIXct(NA)` and `NA_character`.

The arguments `key` and `namespace` can be recycled if any of them is a
scalar character and the other is a vector. No other recycling rule is
permitted.

#### Usage

    TileDBStorr$clr_keymeta(key, namespace = self$default_namespace,
      use_cache = getOption("storr.tiledb.cache", TRUE))

#### Arguments

- `key`:

  A character vector of key names.

- `namespace`:

  A character vector of namespaces.

- `use_cache`:

  Should the cache be used to retrieve the metadata? Default is `TRUE`.
  If a key:namespace not found in the cache, it will be fetched from
  database. Note that when setting `FALSE`, the cache will always be
  cleared for this key-namespace; this is to avoid mismatch between
  cache and database when reading back with `use_cache = TRUE`.

#### Returns

The `key:namespace` character vector of the recycled length, invisibly.

------------------------------------------------------------------------

### `TileDBStorr$clr_keymeta_async()`

Remove key metadata asynchronously.

This method is a convenient wrapper around `set_keymeta_async()` and
`mset_keymeta_async()` and sets the key metadata fields to `NA` values,
i.e., `as.POSIXct(NA)` and `NA_character`.

The arguments `key` and `namespace` can be recycled if any of them is a
scalar character and the other is a vector. No other recycling rule is
permitted.

#### Usage

    TileDBStorr$clr_keymeta_async(key, namespace = self$default_namespace,
      use_cache = getOption("storr.tiledb.cache", TRUE), cfg = NULL)

#### Arguments

- `key`:

  A character vector of key names.

- `namespace`:

  A character vector of namespaces.

- `use_cache`:

  Should the cache be used to retrieve the metadata? Default is `TRUE`.
  If a key:namespace not found in the cache, it will be fetched from
  database. Note that when setting `FALSE`, the cache will always be
  cleared for this key-namespace; this is to avoid mismatch between
  cache and database when reading back with `use_cache = TRUE`.

- `cfg`:

  Pass a
  [`tiledb::config()`](https://tiledb-inc.github.io/TileDB-R/reference/generics.html)
  object to override context's configuration.

#### Returns

A named list with two elements (invisibly):

- `mirai`: a mirai object

- `keyns`: The `key:namespace` character vector of the recycled length

------------------------------------------------------------------------

### `TileDBStorr$fill()`

Set one or more keys to the same value.

The arguments `key` and `namespace` can be recycled if any of them is a
scalar character and the other is a vector. No other recycling rule is
permitted.

#### Usage

    TileDBStorr$fill(key, value, namespace = self$default_namespace,
      use_cache = getOption("storr.tiledb.cache", TRUE))

#### Arguments

- `key`:

  A character vector of key names.

- `value`:

  An R object to store.

- `namespace`:

  A character vector of namespaces.

- `use_cache`:

  Should the cache be used? Default is `TRUE`.

#### Returns

A vector of hash values, invisibly.

------------------------------------------------------------------------

### `TileDBStorr$duplicate()`

Duplicate a set of keys.

#### Usage

    TileDBStorr$duplicate(key_src, key_dest, namespace = self$default_namespace,
      namespace_src = namespace, namespace_dest = namespace)

#### Arguments

- `key_src`:

  A character vector of source keys.

- `key_dest`:

  A character vector of destination keys.

- `namespace`:

  The namespace to copy keys within (used only of `namespace_src` and
  `namespace_dest` are not provided.

- `namespace_src`:

  The source namespace - use this where keys are duplicated across
  namespaces.

- `namespace_dest`:

  The destination namespace - use this where keys are duplicated across
  namespaces.

#### Returns

`NULL`, invisibly.

------------------------------------------------------------------------

### `TileDBStorr$clear()`

Clear a storr.

#### Usage

    TileDBStorr$clear(namespace = self$default_namespace)

#### Arguments

- `namespace`:

  A scalar character of namespace name or `NULL` to clear all
  namespaces.

#### Returns

The number of deleted namespaces.

------------------------------------------------------------------------

### `TileDBStorr$exists()`

Check a key-namespace pair exists.

The arguments `key` and `namespace` can be recycled if any of them is a
scalar character and the other is a vector. No other recycling rule is
permitted.

#### Usage

    TileDBStorr$exists(key, namespace = self$default_namespace)

#### Arguments

- `key`:

  A character vector of key names.

- `namespace`:

  A character vector of namespaces.

#### Returns

A logical vector indicating which key-namespace pair exists.

------------------------------------------------------------------------

### `TileDBStorr$exists_object()`

Check a serialised object exists given a hash.

#### Usage

    TileDBStorr$exists_object(hash)

#### Arguments

- `hash`:

  A vector of hash values.

#### Returns

A logical vector indicating which object exists.

------------------------------------------------------------------------

### `TileDBStorr$del()`

Delete an object from the storr.

The arguments `key` and `namespace` can be recycled if any of them is a
scalar character and the other is a vector. No other recycling rule is
permitted.

#### Usage

    TileDBStorr$del(key, namespace = self$default_namespace)

#### Arguments

- `key`:

  A character vector of key names.

- `namespace`:

  A character vector of namespaces.

#### Returns

A logical vector indicating which key-namespace pair was deleted,
invisibly.

------------------------------------------------------------------------

### `TileDBStorr$keys_with_expiration()`

Get the key-namespace pairs with expiration timestamps.

#### Usage

    TileDBStorr$keys_with_expiration(namespace = self$default_namespace,
      datetimes = TRUE)

#### Arguments

- `namespace`:

  A character vector of namespaces or `NULL` for all namespaces.

- `datetimes`:

  Should the `expires_at` column be returned? Default is `TRUE`.

#### Returns

An object of class `data.table`.

------------------------------------------------------------------------

### `TileDBStorr$expired_keys()`

Get the expired key-namespace pairs.

#### Usage

    TileDBStorr$expired_keys(namespace = self$default_namespace,
      datetimes = TRUE)

#### Arguments

- `namespace`:

  A character vector of namespaces or `NULL` for all namespaces.

- `datetimes`:

  Should the `expires_at` column be returned? Default is `TRUE`.

#### Returns

An object of class `data.table`.

------------------------------------------------------------------------

### `TileDBStorr$has_expired_keys()`

Check for expired key-namespace pairs.

#### Usage

    TileDBStorr$has_expired_keys(namespace = self$default_namespace)

#### Arguments

- `namespace`:

  A character vector of namespaces or `NULL` for all namespaces.

#### Returns

`TRUE` for expired keys, `FALSE` otherwise.

------------------------------------------------------------------------

### `TileDBStorr$clear_expired_keys()`

Remove the expired key-namespace pairs.

#### Usage

    TileDBStorr$clear_expired_keys(namespace = self$default_namespace)

#### Arguments

- `namespace`:

  A character vector of namespaces or `NULL` for all namespaces.

#### Returns

A boolean value `TRUE` indicating success, invisibly.

------------------------------------------------------------------------

### `TileDBStorr$list()`

List all keys stored in a namespace.

#### Usage

    TileDBStorr$list(namespace = self$default_namespace)

#### Arguments

- `namespace`:

  A scalar character of namespace name.

#### Returns

A sorted character vector with keys.

------------------------------------------------------------------------

### `TileDBStorr$list_hashes()`

List all hashes stored in the storr.

#### Usage

    TileDBStorr$list_hashes()

#### Returns

A sorted character vector with hashes.

------------------------------------------------------------------------

### `TileDBStorr$list_namespaces()`

List all namespaces in the storr.

#### Usage

    TileDBStorr$list_namespaces()

#### Returns

A sorted character vector with namespaces.

------------------------------------------------------------------------

### `TileDBStorr$gc()`

Garbage collect the storr.

#### Usage

    TileDBStorr$gc(clear_expired = FALSE)

#### Arguments

- `clear_expired`:

  Should the expired keys be deleted? Default is `FALSE`.

#### Returns

A vector of unused hashes, invisibly.

------------------------------------------------------------------------

### `TileDBStorr$import()`

Import objects to storr.

#### Usage

    TileDBStorr$import(src, list = NULL, namespace = self$default_namespace,
      skip_missing = FALSE)

#### Arguments

- `src`:

  A source to import objects from. It can be a storr, list, or
  environment.

- `list`:

  Names of objects to import (or `NULL` for all objects) . If given it
  must be a character vector. If named, the names of the character
  vector will be the names of the objects as created in the storr.

- `namespace`:

  Namespace to get objects from, and to put objects into. If `NULL`, all
  namespaces from `src` will be imported. If named, then the same rule
  is followed as `list`; `namespace = c(a = b)` will import the contents
  of namespace `b` as namespace `a`.

- `skip_missing`:

  Logical, indicating if missing keys (specified in `list`) should be
  skipped over, rather than being treated as an error (the default).

#### Returns

A vector with destination namespaces, invisibly.

------------------------------------------------------------------------

### `TileDBStorr$export()`

Export objects from storr.

Use list() to export to a brand new list, or use as.list(object) for a
shorthand.

#### Usage

    TileDBStorr$export(dest, list = NULL, namespace = self$default_namespace,
      skip_missing = FALSE)

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

### `TileDBStorr$index_export()`

Generate a `data.table` with an index of objects present in a storr.

#### Usage

    TileDBStorr$index_export(namespace = NULL)

#### Arguments

- `namespace`:

  A character vector of namespaces or `NULL` for all namespaces.

#### Returns

An object of class `data.table`.

------------------------------------------------------------------------

### `TileDBStorr$index_import()`

Import an index of objects from a storr.

#### Usage

    TileDBStorr$index_import(index)

#### Arguments

- `index`:

  A `data.frame` with minimum required columns 'namespace', 'key' 'hash'
  and optionally 'expires_at' and 'notes'. It is an error if not all
  hashes are present in the storr.

#### Returns

`TRUE`, invisibly.

------------------------------------------------------------------------

### `TileDBStorr$export_tdb()`

Export objects from storr to another TileDB storr.

#### Usage

    TileDBStorr$export_tdb(key = character(0),
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

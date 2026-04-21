# Generate a `TileDBDriver` Object

An R6 class that represents a content addressed storage driver that
complies with 'storr' interface using TileDB Embedded as a back-end.

This class is intended for usage in
[storr::storr](https://richfitz.github.io/storr/reference/storr.html) or
[TileDBStorr](https://cgiachalis.github.io/storr.tiledb/reference/TileDBStorr.md).

## Value

A `TileDBDriver`, `R6` object.

## Super classes

[`R6.tiledb::TileDBObject`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html)
-\>
[`R6.tiledb::TileDBGroup`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.html)
-\>
[`storr.tiledb::CAS`](https://cgiachalis.github.io/storr.tiledb/reference/CAS.md)
-\> `TileDBDriver`

## Public fields

- `traits`:

  Driver traits (**immutable**).

- `binary`:

  Binary toggle, default is `FALSE` (**immutable**).

## Methods

### Public methods

- [`TileDBDriver$new()`](#method-TileDBDriver-new)

- [`TileDBDriver$type()`](#method-TileDBDriver-type)

- [`TileDBDriver$get_hash()`](#method-TileDBDriver-get_hash)

- [`TileDBDriver$mget_hash()`](#method-TileDBDriver-mget_hash)

- [`TileDBDriver$set_hash()`](#method-TileDBDriver-set_hash)

- [`TileDBDriver$mset_hash()`](#method-TileDBDriver-mset_hash)

- [`TileDBDriver$get_object()`](#method-TileDBDriver-get_object)

- [`TileDBDriver$mget_object()`](#method-TileDBDriver-mget_object)

- [`TileDBDriver$set_object()`](#method-TileDBDriver-set_object)

- [`TileDBDriver$mset_object()`](#method-TileDBDriver-mset_object)

- [`TileDBDriver$set_keymeta()`](#method-TileDBDriver-set_keymeta)

- [`TileDBDriver$mset_keymeta()`](#method-TileDBDriver-mset_keymeta)

- [`TileDBDriver$get_keymeta()`](#method-TileDBDriver-get_keymeta)

- [`TileDBDriver$mget_keymeta()`](#method-TileDBDriver-mget_keymeta)

- [`TileDBDriver$exists_hash()`](#method-TileDBDriver-exists_hash)

- [`TileDBDriver$exists_object()`](#method-TileDBDriver-exists_object)

- [`TileDBDriver$del_hash()`](#method-TileDBDriver-del_hash)

- [`TileDBDriver$del_object()`](#method-TileDBDriver-del_object)

- [`TileDBDriver$list_hashes()`](#method-TileDBDriver-list_hashes)

- [`TileDBDriver$list_namespaces()`](#method-TileDBDriver-list_namespaces)

- [`TileDBDriver$list_keys()`](#method-TileDBDriver-list_keys)

- [`TileDBDriver$list_unused_hashes()`](#method-TileDBDriver-list_unused_hashes)

- [`TileDBDriver$delete_unused_hashes()`](#method-TileDBDriver-delete_unused_hashes)

- [`TileDBDriver$delete_namespaces()`](#method-TileDBDriver-delete_namespaces)

- [`TileDBDriver$keys_with_expiration()`](#method-TileDBDriver-keys_with_expiration)

- [`TileDBDriver$keys_without_expiration()`](#method-TileDBDriver-keys_without_expiration)

- [`TileDBDriver$expired_keys()`](#method-TileDBDriver-expired_keys)

- [`TileDBDriver$unexpired_keys()`](#method-TileDBDriver-unexpired_keys)

- [`TileDBDriver$delete_expired_keys()`](#method-TileDBDriver-delete_expired_keys)

- [`TileDBDriver$num_expired_keys()`](#method-TileDBDriver-num_expired_keys)

- [`TileDBDriver$num_unexpired_keys()`](#method-TileDBDriver-num_unexpired_keys)

- [`TileDBDriver$has_expired_keys()`](#method-TileDBDriver-has_expired_keys)

- [`TileDBDriver$has_unexpired_keys()`](#method-TileDBDriver-has_unexpired_keys)

- [`TileDBDriver$export_tdb()`](#method-TileDBDriver-export_tdb)

Inherited methods

- [`R6.tiledb::TileDBObject$class()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-class)
- [`R6.tiledb::TileDBObject$exists()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-exists)
- [`R6.tiledb::TileDBObject$get_metadata()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-get_metadata)
- [`R6.tiledb::TileDBObject$is_open()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-is_open)
- [`R6.tiledb::TileDBObject$reopen()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-reopen)
- [`R6.tiledb::TileDBObject$set_metadata()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-set_metadata)
- [`R6.tiledb::TileDBGroup$count_members()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.html#method-count_members)
- [`R6.tiledb::TileDBGroup$delete()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.html#method-delete)
- [`R6.tiledb::TileDBGroup$get_member()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.html#method-get_member)
- [`R6.tiledb::TileDBGroup$get_members_df()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.html#method-get_members_df)
- [`R6.tiledb::TileDBGroup$member_exists()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.html#method-member_exists)
- [`R6.tiledb::TileDBGroup$names()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.html#method-names)
- [`R6.tiledb::TileDBGroup$print()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.html#method-print)
- [`R6.tiledb::TileDBGroup$remove()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.html#method-remove)
- [`R6.tiledb::TileDBGroup$set_member()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.html#method-set_member)
- [`storr.tiledb::CAS$close()`](https://cgiachalis.github.io/storr.tiledb/reference/CAS.html#method-close)
- [`storr.tiledb::CAS$create()`](https://cgiachalis.github.io/storr.tiledb/reference/CAS.html#method-create)
- [`storr.tiledb::CAS$destroy()`](https://cgiachalis.github.io/storr.tiledb/reference/CAS.html#method-destroy)
- [`storr.tiledb::CAS$dir_tree()`](https://cgiachalis.github.io/storr.tiledb/reference/CAS.html#method-dir_tree)
- [`storr.tiledb::CAS$dump()`](https://cgiachalis.github.io/storr.tiledb/reference/CAS.html#method-dump)
- [`storr.tiledb::CAS$filter_keys()`](https://cgiachalis.github.io/storr.tiledb/reference/CAS.html#method-filter_keys)
- [`storr.tiledb::CAS$open()`](https://cgiachalis.github.io/storr.tiledb/reference/CAS.html#method-open)

------------------------------------------------------------------------

### Method `new()`

Create a new `TileDBDriver` object.

#### Usage

    TileDBDriver$new(uri, ctx = NULL)

#### Arguments

- `uri`:

  URI path for the `TileDBDriver` object.

- `ctx`:

  Optional
  [`tiledb::tiledb_ctx()`](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_ctx.html)
  object.

------------------------------------------------------------------------

### Method `type()`

Driver type.

#### Usage

    TileDBDriver$type()

#### Returns

A character string.

------------------------------------------------------------------------

### Method `get_hash()`

Get hash values.

#### Usage

    TileDBDriver$get_hash(key, namespace)

#### Arguments

- `key`:

  A character vector of keys.

- `namespace`:

  A character vector of namespaces.

#### Returns

A vector of hashes.

------------------------------------------------------------------------

### Method `mget_hash()`

Get hash values.

#### Usage

    TileDBDriver$mget_hash(key, namespace)

#### Arguments

- `key`:

  A character vector of keys.

- `namespace`:

  A character vector of namespaces.

#### Returns

A vector of hashes.

------------------------------------------------------------------------

### Method `set_hash()`

Set hash values.

#### Usage

    TileDBDriver$set_hash(key, namespace, hash, expires_at, notes)

#### Arguments

- `key`:

  A character vector of keys.

- `namespace`:

  A character vector of namespaces.

- `hash`:

  A vector of hash values.

- `expires_at`:

  A vector of expiration datetimes.

- `notes`:

  A character vector of notes.

#### Returns

`TRUE` for successful operation, invisibly.

------------------------------------------------------------------------

### Method `mset_hash()`

Set hash values.

#### Usage

    TileDBDriver$mset_hash(key, namespace, hash, expires_at, notes)

#### Arguments

- `key`:

  A character vector of keys.

- `namespace`:

  A character vector of namespaces.

- `hash`:

  A vector of hash values.

- `expires_at`:

  A vector of expiration datetimes.

- `notes`:

  A character vector of notes.

#### Returns

`TRUE` for successful operation, invisibly.

------------------------------------------------------------------------

### Method `get_object()`

Get R object given a hash.

#### Usage

    TileDBDriver$get_object(hash)

#### Arguments

- `hash`:

  A hash value.

#### Returns

A de-serialized R object.

------------------------------------------------------------------------

### Method `mget_object()`

Get a list R objects given a hash vector.

#### Usage

    TileDBDriver$mget_object(hash)

#### Arguments

- `hash`:

  A vector of hash values.

#### Returns

A list with de-serialized R objects.

------------------------------------------------------------------------

### Method `set_object()`

Store serialised R objects.

#### Usage

    TileDBDriver$set_object(hash, value)

#### Arguments

- `hash`:

  A vector of hash values.

- `value`:

  A vector with serialised values.

#### Returns

`TRUE` for successful operation, invisibly.

------------------------------------------------------------------------

### Method `mset_object()`

Store serialised R objects.

#### Usage

    TileDBDriver$mset_object(hash, value)

#### Arguments

- `hash`:

  A vector of hash values.

- `value`:

  A vector with serialised values.

#### Returns

`TRUE` for successful operation, invisibly.

------------------------------------------------------------------------

### Method `set_keymeta()`

Set key-namespace metadata.

Sets a pair of expiry date-time and notes.

#### Usage

    TileDBDriver$set_keymeta(key, namespace, expires_at, notes)

#### Arguments

- `key`:

  A character vector of keys.

- `namespace`:

  A character vector of namespaces.

- `expires_at`:

  A vector of expiration datetimes.

- `notes`:

  A character vector of notes.

#### Returns

`TRUE` for successful operation, invisibly.

------------------------------------------------------------------------

### Method `mset_keymeta()`

Set multiple key-namespace metadata.

Sets a pair of expiry date-time and notes.

#### Usage

    TileDBDriver$mset_keymeta(key, namespace, expires_at, notes)

#### Arguments

- `key`:

  A character vector of keys.

- `namespace`:

  A character vector of namespaces.

- `expires_at`:

  A vector of expiration datetimes.

- `notes`:

  A character vector of notes.

#### Returns

`TRUE` for successful operation, invisibly.

------------------------------------------------------------------------

### Method `get_keymeta()`

Get key-namespace metadata.

#### Usage

    TileDBDriver$get_keymeta(key, namespace)

#### Arguments

- `key`:

  A single character key.

- `namespace`:

  A single character namespace.

#### Returns

A named list with key-metadata, `"expires_at"` and `"notes".`

------------------------------------------------------------------------

### Method `mget_keymeta()`

Get multiple key-namespace metadata.

#### Usage

    TileDBDriver$mget_keymeta(key, namespace, nomatch = NULL)

#### Arguments

- `key`:

  A character vector of keys.

- `namespace`:

  A character vector of namespaces.

- `nomatch`:

  Value to fill in case of no match.

#### Returns

A list with key metadata for each key-namespace pair. For not found
pairs will return the nomatch value.

------------------------------------------------------------------------

### Method `exists_hash()`

Check a key-namespace pair exists.

#### Usage

    TileDBDriver$exists_hash(key, namespace)

#### Arguments

- `key`:

  A character vector of keys.

- `namespace`:

  A character vector of namespaces.

#### Returns

A logical vector.

------------------------------------------------------------------------

### Method `exists_object()`

Check a serialised object exists.

#### Usage

    TileDBDriver$exists_object(hash)

#### Arguments

- `hash`:

  A vector of hash values.

#### Returns

A logical vector.

------------------------------------------------------------------------

### Method `del_hash()`

Delete a key/namespace pair.

#### Usage

    TileDBDriver$del_hash(key, namespace)

#### Arguments

- `key`:

  A character vector of keys.

- `namespace`:

  A character vector of namespaces.

#### Returns

A logical vector.

------------------------------------------------------------------------

### Method `del_object()`

Delete serialised objects.

#### Usage

    TileDBDriver$del_object(hash)

#### Arguments

- `hash`:

  A vector of hash values.

#### Returns

A logical vector.

------------------------------------------------------------------------

### Method `list_hashes()`

List all hash values.

#### Usage

    TileDBDriver$list_hashes()

#### Returns

A vector of hash values.

------------------------------------------------------------------------

### Method `list_namespaces()`

List all namespace values.

#### Usage

    TileDBDriver$list_namespaces()

#### Returns

A vector of namespace values.

------------------------------------------------------------------------

### Method `list_keys()`

List keys given a namespace.

#### Usage

    TileDBDriver$list_keys(namespace)

#### Arguments

- `namespace`:

  A single character namespace.

#### Returns

A vector of key values.

------------------------------------------------------------------------

### Method `list_unused_hashes()`

List unused hashes.

#### Usage

    TileDBDriver$list_unused_hashes()

#### Returns

A vector of hash values.

------------------------------------------------------------------------

### Method `delete_unused_hashes()`

Delete unused hashes.

#### Usage

    TileDBDriver$delete_unused_hashes()

#### Returns

A vector of deleted hash values, invisibly.

------------------------------------------------------------------------

### Method `delete_namespaces()`

Delete namespaces.

#### Usage

    TileDBDriver$delete_namespaces(ns)

#### Arguments

- `ns`:

  A character vector of namespaces or `NULL` for all namespaces.

#### Returns

A logical vector indicating successful deletion or not. `FALSE` means
the namespace was not found in database.

------------------------------------------------------------------------

### Method `keys_with_expiration()`

Get the key-namespace pairs with expiration timestamps.

#### Usage

    TileDBDriver$keys_with_expiration(namespace, datetimes = TRUE)

#### Arguments

- `namespace`:

  A character vector of namespaces or `NULL` for all namespaces.

- `datetimes`:

  Should the `expires_at` column be returned? Default is `TRUE`.

#### Returns

An `ArrowObject` object.

------------------------------------------------------------------------

### Method `keys_without_expiration()`

Get the key-namespace pairs without expiration timestamps.

#### Usage

    TileDBDriver$keys_without_expiration(namespace, datetimes = TRUE)

#### Arguments

- `namespace`:

  A character vector of namespaces or `NULL` for all namespaces.

- `datetimes`:

  Should the `expires_at` column be returned? Default is `TRUE`.

#### Returns

An `ArrowObject` object.

------------------------------------------------------------------------

### Method `expired_keys()`

Get the expired key-namespace pairs.

#### Usage

    TileDBDriver$expired_keys(namespace, datetimes = TRUE)

#### Arguments

- `namespace`:

  A character vector of namespaces or `NULL` for all namespaces.

- `datetimes`:

  Should the `expires_at` column be returned? Default is `TRUE`.

#### Returns

An `ArrowObject` object.

------------------------------------------------------------------------

### Method `unexpired_keys()`

Get the unexpired key-namespace pairs.

#### Usage

    TileDBDriver$unexpired_keys(namespace, datetimes = TRUE)

#### Arguments

- `namespace`:

  A character vector of namespaces or `NULL` for all namespaces.

- `datetimes`:

  Should the `expires_at` column be returned? Default is `TRUE`.

#### Returns

An `ArrowObject` object.

------------------------------------------------------------------------

### Method `delete_expired_keys()`

Get the expired key-namespace pairs.

#### Usage

    TileDBDriver$delete_expired_keys(namespace)

#### Arguments

- `namespace`:

  A character vector of namespaces or `NULL` for all namespaces.

#### Returns

A boolean value `TRUE` indicating success, invisibly.

------------------------------------------------------------------------

### Method `num_expired_keys()`

Get the number of expired key-namespace pairs.

#### Usage

    TileDBDriver$num_expired_keys(namespace)

#### Arguments

- `namespace`:

  A character vector of namespaces or `NULL` for all namespaces.

#### Returns

A numeric value.

------------------------------------------------------------------------

### Method `num_unexpired_keys()`

Get the number of unexpired key-namespace pairs.

#### Usage

    TileDBDriver$num_unexpired_keys(namespace)

#### Arguments

- `namespace`:

  A character vector of namespaces or `NULL` for all namespaces.

#### Returns

A numeric value.

------------------------------------------------------------------------

### Method `has_expired_keys()`

Check for expired key-namespace pairs.

#### Usage

    TileDBDriver$has_expired_keys(namespace)

#### Arguments

- `namespace`:

  A character vector of namespaces or `NULL` for all namespaces.

#### Returns

`TRUE` for expired keys, `FALSE` otherwise.

------------------------------------------------------------------------

### Method `has_unexpired_keys()`

Check for unexpired key-namespace pairs.

#### Usage

    TileDBDriver$has_unexpired_keys(namespace)

#### Arguments

- `namespace`:

  A character vector of namespaces or `NULL` for all namespaces.

#### Returns

`TRUE` for unexpired keys, `FALSE` otherwise.

------------------------------------------------------------------------

### Method `export_tdb()`

Export objects from storr to another TileDB storr.

#### Usage

    TileDBDriver$export_tdb(key, namespace, dest_driver)

#### Arguments

- `key`:

  A character vector of source keys.

- `namespace`:

  A character vector of namespaces or `NULL` for all namespaces.

- `dest_driver`:

  The destination TileDB driver, See
  [`driver_tiledb()`](https://cgiachalis.github.io/storr.tiledb/reference/driver_tiledb.md).

#### Returns

A logical `TRUE` indicating successful export, invisibly.

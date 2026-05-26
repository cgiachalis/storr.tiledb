# Generate a `TimeTravelDriver` Object

A
[TileDBDriver](https://cgiachalis.github.io/storr.tiledb/reference/TileDBDriver.md)
variant with read only class methods and time-travel support.

This class is intended for usage into
[StorrTimeTravel](https://cgiachalis.github.io/storr.tiledb/reference/StorrTimeTravel.md).

## Value

A `TimeTravelDriver`, `R6` object.

## Super classes

[`R6.tiledb::TileDBObject`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html)
-\>
[`R6.tiledb::TileDBGroup`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.html)
-\> `TimeTravelDriver`

## Public fields

- `traits`:

  Driver traits (**immutable**).

## Active bindings

- `timestamp`:

  Set or retrieve a `TileDB` timestamp range that the resource will be
  opened at. Effective in `"READ"` mode only.

- `hash_algorithm`:

  Hash algorithm

- `members_instantiated`:

  Have the members been instantiated?

## Methods

### Public methods

- [`TimeTravelDriver$new()`](#method-TimeTravelDriver-initialize)

- [`TimeTravelDriver$type()`](#method-TimeTravelDriver-type)

- [`TimeTravelDriver$open()`](#method-TimeTravelDriver-open)

- [`TimeTravelDriver$close()`](#method-TimeTravelDriver-close)

- [`TimeTravelDriver$filter_keys()`](#method-TimeTravelDriver-filter_keys)

- [`TimeTravelDriver$get_hash()`](#method-TimeTravelDriver-get_hash)

- [`TimeTravelDriver$mget_hash()`](#method-TimeTravelDriver-mget_hash)

- [`TimeTravelDriver$get_object()`](#method-TimeTravelDriver-get_object)

- [`TimeTravelDriver$mget_object()`](#method-TimeTravelDriver-mget_object)

- [`TimeTravelDriver$get_keymeta()`](#method-TimeTravelDriver-get_keymeta)

- [`TimeTravelDriver$mget_keymeta()`](#method-TimeTravelDriver-mget_keymeta)

- [`TimeTravelDriver$exists_hash()`](#method-TimeTravelDriver-exists_hash)

- [`TimeTravelDriver$exists_object()`](#method-TimeTravelDriver-exists_object)

- [`TimeTravelDriver$list_hashes()`](#method-TimeTravelDriver-list_hashes)

- [`TimeTravelDriver$list_namespaces()`](#method-TimeTravelDriver-list_namespaces)

- [`TimeTravelDriver$list_keys()`](#method-TimeTravelDriver-list_keys)

- [`TimeTravelDriver$list_unused_hashes()`](#method-TimeTravelDriver-list_unused_hashes)

- [`TimeTravelDriver$keys_with_expiration()`](#method-TimeTravelDriver-keys_with_expiration)

- [`TimeTravelDriver$keys_without_expiration()`](#method-TimeTravelDriver-keys_without_expiration)

- [`TimeTravelDriver$expired_keys()`](#method-TimeTravelDriver-expired_keys)

- [`TimeTravelDriver$unexpired_keys()`](#method-TimeTravelDriver-unexpired_keys)

- [`TimeTravelDriver$num_expired_keys()`](#method-TimeTravelDriver-num_expired_keys)

- [`TimeTravelDriver$num_unexpired_keys()`](#method-TimeTravelDriver-num_unexpired_keys)

- [`TimeTravelDriver$has_expired_keys()`](#method-TimeTravelDriver-has_expired_keys)

- [`TimeTravelDriver$has_unexpired_keys()`](#method-TimeTravelDriver-has_unexpired_keys)

- [`TimeTravelDriver$export_tdb()`](#method-TimeTravelDriver-export_tdb)

Inherited methods

- [`R6.tiledb::TileDBObject$class()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-class)
- [`R6.tiledb::TileDBObject$exists()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-exists)
- [`R6.tiledb::TileDBObject$get_metadata()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-get_metadata)
- [`R6.tiledb::TileDBObject$is_open()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-is_open)
- [`R6.tiledb::TileDBObject$reopen()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-reopen)
- [`R6.tiledb::TileDBObject$set_metadata()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-set_metadata)
- [`R6.tiledb::TileDBGroup$count_members()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.html#method-count_members)
- [`R6.tiledb::TileDBGroup$create()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.html#method-create)
- [`R6.tiledb::TileDBGroup$delete()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.html#method-delete)
- [`R6.tiledb::TileDBGroup$dump()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.html#method-dump)
- [`R6.tiledb::TileDBGroup$get_member()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.html#method-get_member)
- [`R6.tiledb::TileDBGroup$get_members_df()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.html#method-get_members_df)
- [`R6.tiledb::TileDBGroup$member_exists()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.html#method-member_exists)
- [`R6.tiledb::TileDBGroup$names()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.html#method-names)
- [`R6.tiledb::TileDBGroup$print()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.html#method-print)
- [`R6.tiledb::TileDBGroup$remove()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.html#method-remove)
- [`R6.tiledb::TileDBGroup$set_member()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.html#method-set_member)

------------------------------------------------------------------------

### `TimeTravelDriver$new()`

Instantiate a new `TimeTravelDriver` object.

#### Usage

    TimeTravelDriver$new(uri, ctx = NULL, timestamp = NULL)

#### Arguments

- `uri`:

  URI path to `TileDB` driver.

- `ctx`:

  Optional
  [tiledb_ctx](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_ctx.html)
  object.

- `timestamp`:

  Set a `TileDB` timestamp range that the resource will be opened at.
  Effective in `"READ"` mode only. Valid options:

  - A `NULL` value (default)

  - An `R` object coercible to `POSIXct` with length 1 which is used for
    end timestamp, or length 2 with start, end timestamps

  - An object of class `tiledb_timestamp`. See
    [`set_tiledb_timestamp()`](https://cgiachalis.github.io/R6.tiledb/reference/set_tiledb_timestamp.html)

------------------------------------------------------------------------

### `TimeTravelDriver$type()`

Driver type.

#### Usage

    TimeTravelDriver$type()

#### Returns

A character string.

------------------------------------------------------------------------

### `TimeTravelDriver$open()`

Open `TimeTravelDriver` object for read or write.

Setting`instantiate` argument to `TRUE`, all members will be
instantiated and cached on opening. They can be accessed via `members`
active field, i.e., using `<member>$object` element.

#### Usage

    TimeTravelDriver$open(mode = c("READ", "WRITE"), instantiate = FALSE)

#### Arguments

- `mode`:

  Mode to open : either `"READ"` or `"WRITE"`. Default is `"READ"`.

- `instantiate`:

  Should be all members be instantiated at opening? Default is `FALSE`.

#### Returns

The object, invisibly.

------------------------------------------------------------------------

### `TimeTravelDriver$close()`

Close the group object.

All instantiated group members will be closed if opened, and before
closing the group object.

#### Usage

    TimeTravelDriver$close()

#### Returns

The object, invisibly.

------------------------------------------------------------------------

### `TimeTravelDriver$filter_keys()`

Filter `tbl_keys` by key and namespace

#### Usage

    TimeTravelDriver$filter_keys(key, namespace, attrnames = character())

#### Arguments

- `key`:

  A character vector of keys.

- `namespace`:

  A character vector of namespaces.

- `attrnames`:

  A character vector with tiledb attributes (columns).

#### Returns

A \`data.table.

------------------------------------------------------------------------

### `TimeTravelDriver$get_hash()`

Get hash values.

#### Usage

    TimeTravelDriver$get_hash(key, namespace)

#### Arguments

- `key`:

  A character vector of keys.

- `namespace`:

  A character vector of namespaces.

#### Returns

A vector of hashes.

------------------------------------------------------------------------

### `TimeTravelDriver$mget_hash()`

Get hash values.

#### Usage

    TimeTravelDriver$mget_hash(key, namespace)

#### Arguments

- `key`:

  A character vector of keys.

- `namespace`:

  A character vector of namespaces.

#### Returns

A vector of hashes.

------------------------------------------------------------------------

### `TimeTravelDriver$get_object()`

Get R object given a hash.

#### Usage

    TimeTravelDriver$get_object(hash)

#### Arguments

- `hash`:

  A hash value.

#### Returns

A de-serialized R object.

------------------------------------------------------------------------

### `TimeTravelDriver$mget_object()`

Get a list R objects given a hash vector.

#### Usage

    TimeTravelDriver$mget_object(hash)

#### Arguments

- `hash`:

  A vector of hash values.

#### Returns

A list with de-serialized R objects.

------------------------------------------------------------------------

### `TimeTravelDriver$get_keymeta()`

Get key-namespace metadata.

#### Usage

    TimeTravelDriver$get_keymeta(key, namespace)

#### Arguments

- `key`:

  A single character key.

- `namespace`:

  A single character namespace.

#### Returns

A named list with key-metadata, `"expires_at"` and `"notes".`

------------------------------------------------------------------------

### `TimeTravelDriver$mget_keymeta()`

Get multiple key-namespace metadata.

#### Usage

    TimeTravelDriver$mget_keymeta(key, namespace, nomatch = NULL)

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

### `TimeTravelDriver$exists_hash()`

Check a key-namespace pair exists.

#### Usage

    TimeTravelDriver$exists_hash(key, namespace)

#### Arguments

- `key`:

  A character vector of keys.

- `namespace`:

  A character vector of namespaces.

#### Returns

A logical vector.

------------------------------------------------------------------------

### `TimeTravelDriver$exists_object()`

Check a serialised object exists.

#### Usage

    TimeTravelDriver$exists_object(hash)

#### Arguments

- `hash`:

  A vector of hash values.

#### Returns

A logical vector.

------------------------------------------------------------------------

### `TimeTravelDriver$list_hashes()`

List all hash values.

#### Usage

    TimeTravelDriver$list_hashes()

#### Returns

A vector of hash values.

------------------------------------------------------------------------

### `TimeTravelDriver$list_namespaces()`

List all namespace values.

#### Usage

    TimeTravelDriver$list_namespaces()

#### Returns

A vector of namespace values.

------------------------------------------------------------------------

### `TimeTravelDriver$list_keys()`

List keys given a namespace.

#### Usage

    TimeTravelDriver$list_keys(namespace)

#### Arguments

- `namespace`:

  A single character namespace.

#### Returns

A vector of key values.

------------------------------------------------------------------------

### `TimeTravelDriver$list_unused_hashes()`

List unused hashes.

#### Usage

    TimeTravelDriver$list_unused_hashes()

#### Returns

A vector of hash values.

------------------------------------------------------------------------

### `TimeTravelDriver$keys_with_expiration()`

Get the key-namespace pairs with expiration timestamps.

#### Usage

    TimeTravelDriver$keys_with_expiration(namespace, datetimes = TRUE)

#### Arguments

- `namespace`:

  A character vector of namespaces or `NULL` for all namespaces.

- `datetimes`:

  Should the `expires_at` column be returned? Default is `TRUE`.

#### Returns

An `ArrowObject` object.

------------------------------------------------------------------------

### `TimeTravelDriver$keys_without_expiration()`

Get the key-namespace pairs without expiration timestamps.

#### Usage

    TimeTravelDriver$keys_without_expiration(namespace, datetimes = TRUE)

#### Arguments

- `namespace`:

  A character vector of namespaces or `NULL` for all namespaces.

- `datetimes`:

  Should the `expires_at` column be returned? Default is `TRUE`.

#### Returns

An `ArrowObject` object.

------------------------------------------------------------------------

### `TimeTravelDriver$expired_keys()`

Get the expired key-namespace pairs.

#### Usage

    TimeTravelDriver$expired_keys(namespace, datetimes = TRUE)

#### Arguments

- `namespace`:

  A character vector of namespaces or `NULL` for all namespaces.

- `datetimes`:

  Should the `expires_at` column be returned? Default is `TRUE`.

#### Returns

An `ArrowObject` object.

------------------------------------------------------------------------

### `TimeTravelDriver$unexpired_keys()`

Get the unexpired key-namespace pairs.

#### Usage

    TimeTravelDriver$unexpired_keys(namespace, datetimes = TRUE)

#### Arguments

- `namespace`:

  A character vector of namespaces or `NULL` for all namespaces.

- `datetimes`:

  Should the `expires_at` column be returned? Default is `TRUE`.

#### Returns

An `ArrowObject` object.

------------------------------------------------------------------------

### `TimeTravelDriver$num_expired_keys()`

Get the number of expired key-namespace pairs.

#### Usage

    TimeTravelDriver$num_expired_keys(namespace)

#### Arguments

- `namespace`:

  A character vector of namespaces or `NULL` for all namespaces.

#### Returns

A numeric value.

------------------------------------------------------------------------

### `TimeTravelDriver$num_unexpired_keys()`

Get the number of unexpired key-namespace pairs.

#### Usage

    TimeTravelDriver$num_unexpired_keys(namespace)

#### Arguments

- `namespace`:

  A character vector of namespaces or `NULL` for all namespaces.

#### Returns

A numeric value.

------------------------------------------------------------------------

### `TimeTravelDriver$has_expired_keys()`

Check for expired key-namespace pairs.

#### Usage

    TimeTravelDriver$has_expired_keys(namespace)

#### Arguments

- `namespace`:

  A character vector of namespaces or `NULL` for all namespaces.

#### Returns

`TRUE` for expired keys, `FALSE` otherwise.

------------------------------------------------------------------------

### `TimeTravelDriver$has_unexpired_keys()`

Check for unexpired key-namespace pairs.

#### Usage

    TimeTravelDriver$has_unexpired_keys(namespace)

#### Arguments

- `namespace`:

  A character vector of namespaces or `NULL` for all namespaces.

#### Returns

`TRUE` for unexpired keys, `FALSE` otherwise.

------------------------------------------------------------------------

### `TimeTravelDriver$export_tdb()`

Export objects from storr to another TileDB storr.

#### Usage

    TimeTravelDriver$export_tdb(key, namespace, dest_driver)

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

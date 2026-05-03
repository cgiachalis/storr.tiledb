# Generate a `CAS` Object

An R6 class for creating a content addressable storage.

## Value

A `CAS` object.

## Super classes

[`R6.tiledb::TileDBObject`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html)
-\>
[`R6.tiledb::TileDBGroup`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBGroup.html)
-\> `CAS`

## Active bindings

- `hash_algorithm`:

  Hash algorithm

- `members_instantiated`:

  Have the members been instantiated?

- `size`:

  Return directory size

## Methods

### Public methods

- [`CAS$create()`](#method-CAS-create)

- [`CAS$open()`](#method-CAS-open)

- [`CAS$close()`](#method-CAS-close)

- [`CAS$destroy()`](#method-CAS-destroy)

- [`CAS$filter_keys()`](#method-CAS-filter_keys)

- [`CAS$dir_tree()`](#method-CAS-dir_tree)

- [`CAS$dump()`](#method-CAS-dump)

- [`CAS$clone()`](#method-CAS-clone)

Inherited methods

- [`R6.tiledb::TileDBObject$class()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-class)
- [`R6.tiledb::TileDBObject$exists()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-exists)
- [`R6.tiledb::TileDBObject$get_metadata()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-get_metadata)
- [`R6.tiledb::TileDBObject$initialize()`](https://cgiachalis.github.io/R6.tiledb/reference/TileDBObject.html#method-initialize)
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

------------------------------------------------------------------------

### `CAS$create()`

Create CAS.

#### Usage

    CAS$create(compression_level = -7, algo = NULL, keep_open = TRUE)

#### Arguments

- `compression_level`:

  Set an integer value for ZSTD compression level.

- `algo`:

  Select a hash algorithm to be used.

- `keep_open`:

  Should `CAS` be kept opened after creation? Default is `TRUE`; the
  mode will be `"WRITE"`.

#### Returns

The object, invisibly

------------------------------------------------------------------------

### `CAS$open()`

Open `CAS` object for read or write.

Setting`instantiate` argument to `TRUE`, all members will be
instantiated and cached on opening. They can be accessed via `members`
active field, i.e., using `<member>$object` element.

#### Usage

    CAS$open(mode = c("READ", "WRITE"), instantiate = FALSE)

#### Arguments

- `mode`:

  Mode to open : either `"READ"` or `"WRITE"`. Default is `"READ"`.

- `instantiate`:

  Should be all members be instantiated at opening? Default is `FALSE`.

#### Returns

The object, invisibly.

------------------------------------------------------------------------

### `CAS$close()`

Close the group object.

All instantiated group members will be closed if opened, and before
closing the group object.

#### Usage

    CAS$close()

#### Returns

The object, invisibly.

------------------------------------------------------------------------

### `CAS$destroy()`

Delete CAS.

#### Usage

    CAS$destroy()

#### Returns

The object, invisibly.

------------------------------------------------------------------------

### `CAS$filter_keys()`

Filter `tbl_keys` by key and namespace

#### Usage

    CAS$filter_keys(key, namespace, attrnames = character())

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

### `CAS$dir_tree()`

Print directory contents.

#### Usage

    CAS$dir_tree(what = c("all", "keys", "data"))

#### Arguments

- `what`:

  Select directory: 'all' for storr, 'keys' for `tbl_keys` array and
  'data' for `tbl_data` array.

#### Returns

A character vector with file paths, invisibly.

------------------------------------------------------------------------

### `CAS$dump()`

Dump the Storr structure to string.

#### Usage

    CAS$dump()

#### Returns

A `character` string, invisibly.

------------------------------------------------------------------------

### `CAS$clone()`

The objects of this class are cloneable with this method.

#### Usage

    CAS$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

# TileDB Storr Driver

Storr driver using TileDB storage engine.

## Usage

``` r
driver_tiledb(uri, context = NULL, init = FALSE, ...)

driver_tiledb_create(uri, hash_algorithm = NULL, compression_level = -7,
  context = NULL)
```

## Arguments

- uri:

  The URI path of storr.

- context:

  Optional
  [tiledb_ctx](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_ctx.html)
  object.

- init:

  Should the driver be created if not exist? Default is `FALSE`.

- ...:

  Other arguments passed to driver's create method when `init = TRUE`.
  Valid arguments: `hash_algorithm`, `compression_level` and
  `keep_open`.

- hash_algorithm:

  Select a hash algorithm supported by
  [digest](https://eddelbuettel.github.io/digest/man/digest.html):
  'md5', 'sha1', 'crc32', 'sha256', 'sha512', 'xxhash32', 'xxhash64',
  'murmur32', 'spookyhash', 'blake3', 'crc32c', 'xxh3_64', 'xxh3_128'.
  If not given, the default is 'md5'.

- compression_level:

  Set an integer value for ZSTD compression level.

## Value

- *driver_tiledb()* returns a
  [TileDBDriver](https://cgiachalis.github.io/storr.tiledb/reference/TileDBDriver.md)
  object.

- *driver_tiledb_create()* returns logical `TRUE` invisibly, for
  successful driver creation.

## Details

`driver_tiledb()` returns the TileDB driver given a URI path. You can
create a new driver using `init` argument or with
`driver_tiledb_create()`.

Note that
[`storr_tiledb()`](https://cgiachalis.github.io/storr.tiledb/reference/storr_tiledb.md)
abstracts the driver creation, so it's the preferred way unless you want
to pass the driver onto
[storr](https://richfitz.github.io/storr/reference/storr.html).

## Class Methods Summary

For complete definitions, see **Methods** section in
[TileDBDriver](https://cgiachalis.github.io/storr.tiledb/reference/TileDBDriver.md)
and its parent class
[CAS](https://cgiachalis.github.io/storr.tiledb/reference/CAS.md).

**Active Fields**

- **`hash_algorithm`** - Property for managing the hash algorithm
  (read/write)

- **`members_instantiated`** - Property indicating instantiation status
  (read-only)

- **`size`** - Get storr size (read-only)

**Lifecycle**

- **[`open()`](https://rdrr.io/r/base/connections.html)** - Opens the
  driver for read/write operations

- **`create()`** - Create a new driver object

- **[`close()`](https://rdrr.io/r/base/connections.html)** - Closes the
  driver

- **`destroy()`** - Destroy/delete the driver

**Hash Management**

- **`get_hash()`** and **`mget_hash()`** - Retrieve hash values for a
  given key and namespace

- **`set_hash()`** and **`mset_hash()`** - Set hash values with metadata
  like expiry date-times and notes

- **`exists_hash()`** - Verify the existence of specific keys,
  namespaces

- **`del_hash()`** - Remove key-namespace pairs

- **`delete_namespaces()`** - Clear namespaces or delete specified ones

- **`list_namespaces()`**, **`list_keys()`** - Retrieve all namespaces
  or keys for a given namespace

**Object Management**

- **`get_object()`** and **`mget_object()`** - Fetch serialized R
  objects using hash values

- **`set_object()`** and **`mset_object()`** - Store serialized R
  objects

- **`exists_object()`** - Verify the existence of specific objects

- **`del_object()`** - Remove serialized objects.

- **`delete_unused_hashes()`** - Remove hashes that are not in active
  use

- **`list_hashes()`** - Retrieve all hashes

- **`list_unused_hashes()`** - Identify unused hashes

**Key-Namespace Metadata**

- **`get_keymeta()`**, **`set_keymeta()`**, and **`mset_keymeta()`** -
  Manage metadata such as expiry times and notes for key-namespace pairs

**Expiration Management**

- **`keys_with_expiration`** and **`keys_without_expiration`** -
  Retrieve the key namespace pairs with or without expiration timestamps

- **`expired_keys()`** and **`unexpired_keys()`** - Retrieve the
  (un)expired key namespace pairs

- **`delete_expired_keys()`** - Delete all expired keys or for specific
  namespaces

- **`num_expired_keys()`** and **`num_unexpired_keys()`** - Get the
  number of (un)expired keys or for specific namespaces

- **`has_expired_keys()`** and **`has_unexpired_keys()`** - Verify the
  existence of (un)expired keys or for specific namespaces

**Export Utilities**

- **`export_tdb()`** - Export objects to another TileDB storr

**Directory Methods**

- **`dir_tree()`** - Displays directory structure

- **[`dump()`](https://rdrr.io/r/base/dump.html)** - Outputs a string
  representation of storr structure

## Data Model

The underlying structure is similar to
[driver_dbi()](https://richfitz.github.io/storr/reference/storr_dbi.html);
a content-addressed database that consists of two (array) tables.

Specifically, the driver storage is a TileDB Group with two member
Arrays which are stored relative to Group's URI path. The next
sub-sections describe the group structure and array data models.

**Group**

*Members*

1.  `tbl_keys` (array): maps key-namespace pairs to hashes (and to
    expiry and/or notes, optional)

2.  `tbl_data` (array): maps hashes to values (serialised R objects)

*Metadata*

- `hash_algo`: The name of hash algorithm.

- `type`: Group identifier, `"storr"`

**Arrays**

*tbl_keys* - A 2D sparse array that maps key-namespace pairs to hashes
and key-metadata.

- Dimensions: `namespace` (*ASCII*) and `key` (*ASCII*)

- Attributes: `hash` (*ASCII*), `expires_at` (*DATETIME_MS*) and `notes`
  (*UTF8*)

*tbl_data* - A 1D sparse array that maps hashes to object values.

- Dimensions: `hash` (*ASCII*)

- Attributes: `value` (*ASCII*)

TileDB datatypes in parentheses.

## See also

[`storr_tiledb()`](https://cgiachalis.github.io/storr.tiledb/reference/storr_tiledb.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# URI path
uri <- tempfile()

# create driver
dr <- driver_tiledb(uri, init = TRUE)

dr$print()
# R6Class: <TileDBDriver>
#  → URI Basename: file6bb0182c1362
#   • Arrays: "tbl_keys" and "tbl_data"

# members
dr$names()
# "tbl_keys" "tbl_data"
} # }
```

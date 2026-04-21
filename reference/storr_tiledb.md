# A Storr using TileDB Engine

Create a 'storr' using TileDB driver for storage.

## Usage

``` r
storr_tiledb(uri, default_namespace = "objects", context = NULL,
  init = FALSE, hash_algorithm = NULL, async = FALSE, ...)
```

## Arguments

- uri:

  The URI path of storr.

- default_namespace:

  The default namespace: `"objects"`.

- context:

  Optional
  [tiledb_ctx](https://tiledb-inc.github.io/TileDB-R/reference/tiledb_ctx.html)
  object.

- init:

  Should the driver be created if not exist? Default is `FALSE`.

- hash_algorithm:

  Select a hash algorithm supported by
  [digest](https://eddelbuettel.github.io/digest/man/digest.html):
  'md5', 'sha1', 'crc32', 'sha256', 'sha512', 'xxhash32', 'xxhash64',
  'murmur32', 'spookyhash', 'blake3', 'crc32c', 'xxh3_64', 'xxh3_128'.
  If not given, the default is 'md5'.

- async:

  Should the
  [mirai::mirai](https://mirai.r-lib.org/reference/mirai.html) daemons
  be enabled for async functions? Default is `FALSE`.

- ...:

  Other arguments passed to driver when `init = TRUE`. Valid arguments:
  `compression_level` and `keep_open`.

## Value

An object of class
[TileDBStorr](https://cgiachalis.github.io/storr.tiledb/reference/TileDBStorr.md),
R6.

## Details

[‘storr’](https://richfitz.github.io/storr/reference/storr.html) is a
content addressed key-value store with an optional caching layer.

The `storr_tiledb` generates a
[TileDBStorr](https://cgiachalis.github.io/storr.tiledb/reference/TileDBStorr.md)
object with identical interface as `storr` that additionally supports
metadata next to key-values (notes and expiration timestamps) as well as
asynchronous writes using the
[mirai](https://cran.r-project.org/web/packages/mirai/index.html)
framework.

`storr_tiledb()` and `storr(driver_tiledb())` can not be used
interchangeably if you use the extra features (i.e., expiration
timestamps). The latter is the standard storr interface and the former
produces a stand-alone R6 class that replicates the storr interface with
additional features.

Another difference, but not visible to the user, is that the
`storr_tiledb`'s cache layer uses hash tables via
[hashtab()](https://rdrr.io/r/utils/hashtab.html) instead of
environments.

### Cache option

By default, the in-memory caching layer is enabled. The global option
`storr.tiledb.cache` can be used to disable it, like so:
`options(storr.tiledb.cache = FALSE)`.

### Buffer size

The buffer allocation size is set to 3 MB per column when fetching data.
Use
[`tiledb::set_allocation_size_preference()`](https://tiledb-inc.github.io/TileDB-R/reference/save_allocation_size_preference.html)
to set a different limit.

## Class Methods Summary

For complete definitions, see **Methods** section in
[TileDBStorr](https://cgiachalis.github.io/storr.tiledb/reference/TileDBStorr.md).

**Active Fields**

- **`async_info`** - Get mirai daemon information (read-only)

- **`size`** - Get storr size (read-only)

**Initialisation & Lifecycle**

- **`new()`** - Initialise a TileDBStorr object with a TileDB driver,
  default namespace, and optional async support

- **`destroy()`** - Destroy/delete the storr and clean up the driver

**Cache Management**

- **`flush_cache()`** - Remove all items from both object and metadata
  hash tables

**Single Key-Value Operations**

- **`set()`** - Set a key-value pair with optional metadata (expires_at,
  notes)

- **[`get()`](https://rdrr.io/r/base/get.html)** - Retrieve an object by
  key-namespace pair

- **`set_by_value()`** - Set a key-value pair using the object's hash as
  the key

- **`get_value()`** - Retrieve an object given its hash

**Multiple Key-Value Operations**

- **`mset()`** - Set multiple key-value pairs in batch

- **[`mget()`](https://rdrr.io/r/base/get.html)** - Get multiple objects
  by keys

- **`mset_by_value()`** - Set multiple key-value pairs using their
  hashes as keys

- **`mget_value()`** - Get multiple objects by their hashes

**Asynchronous Operations**

- **`set_async()`** - Set a key-value pair asynchronously using mirai

- **`mset_async()`** - Set multiple key-value pairs asynchronously

- **`set_by_value_async()`** - Set key-value pair using hash,
  asynchronously

- **`mset_by_value_async()`** - Set multiple key-value pairs using
  hashes, asynchronously

**Metadata Operations**

- **`set_keymeta()`** - Set metadata (expires_at, notes) for a key

- **`mset_keymeta()`** - Set metadata for multiple keys

- **`get_keymeta()`** - Retrieve metadata for a key

- **`mget_keymeta()`** - Retrieve metadata for multiple keys

- **`set_keymeta_async()`** - Set metadata asynchronously

- **`mset_keymeta_async()`** - Set multiple metadata asynchronously

- **`clr_keymeta()`** - Clear metadata (set to NA) for key(s)

- **`clr_keymeta_async()`** - Clear metadata asynchronously

**Object Hash Management**

- **`set_value()`** - Add an R object and return its hash (internal use)

- **`mset_value()`** - Add multiple R objects and return their hashes
  (internal use)

- **`get_hash()`** - Get hash value for a key-namespace pair

- **`mget_hash()`** - Get hash values for multiple keys

- **`hash_object()`** - Create a hash digest for an R object

**Key Management**

- **[`exists()`](https://rdrr.io/r/base/exists.html)** - Check if
  key-namespace pair(s) exist

- **`exists_object()`** - Check if object(s) with given hash(es) exist

- **`del()`** - Delete key-namespace pair(s)

- **`fill()`** - Set one or more keys to the same value

- **`duplicate()`** - Duplicate/copy keys from source to destination

**Expiration Management**

- **`keys_with_expiration()`** - List keys that have expiration
  timestamps

- **`expired_keys()`** - Get keys that have already expired

- **`has_expired_keys()`** - Check if any keys are expired

- **`clear_expired_keys()`** - Remove expired key-namespace pairs

**Listing**

- **[`list()`](https://rdrr.io/r/base/list.html)** - List all keys in a
  namespace

- **`list_hashes()`** - List all stored object hashes

- **`list_namespaces()`** - List all namespaces

**Storage Management**

- **`clear()`** - Clear a namespace or all namespaces

- **[`gc()`](https://rdrr.io/r/base/gc.html)** - Garbage collect unused
  hashes

- **`index_export()`** - Export object index as data.table

- **`index_import()`** - Import objects from index

- **`import()`** - Import objects from another storr/list/environment

- **`export()`** - Export objects to another storr/list/environment

- **`export_tdb()`** - Export objects to another TileDB storr

## See also

[`driver_tiledb()`](https://cgiachalis.github.io/storr.tiledb/reference/driver_tiledb.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# URI path
uri <- tempfile()
sto <- storr_tiledb(uri, init = TRUE)

# set key-values
sto$set("a", 1)
sto$set("b", 1, namespace = "ns1", notes = "note1")

# listing methods
sto$list("ns1") # b
sto$list_namespaces() # "ns1"     "objects"
sto$list_hashes() # "632336c518ae1c89ecf26ae5fbec5860"

# get methods
sto$get("a") # 1
sto$get("b", "ns1") # 1
sto$get_keymeta("b", "ns1") # list(exprires_at = NA, notes = "note1")

#-----------------------------------------------------------------
#   Storr with encryption
#-----------------------------------------------------------------

# Requires a TileDB Context with encryption configuration parameters
key <- "0123456789abcdeF0123456789abcdeF"
config <- tiledb::tiledb_config()
config["sm.encryption_type"] <- "AES_256_GCM";
config["sm.encryption_key"] <- key
ctx <- tiledb::tiledb_ctx(config)

# Create a storr with context that encapsulates encryption configuration
uri_enc <- tempfile()
stoe <- storr_tiledb(uri_enc, init = TRUE, context = ctx)

stoe$set("a", 1)
stoe$get("a") # 1

# No access without the key
# stoe_new <- storr_tiledb(uri_enc) # This will fail

# Pass the context with encryption parameters
stoe_new <- storr_tiledb(uri_enc, context = ctx)
stoe_new$get("a") # 1
} # }

```

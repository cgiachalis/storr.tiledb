# A Storr with Time-Travel

Generate an instance of
[StorrTimeTravel](https://cgiachalis.github.io/storr.tiledb/reference/StorrTimeTravel.md),
a variant of
[TileDBStorr](https://cgiachalis.github.io/storr.tiledb/reference/TileDBStorr.md)
designed to query key-value data at specific points in time and in
read-only mode with no write capabilities.

## Usage

``` r
storr_timetravel(uri, default_namespace = "objects", context = NULL,
  timestamp = NULL)
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

- timestamp:

  Set a `TileDB` timestamp range that the resource will be opened at.
  Effective only for `"READ"` mode. Valid options:

  - A `NULL` value (default)

  - An `R` object coercible to `POSIXct` with length 1 which used for
    end timestamp, or length 2 with start, end timestamps

  - An object of class `tiledb_timestamp`. See
    [`R6.tiledb::set_tiledb_timestamp()`](https://cgiachalis.github.io/R6.tiledb/reference/set_tiledb_timestamp.html)

  Set a new timestamp with active field `$timestamp`, see examples.

## Value

An object of class
[StorrTimeTravel](https://cgiachalis.github.io/storr.tiledb/reference/StorrTimeTravel.md),
R6.

## Class Methods Summary

For complete definitions, see **Methods** section in
[StorrTimeTravel](https://cgiachalis.github.io/storr.tiledb/reference/StorrTimeTravel.md).

**Active Fields**

- **`timestamp`** - Get or set a TileDB timestamp range that the 'storr'
  will be opened at.

**Initialisation & Lifecycle**

- **`new()`** - Initialise a StorrTimeTravel object with a TileDB
  driver, default namespace, and optional timestamp

**Single Key-Value Operations**

- **[`get()`](https://rdrr.io/r/base/get.html)** - Retrieve an object by
  key-namespace pair

- **`get_value()`** - Retrieve an object given its hash

**Multiple Key-Value Operations**

- **[`mget()`](https://rdrr.io/r/base/get.html)** - Get multiple objects
  by keys

- **`mget_value()`** - Get multiple objects by their hashes

**Metadata Operations**

- **`get_keymeta()`** - Retrieve metadata for a key

- **`mget_keymeta()`** - Retrieve metadata for multiple keys

**Object Hash Management**

- **`get_hash()`** - Get hash value for a key-namespace pair

- **`mget_hash()`** - Get hash values for multiple keys

**Key Management**

- **[`exists()`](https://rdrr.io/r/base/exists.html)** - Check if
  key-namespace pair(s) exist

- **`exists_object()`** - Check if object(s) with given hash(es) exist

**Expiration Management**

- **`keys_with_expiration()`** - List keys that have expiration
  timestamps

- **`expired_keys()`** - Get keys that have already expired

- **`has_expired_keys()`** - Check if any keys are expired

**Listing**

- **[`list()`](https://rdrr.io/r/base/list.html)** - List all keys in a
  namespace

- **`list_hashes()`** - List all stored object hashes

- **`list_namespaces()`** - List all namespaces

**Storage Management**

- **`index_export()`** - Export object index as data.table

- **`export()`** - Export objects to another storr/list/environment

- **`export_tdb()`** - Export objects to another TileDB storr

## Examples

``` r
if (FALSE) { # \dontrun{
# URI path
uri <- tempfile()
sto <- storr_tiledb(uri, init = TRUE)

# set key-values
t0 <- Sys.time()
sto$set("a", 1)

t1 <- Sys.time()
sto$set("b", 2)

t2 <- Sys.time()

# Open storr with time-travel support at t0
stor <- storr_timetravel(uri, timestamp = t0)

# Read at t0
stor$list_hashes() # character(0), no hashes at t0

stor$get("a") #  key 'a' ('objects') not found

# Read at t1
stor$timestamp <- t1
stor$get("a") # 1
stor$get("b") # key 'b' ('objects') not found

# Read at t2
stor$timestamp <- t2
sto$get("b") # 2

} # }

```

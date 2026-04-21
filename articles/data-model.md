# Data Model

## Overview

The **Content Addressable Storage (CAS)** system using TileDB as the
underlying storage consists of collection two Sparse Arrays: one that
maps keys to hashes and another that maps hashes to objects.

Specifically, the driver storage is a TileDB Group with two member
TileDB Arrays which are stored relative to Group’s URI path.

## CAS

### Group Structure

**`Members`**

1.  `tbl_keys` (array): maps key-namespace pairs to hashes (and to
    expiry and/or notes, optional)
2.  `tbl_data` (array): maps hashes to values (serialised R objects)

**`Metadata`**

- `hash_algo`: The name of hash algorithm.
- `type`: Group identifier, `"storr"`

### Array Data Models

**`tbl_keys`** - A 2D sparse array that maps key-namespace pairs to
hashes and key-metadata.

- **Dimensions**: `namespace` (*ASCII*) and `key` (*ASCII*)
- **Attributes**: `hash` (*ASCII*), `expires_at` (*DATETIME_MS*) and
  `notes` (*UTF8*)

**`tbl_data`** - A 1D sparse array that maps hashes to object values.

- **Dimensions**: `hash` (*ASCII*)
- **Attributes**: `value` (*ASCII*)

TileDB datatypes in parentheses.

### Example

``` r
# URI path
uri <- tempfile()

# Create a driver
dr <- driver_tiledb(uri, init = TRUE)
```

CAS Structure:

``` r
dr$dir_tree()
```

    # C:/Users/cgiac/AppData/Local/Temp/RtmpG8aZLG/file1e204dba69e5
    # ├── tbl_data
    # │   ├── __commits
    # │   ├── __fragments
    # │   ├── __fragment_meta
    # │   ├── __labels
    # │   ├── __meta
    # │   └── __schema
    # │       ├── __1776672633305_1776672633305_722b26a5560e3fb2055d0ff6f641d211
    # │       └── __enumerations
    # ├── tbl_keys
    # │   ├── __commits
    # │   ├── __fragments
    # │   ├── __fragment_meta
    # │   ├── __labels
    # │   ├── __meta
    # │   └── __schema
    # │       ├── __1776672633291_1776672633291_352383d242e46903cf97dd6aec82b64e
    # │       └── __enumerations
    # ├── __group
    # │   └── __1776672633377_1776672633377_027b74659ad493e493793be261abb635_2
    # ├── __meta
    # │   └── __1776672633289_1776672633289_3673909d253dfd136bbec9a0e4616d8c
    # └── __tiledb_group.tdb
    # 
    # ❯ directories (7) • total size (647 B)

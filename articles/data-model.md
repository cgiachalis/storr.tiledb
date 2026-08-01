# Storr Data Models

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

    # C:/Users/cgiac/AppData/Local/Temp/RtmpIV06wB/file94c457086b3
    # ├── tbl_data
    # │   ├── __commits
    # │   ├── __fragments
    # │   ├── __fragment_meta
    # │   ├── __labels
    # │   ├── __meta
    # │   └── __schema
    # │       ├── __1785580886312_1785580886312_6e1c593b059254f5934a5a6a2d6be9a2
    # │       └── __enumerations
    # ├── tbl_keys
    # │   ├── __commits
    # │   ├── __fragments
    # │   ├── __fragment_meta
    # │   ├── __labels
    # │   ├── __meta
    # │   └── __schema
    # │       ├── __1785580886310_1785580886310_5971e8efd915ba42ccd52498694e1f1e
    # │       └── __enumerations
    # ├── __group
    # │   └── __1785580886373_1785580886373_624f949cbf9498982f953fe4cb5dc0d1_2
    # ├── __meta
    # │   └── __1785580886318_1785580886318_5971e8ee81f51a500417aa72204ebfbc
    # └── __tiledb_group.tdb
    # 
    # ❯ directories (7) • total size (647 B)

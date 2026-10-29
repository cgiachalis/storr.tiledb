# Storr Data Models

## Overview

This vignette describes the **Content-Addressable Storage (CAS)** data
model used by the storr.tiledb driver. The driver stores storr data
inside a TileDB Group that contains two Sparse Arrays:

- `tbl_keys` maps key-namespace pairs to hashes and optional key
  metadata
- `tbl_data` maps content hashes to serialized object values

The arrays are stored as members inside a TileDB Group at the driver’s
URI. The Group also contains metadata about the storage format.

------------------------------------------------------------------------

## CAS: Group Structure

### Members

1.  `tbl_keys` (array) maps key/namespace pairs to hashes and can store
    expiry and notes.
2.  `tbl_data` (array) maps content hashes to serialized R objects.

### Metadata

- `hash_algo` - name of the hash algorithm used (e.g., `"sha256"`).
- `type` - group identifier string `"storr"`.
- `serial_format` - name of the serialization format used (e.g.,
  `"rds"`).

------------------------------------------------------------------------

## Array data models

### **`tbl_keys`** array

A 2-dimensional sparse array that maps key-namespace pairs to hashes and
key-metadata.

**Dimensions**

- `namespace` (*ASCII*)
- `key` (*ASCII*)

**Attributes**

- `hash` (*ASCII*)
- `expires_at` (*DATETIME_MS*)
- `notes` (*UTF8*)

### **`tbl_data`** array

A 1-dimensional sparse array that maps content hashes to serialized
values.

**Dimensions**

- `hash` (*ASCII*)

**Attributes**

- `value` (*ASCII*)

TileDB datatypes shown in parentheses.

### Example

Below is a short example that creates a TileDB-backed driver and
inspects its layout.

``` r

# Create a temporary URI
uri <- tempfile()

# Create a driver (initializes the TileDB Group and member arrays)
dr <- driver_tiledb(uri, init = TRUE)
```

Inspecting the driver directory tree:

``` r

dr$dir_tree()
```

    # C:/Users/cgiac/AppData/Local/Temp/Rtmpw5Fdb0/file91183368131
    # ├── tbl_data
    # │   ├── __commits
    # │   ├── __fragments
    # │   ├── __fragment_meta
    # │   ├── __labels
    # │   ├── __meta
    # │   └── __schema
    # │       ├── __1789455127951_1789455127951_171805577a203143514acb520222971b
    # │       └── __enumerations
    # ├── tbl_keys
    # │   ├── __commits
    # │   ├── __fragments
    # │   ├── __fragment_meta
    # │   ├── __labels
    # │   ├── __meta
    # │   └── __schema
    # │       ├── __1789455127932_1789455127932_57ada422b6618fe50a02c603f89f2707
    # │       └── __enumerations
    # ├── __group
    # │   └── __1789455128029_1789455128029_3b4660a540bb900d4d9db238aa989654_2
    # ├── __meta
    # │   └── __1789455127959_1789455127959_37e1411a276115f55fa073f01d647167
    # └── __tiledb_group.tdb
    # 
    # ❯ directories (7) • total size (667 B)

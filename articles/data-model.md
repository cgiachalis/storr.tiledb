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

Below is a short example that shows creating a TileDB-backed driver and
inspecting its layout.

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

    # C:/Users/cgiac/AppData/Local/Temp/RtmpCGgG9I/filea8c4634c2198
    # ├── tbl_data
    # │   ├── __commits
    # │   ├── __fragments
    # │   ├── __fragment_meta
    # │   ├── __labels
    # │   ├── __meta
    # │   └── __schema
    # │       ├── __1788419331967_1788419331967_74b1721886a26ec2193126881f9dbb8c
    # │       └── __enumerations
    # ├── tbl_keys
    # │   ├── __commits
    # │   ├── __fragments
    # │   ├── __fragment_meta
    # │   ├── __labels
    # │   ├── __meta
    # │   └── __schema
    # │       ├── __1788419331966_1788419331966_0e09559a95f4ec77500deec0c06f103e
    # │       └── __enumerations
    # ├── __group
    # │   └── __1788419332028_1788419332028_1f56eaee7d9f55ceb8eb3340202782d9_2
    # ├── __meta
    # │   └── __1788419331971_1788419331971_5c4223e71cb4e86ec491b1ad188091ae
    # └── __tiledb_group.tdb
    # 
    # ❯ directories (7) • total size (667 B)

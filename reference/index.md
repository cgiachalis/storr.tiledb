# Package index

## Storr Interface

The storr interface is built on top of the R6 class system which
provides the framework for managing content-addressable storage with
TileDB. The key function to interact with a storr is
[`storr_tiledb()`](https://cgiachalis.github.io/storr.tiledb/reference/storr_tiledb.md).
Additional functionality is provided for managing TileDB fragments as
well as convenient storr utilities.

### Storr

Key functions to work with a ‘storr’ or creating a TileDB driver.

- [`storr_tiledb()`](https://cgiachalis.github.io/storr.tiledb/reference/storr_tiledb.md)
  : A Storr using TileDB Engine
- [`driver_tiledb()`](https://cgiachalis.github.io/storr.tiledb/reference/driver_tiledb.md)
  [`driver_tiledb_create()`](https://cgiachalis.github.io/storr.tiledb/reference/driver_tiledb.md)
  : TileDB Storr Driver

### Storr Management

Utilities for Storr management.

- [`storr_copy()`](https://cgiachalis.github.io/storr.tiledb/reference/storr_copy.md)
  : Copy Storr to another URI
- [`storr_move()`](https://cgiachalis.github.io/storr.tiledb/reference/storr_move.md)
  : Move Storr to another URI
- [`storr_rename()`](https://cgiachalis.github.io/storr.tiledb/reference/storr_rename.md)
  : Rename Storr URI

### Fragment Management

Functionality to inspect and manage TileDB fragments.

- [`storr_fragments()`](https://cgiachalis.github.io/storr.tiledb/reference/storr_fragments.md)
  : Storr Fragments
- [`storr_consolidate()`](https://cgiachalis.github.io/storr.tiledb/reference/storr_consolidate.md)
  : Consolidate Storr Fragments
- [`storr_vacuum()`](https://cgiachalis.github.io/storr.tiledb/reference/storr_vacuum.md)
  : Vacuum Storr Fragments

## R6 Classes

R6 classes that represent the storr interface and TileDB driver. These
are internal objects and should not be used directly.

- [`CAS`](https://cgiachalis.github.io/storr.tiledb/reference/CAS.md) :

  Generate a `CAS` Object

- [`TileDBDriver`](https://cgiachalis.github.io/storr.tiledb/reference/TileDBDriver.md)
  :

  Generate a `TileDBDriver` Object

- [`TileDBStorr`](https://cgiachalis.github.io/storr.tiledb/reference/TileDBStorr.md)
  :

  Generate a `TileDBStorr` Object

- [`StorrFragments`](https://cgiachalis.github.io/storr.tiledb/reference/StorrFragments.md)
  :

  Generate a `StorrFragments` Object

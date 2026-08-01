# Time Travel

## Overview

This vignette demonstrates the time-travel capabilities provided by
storr.tiledb via
[`storr_timetravel()`](https://cgiachalis.github.io/storr.tiledb/reference/storr_timetravel.md)
and the `TimeTravelDriver`/`StorrTimeTravel` classes. Use time-travel to
open a storr at a specific TileDB timestamp range (read-only) and
inspect the state of keys and objects at that time.

### Create a storr and write values at successive times

``` r

# Create a storr
uri <- tempfile()
sto <- storr_tiledb(uri, init = TRUE, default_namespace = "ns1")

# Record timestamps around operations
t0 <- Sys.time()
sto$set("a", 1)   # first version of "a"
t1 <- Sys.time()
sto$set("a", 2)   # updated "a"
sto$set("b", 3, namespace = "ns2")
t2 <- Sys.time()

# Verify current (latest) state
sto$list()
# [1] "a"
sto$list_hashes()
# [1] "02c87a685a6264c39c65c94a51de14b8"
# [2] "38e42db36c4414f7bbc19d750f71a721"
# [3] "87494137ffd66807c0c5c877856799cc"
sto$get("a")
# [1] 2
sto$get("b", namespace = "ns2")
# [1] 3
```

### Open a time-travel storr at t0 (before any writes)

``` r

stor <- storr_timetravel(uri, default_namespace = "ns1", timestamp = t0)

# At t0 nothing should be present
stor$list()
# character(0)
stor$list_hashes()
# character(0)
# Attempting to read keys written after t0 should error
stor$get("a")
# Error:
# ! key 'a' ('ns1') not found
```

### Move forward to t1 and inspect the state

``` r

# Set the timestamp to t1 (this uses the active binding)
stor$timestamp <- t1

# At t1 "a" has the first value, "b" is not yet present in ns2
stor$list()
# [1] "a"
stor$get("a")
# [1] 1
stor$mget(c("a", "b"), namespace = c("ns1", "ns2"))
# [[1]]
# [1] 1
# 
# [[2]]
# NULL
# 
# attr(,"missing")
# [1] 2
stor$exists(c("a", "b"), namespace = c("ns1", "ns2"))
# [1]  TRUE FALSE
```

### Move forward to t2 and inspect the later state

``` r

stor$timestamp <- t2

# Now "a" has the updated value and "b" exists in ns2 (but default_namespace is ns1)
stor$get("a")
# [1] 2
# mget across namespaces
stor$mget(c("a", "b"), namespace = c("ns1", "ns2"))
# [[1]]
# [1] 2
# 
# [[2]]
# [1] 3
stor$list_namespaces()
# [1] "ns1" "ns2"
stor$list_hashes()
# [1] "02c87a685a6264c39c65c94a51de14b8"
# [2] "38e42db36c4414f7bbc19d750f71a721"
# [3] "87494137ffd66807c0c5c877856799cc"
```

### Read-only behaviour

`StorrTimeTravel` is designed for read-only access. Attempts to modify
the store via the time-travel object should fail:

``` r

# Try writing — should error
stor$set("z", 10)
# Error:
# ! attempt to apply non-function
```

### Using timestamp ranges

You can also open a time-travel storr with an explicit start/end
timestamp pair to see the state as of the end timestamp (and the tileDB
timestamp object can be created with
[`set_tiledb_timestamp()`](https://cgiachalis.github.io/R6.tiledb/reference/set_tiledb_timestamp.html)
when needed). Example below demonstrates the idea:

``` r

# Example: create a timestamp range (end only here)
stor$timestamp <- t1          # open at t1 (end)
stor$get("a")                # retrieves version at or before t1
# [1] 1

# If you need an explicit tiledb_timestamp object:
# tt <- set_tiledb_timestamp(Sys.time() - 3600, Sys.time())
# stor$timestamp <- tt
```

### Reset timestamp to present

Set timestamp field to `NULL` to reset storr state to present.

``` r

# Set the timestamp to present 
stor$timestamp <- NULL
stor$mget(c("a", "b"), namespace = c("ns1", "ns2"))
# [[1]]
# [1] 2
# 
# [[2]]
# [1] 3
```

## Notes

- Time-travel storrs reuse the underlying TileDB fragments, and the
  `timestamp` active binding controls the time-range used when
  opening/reading the arrays.
- Because time-travel is read-only, consider using a regular
  [`storr_tiledb()`](https://cgiachalis.github.io/storr.tiledb/reference/storr_tiledb.md)
  for writes and
  [`storr_timetravel()`](https://cgiachalis.github.io/storr.tiledb/reference/storr_timetravel.md)
  for inspections of historical states.

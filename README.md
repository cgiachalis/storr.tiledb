# storr.tiledb

<!-- badges: start -->
[![CRAN status](https://img.shields.io/badge/CRAN-not%20published-orange)](https://CRAN.R-project.org/package=storr.tiledb) 
[![repo-status](https://img.shields.io/badge/repo%20status-experimental-orange.svg)](#) 
[![License](https://img.shields.io/badge/License-MIT-003366.svg)](https://opensource.org/licenses/MIT)
[![R CMD Check](https://github.com/cgiachalis/storr.tiledb/actions/workflows/R-CMD-check.yaml/badge.svg?branch=master)](https://github.com/cgiachalis/storr.tiledb/actions/workflows/R-CMD-check.yaml)
[![Last Commit](https://img.shields.io/github/last-commit/cgiachalis/storr.tiledb)](https://github.com/cgiachalis/storr.tiledb)
[![Documentation](https://img.shields.io/badge/docs-GitHub_Pages-blue)](https://cgiachalis.github.io/storr.tiledb/)
<!-- badges: end -->

## Overview

A [storr](https://github.com/richfitz/storr) driver using [TileDB Embedded](https://github.com/TileDB-Inc/TileDB) storage engine.

*What is storr?*

A key-value store with content addressable storage for R objects with optional in-memory caching layer.

The interface is provided by [storr](https://cloud.r-project.org/web/packages/storr/index.html)
package and defines a set of common operations (set, get, del methods) across a range of different
storage drivers (DBI, LMDB, Redis, RDS, environment). The `storr` package is
written by [Rich FitzJohn](https://github.com/richfitz). 

*What is storr.tiledb?*

The `storr.tiledb` contributes a new storr compliant driver using TileDB Engine for backend.

It defines a new storr R6 class that leverages the advantages of TileDB and provides new features
such as adding notes and expiration time-stamps along with key-namespace pairs. 

> [!WARNING]  
> The package is in experimental status. Currently, the driver is complete and
> fully functional in that it passes the auto test specification defined by storr
> package. More testing is needed and additional features to be implemented before
> it is moved to mature state. Feedback is welcomed.


## Key features

 - Key-value operations use driver's methods for speed and efficiency

 - Key-value metadata such as notes and timestamps for data expiration (time-to-live, TTL)

 - Asynchronous key-value operations or in parallel through [mirai](https://cran.rstudio.com/web/packages/mirai/) 
 framework 
 
 - Hash tables (hashtab)  for in memory caching layers instead of environments

 - Cloud storage (S3, Azure, GSC)
 
 - Data versioning (time-travelling)
 
 - Encryption

## Installation

Development version from GitHub:

``` r
# pak
pak::pkg_install("cgiachalis/storr.tiledb")

# remotes
remotes::install_github("cgiachalis/storr.tiledb")
```

## Quick start

```r
library(storr.tiledb)

# Temp URI path
uri <- tempfile()

# Set up storr
sto <- storr_tiledb(uri, init = TRUE)

# Set
sto$set("mykey1", list(a = 1))
sto$set("mykey2", "abc")

# Get
sto$get("mykey2")
 [1] "abc"
 
# List all keys
sto$list()
[1] "mykey1" "mykey2"

# Del
sto$del("mykey1")

```

## Other storr drivers

- [storr_enviroment()](https://richfitz.github.io/storr/reference/storr_environment.html) in memory storage using R enviroment
- [storr_rds()](https://richfitz.github.io/storr/reference/storr_rds.html) on disk storage using RDS file format
- [storr_dbi()](https://richfitz.github.io/storr/reference/storr_dbi.html) using [DBI](https://cran.r-project.org/web/packages/DBI/index.html) interface
- [storr_redis_api()](https://richfitz.github.io/redux/reference/storr_redis_api.html) using Redis through [redux](https://github.com/richfitz/redux)
- [storr_thor()](https://richfitz.github.io/thor/reference/storr_thor.html) - 
using [LMDB](https://github.com/LMDB/lmdb) Lightning Memory-Mapped Database through [thor](https://github.com/richfitz/thor)


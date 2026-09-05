# storr.tiledb

<!-- badges: start -->

![R Language](https://img.shields.io/badge/R_Language-blue?logo=r&logoSize=auto)
[![CRAN status](https://img.shields.io/badge/CRAN-not%20published-orange)](https://CRAN.R-project.org/package=storr.tiledb)
[![storr.tiledb status badge](https://cgiachalis.r-universe.dev/storr.tiledb/badges/version)](https://cgiachalis.r-universe.dev/storr.tiledb)
[![repo-status](https://img.shields.io/badge/repo%20status-stable-brightgreen.svg)](#)
[![License](https://img.shields.io/badge/License-MIT-003366.svg)](https://opensource.org/licenses/MIT) 
[![R CMD Check](https://github.com/cgiachalis/storr.tiledb/actions/workflows/R-CMD-check.yaml/badge.svg?branch=master)](https://github.com/cgiachalis/storr.tiledb/actions/workflows/R-CMD-check.yaml) 
[![Codecov test coverage](https://codecov.io/gh/cgiachalis/storr.tiledb/graph/badge.svg?token=HOLQUXPZC2)](https://codecov.io/gh/cgiachalis/storr.tiledb)
[![Last Commit](https://img.shields.io/github/last-commit/cgiachalis/storr.tiledb)](https://github.com/cgiachalis/storr.tiledb) 
[![Documentation](https://img.shields.io/badge/docs-GitHub_Pages-blue)](https://cgiachalis.github.io/storr.tiledb/)

<!-- badges: end -->

A [TileDB Embedded](https://github.com/TileDB-Inc/TileDB)-based driver for [storr](https://github.com/richfitz/storr), a key-value store with content-addressable storage for R objects.

## Overview

`storr.tiledb` is an R package that extends the storr ecosystem with a TileDB-based driver and custom `storr`-like classes that leverage the advantages of the underlying backend.

[![Ask DeepWiki](https://deepwiki.com/badge.svg)](https://deepwiki.com/cgiachalis/storr.tiledb)

## Key features

- Fast key-value operations using TileDB methods
- Additional faster serialization formats through [qs2](https://cran.r-project.org/web/packages/qs2/index.html) package[^1]
- Per key metadata: notes and Time-To-Live (TTL) expiration timestamps next to key-value pairs
- Asynchronous and parallel operations with [mirai](https://cran.rstudio.com/web/packages/mirai/)
- In-memory caching layers with hash tables
- Native cloud storage support (AWS S3, Azure Blob, Google Cloud Storage)
- Data versioning (*time-traveling*) and encryption support
- Flexible schema configuration for performance tuning

[^1]: [qs2](https://cran.r-project.org/web/packages/qs2/index.html) package is soft dependency, so
it has to be installed to use the 'qs2' or 'qdata' serialization formats.

## Installation

Development version from GitHub:

```r
# pak
pak::pkg_install("cgiachalis/storr.tiledb")

# remotes
remotes::install_github("cgiachalis/storr.tiledb")
```

From R-universe:

```r
# install 'storr.tiledb'
install.packages('storr.tiledb', repos = c('https://cgiachalis.r-universe.dev'))
```

## Quick start

``` r
library(storr.tiledb)

# Create a temporary URI
uri <- tempfile()

# Create a TileDB storr
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

## Documentation

For more detailed information, visit the [full documentation](https://cgiachalis.github.io/storr.tiledb/) on GitHub Pages.

- [Get Started](https://cgiachalis.github.io/storr.tiledb/articles/storr-tiledb.html) - Quick reference to basic operations
- [API Usage](https://cgiachalis.github.io/storr.tiledb/articles/api.html) - Learn about `storr.tiledb` operations through examples
- [Data Model](https://cgiachalis.github.io/storr.tiledb/articles/data-model.html) - Overview of TileDB driver data model

## Alternative storr drivers

- [storr_enviroment()](https://richfitz.github.io/storr/reference/storr_environment.html) - In-memory with R environments
- [storr_rds()](https://richfitz.github.io/storr/reference/storr_rds.html) - RDS file format
- [storr_dbi()](https://richfitz.github.io/storr/reference/storr_dbi.html) - DBI interface
- [storr_redis_api()](https://richfitz.github.io/redux/reference/storr_redis_api.html) - Redis through [redux](https://github.com/richfitz/redux)
- [storr_thor()](https://richfitz.github.io/thor/reference/storr_thor.html) - [LMDB](https://github.com/LMDB/lmdb) Lightning Memory-Mapped Database via [thor](https://github.com/richfitz/thor)

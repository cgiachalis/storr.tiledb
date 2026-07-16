# storr.tiledb

<!-- badges: start -->

![R Language](https://img.shields.io/badge/R_Language-blue?logo=r&logoSize=auto)
[![CRAN status](https://img.shields.io/badge/CRAN-not%20published-orange)](https://CRAN.R-project.org/package=storr.tiledb)
[![repo-status](https://img.shields.io/badge/repo%20status-experimental-orange.svg)](#)
[![License](https://img.shields.io/badge/License-MIT-003366.svg)](https://opensource.org/licenses/MIT) 
[![R CMD Check](https://github.com/cgiachalis/storr.tiledb/actions/workflows/R-CMD-check.yaml/badge.svg?branch=master)](https://github.com/cgiachalis/storr.tiledb/actions/workflows/R-CMD-check.yaml) 
[![Last Commit](https://img.shields.io/github/last-commit/cgiachalis/storr.tiledb)](https://github.com/cgiachalis/storr.tiledb) [![Documentation](https://img.shields.io/badge/docs-GitHub_Pages-blue)](https://cgiachalis.github.io/storr.tiledb/)

<!-- badges: end -->

A [TileDB Embedded](https://github.com/TileDB-Inc/TileDB)-based driver for [storr](https://github.com/richfitz/storr), a key-value store with content-addressable storage for R objects.

## Overview

`storr.tiledb` is an R package that extends the storr ecosystem by providing a TileDB-based driver and custom `storr`-like classes that leverage the advantages of the underlying backend.

[![Ask DeepWiki](https://deepwiki.com/badge.svg)](https://deepwiki.com/cgiachalis/storr.tiledb)

> [!WARNING]\
> **Experimental status.** The package is fully functional and passes storr's test specification. Additional testing and features needed before stable release.

## Key features

- Fast key-value operations via driver methods
- Metadata support: notes and TTL expiration timestamps next to key-value pairs
- Async/parallel operations with [mirai](https://cran.rstudio.com/web/packages/mirai/)
- In-memory caching layers with hash tables
- Cloud storage (AWS S3, Azure Blob, Google Cloud Storage)
- Data version (time-travel)
- Encryption support
- Schemas configuration for tuning TileDB's performance and storage characteristics

## Installation

Development version from GitHub:

``` r
# pak
pak::pkg_install("cgiachalis/storr.tiledb")

# remotes
remotes::install_github("cgiachalis/storr.tiledb")
```

## Quick start

``` r
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

## Documentation

For more detailed information, visit the [full documentation](https://cgiachalis.github.io/storr.tiledb/) on GitHub Pages.

- [Get Started()](https://cgiachalis.github.io/storr.tiledb/articles/storr-tiledb.html) - Quick reference to basic operations
- [API Usage()](https://cgiachalis.github.io/storr.tiledb/articles/api.html) - Learn about `storr.tiledb` operations through examples
- [Data Model()](https://cgiachalis.github.io/storr.tiledb/articles/data-model.html) - Overview of TileDB driver data model

## Alternative storr drivers

- [storr_enviroment()](https://richfitz.github.io/storr/reference/storr_environment.html) - In-memory with R environments
- [storr_rds()](https://richfitz.github.io/storr/reference/storr_rds.html) - RDS file format
- [storr_dbi()](https://richfitz.github.io/storr/reference/storr_dbi.html) - DBI interface
- [storr_redis_api()](https://richfitz.github.io/redux/reference/storr_redis_api.html) - Redis through [redux](https://github.com/richfitz/redux)
- [storr_thor()](https://richfitz.github.io/thor/reference/storr_thor.html) - [LMDB](https://github.com/LMDB/lmdb) Lightning Memory-Mapped Database via [thor](https://github.com/richfitz/thor)

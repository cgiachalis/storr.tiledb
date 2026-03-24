# storr.tiledb

<!-- badges: start -->
[![repo-status](https://img.shields.io/badge/repo%20status-experimental-orange.svg)](#) 
[![License](https://img.shields.io/badge/License-MIT-003366.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

## storr.tiledb

A storr driver using [TileDB Embedded](https://github.com/TileDB-Inc/TileDB) storage engine as a backend.

A storr is a content addressed key-value store for R objects with an optional cache layer
that offers a common interface (key/value methods: set, get, del) across different
storage drivers (DBI, LMDB, redis, rds, environment). The interface is provided
by [storr](https://cloud.r-project.org/web/packages/storr/index.html) package which is 
written by [Rich FitzJohn](https://github.com/richfitz). 


The `storr.tiledb` contributes a new storr compliant driver using the TileDB storage engine.
It has its own storr R6 subclass that utilises the strengths of the underlying backend and
additionally it provides extra features such as key-value metadata and asynchronous writes.

Key features:

 - Key methods of interface has been mostly overwritten to make the most the underlying TileDB functionality
for speed and efficiency

 - Optional set metadata along with key/values pairs: notes and expiration timestamps

 - Cache layers are using hash tables (hashtab) instead of environments

 - Optional set key values asynchronously and in parallel through [mirai](https://cran.rstudio.com/web/packages/mirai/) 
 framework


TileDB engine offers additional features: support for remote storages (S3, Azure, GSC),
time-travelling capabilities and data versioning.

## Quick start


```r

library(storr.tiledb)

uri <- tempfile()

sto <- storr_tiledb(uri, init = TRUE)

sto$set("mykey", list(a = 1))

sto$get("mykey")

$a
[1] 1

```


## Installation

Development version from GitHub:

``` r
# pak
pak::pkg_install("cgiachalis/storr.tiledb")

# remotes
remotes::install_github("cgiachalis/storr.tiledb")
```

## Other storr drivers

- [storr_enviroment()](https://richfitz.github.io/storr/reference/storr_environment.html) using R enviroment
- [storr_rds()](https://richfitz.github.io/storr/reference/storr_rds.html) using RDS file format
- [storr_dbi()](https://richfitz.github.io/storr/reference/storr_dbi.html) using [DBI](https://cran.r-project.org/web/packages/DBI/index.html)
- [storr_redis_api()](https://richfitz.github.io/redux/reference/storr_redis_api.html) using Redis through [redux](https://github.com/richfitz/redux)
- [storr_thor()](https://richfitz.github.io/thor/reference/storr_thor.html) - 
using [LMDB](https://github.com/LMDB/lmdb) Lightning Memory-Mapped Database through [thor](https://github.com/richfitz/thor)


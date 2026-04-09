# storr.tiledb

<!-- badges: start -->
[![CRAN status](https://img.shields.io/badge/CRAN-not%20published-orange)](https://CRAN.R-project.org/package=storr.tiledb) 
[![repo-status](https://img.shields.io/badge/repo%20status-experimental-orange.svg)](#) 
[![License](https://img.shields.io/badge/License-MIT-003366.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

## Overview

A storr driver using [TileDB Embedded](https://github.com/TileDB-Inc/TileDB) storage engine.

*What is storr?*

A storr is a content addressed key-value store for R objects with an optional cache layer.

The interface is provided by [storr](https://cloud.r-project.org/web/packages/storr/index.html)
package and defines a set of common operations (set, get, del methods) across a range of different
storage drivers (DBI, LMDB, Redis, RDS, environment). The `storr` package is
written by [Rich FitzJohn](https://github.com/richfitz). 

*What is storr.tiledb?*

The `storr.tiledb` contributes a new storr compliant driver using TileDB as a back-end.

The package has its own storr R6 class that utilises the strengths of the underlying
engine and offers some extra features, such as the option to add notes and expiration
time-stamps along with key-namespace pairs. 

> [!WARNING]  
> The package is in experimental status. Currently, the driver is complete and
> fully functional in that it passes the auto test specification defined by storr
> package. More testing is needed and additional features to be implemented before
> it is moved to mature state. Feedback is welcomed.


## Key features

 - Key interface methods have been re-written to make the most of the underlying
 back end with respect to speed and efficiency

 - Set optional notes and expiration timestamps when setting keys

 - Set keys asynchronously and in parallel through [mirai](https://cran.rstudio.com/web/packages/mirai/) 
 framework (optional)
 
 - Cache layers are using hash tables (hashtab) instead of environments

Additional features that TileDB supports:

 - Cloud storage (S3, Azure, GSC)
 
 - Data versioning (time-travelling) and 
 
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

uri <- tempfile()

sto <- storr_tiledb(uri, init = TRUE)

sto$set("mykey", list(a = 1))

sto$get("mykey")

$a
[1] 1

```

## Other storr drivers

- [storr_enviroment()](https://richfitz.github.io/storr/reference/storr_environment.html) in memory storage using R enviroment
- [storr_rds()](https://richfitz.github.io/storr/reference/storr_rds.html) on disk storage using RDS file format
- [storr_dbi()](https://richfitz.github.io/storr/reference/storr_dbi.html) using [DBI](https://cran.r-project.org/web/packages/DBI/index.html) interface
- [storr_redis_api()](https://richfitz.github.io/redux/reference/storr_redis_api.html) using Redis through [redux](https://github.com/richfitz/redux)
- [storr_thor()](https://richfitz.github.io/thor/reference/storr_thor.html) - 
using [LMDB](https://github.com/LMDB/lmdb) Lightning Memory-Mapped Database through [thor](https://github.com/richfitz/thor)


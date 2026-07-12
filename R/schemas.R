
# Set up TileDB filters
.tiledb_filter <- function(level = -1L, name = "ZSTD", ctx) {
  tiledb::tiledb_filter_set_option(
    object = tiledb::tiledb_filter(name, ctx = ctx),
    option = "COMPRESSION_LEVEL",
    value = level)
}

.tiledb_flist <- function(level = -1, name = "ZSTD", ctx) {

  if (name == "NONE") {
    tiledb::tiledb_filter_list(c(tiledb::tiledb_filter("NONE", ctx = ctx)))
  } else {
    tiledb::tiledb_filter_list(c(.tiledb_filter(level = level, name = name, ctx = ctx)), ctx = ctx)
  }

}


# Set up shared Dimensions
.dim_ascii <- function(name, level = -1L, fname = "ZSTD", ctx) {

  tiledb::tiledb_dim(name = name,
                     domain = c(NULL, NULL),
                     tile = NULL,
                     type = "ASCII",
                     filter_list = .tiledb_flist(level = level, name = fname, ctx = ctx),
                     ctx = ctx)
}


# Schemas -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

# 2-D Array
# Dims: namespace, keys
# Attrs: hash, expires_at
schema_keys <- function(compression_level = -7, ctx) {

  if (is.null(compression_level)) {
    .filter_zstd <- .filter_rle  <- .tiledb_flist(name = "NONE", ctx = ctx)
    fname <- "NONE"
  } else {
    .filter_zstd <- .tiledb_flist(level = compression_level, name = "ZSTD", ctx = ctx)
    .filter_rle <- .tiledb_flist(level = -1, name = "RLE", ctx = ctx)
    fname <- "ZSTD"

  }

  # domain
  dom <- tiledb::tiledb_domain(c(.dim_ascii("namespace", level = compression_level, fname = fname, ctx = ctx),
                                 .dim_ascii("key", level = compression_level, fname = fname, ctx = ctx)))

  # attributes
  attrs <- c(
    tiledb::tiledb_attr(
      name = "hash",
      type = "ASCII",
      ncells = NA,
      nullable = FALSE,
      filter_list = .filter_zstd,
      ctx = ctx
    ),
    tiledb::tiledb_attr(
      name = "expires_at",
      type = "DATETIME_MS",
      ncells = 1,
      nullable = FALSE,
      filter_list = .filter_zstd,
      ctx = ctx
    ),
    tiledb::tiledb_attr(
      name = "notes",
      type = "UTF8",
      ncells = NA,
      nullable = TRUE,
      filter_list = .filter_zstd,
      ctx = ctx
    )
  )

  # schema constructor
  sch <- tiledb::tiledb_array_schema(
    domain = dom,
    attrs = attrs,
    cell_order = "COL_MAJOR",
    tile_order = "COL_MAJOR",
    capacity = 10000,
    sparse = TRUE,
    allows_dups = FALSE,
    coords_filter_list = .filter_zstd,
    offsets_filter_list = .filter_zstd,
    validity_filter_list = .filter_rle,
    ctx = ctx)

  sch

}


# 1-D Array
# Dims: hash
# Attrs: value
schema_data <- function(compression_level = -7, ctx) {

  if (is.null(compression_level)) {
    .filter_zstd <- .filter_rle  <- .tiledb_flist(name = "NONE", ctx = ctx)
    fname <- "NONE"
  } else {
    .filter_zstd <- .tiledb_flist(level = compression_level, name = "ZSTD", ctx = ctx)
    .filter_rle <- .tiledb_flist(level = -1, name = "RLE", ctx = ctx)
    fname <- "ZSTD"
  }

  # domain
  dom <- tiledb::tiledb_domain(.dim_ascii("hash", level = compression_level, fname = fname, ctx = ctx))

  # attributes
  attrs <-  c(
    tiledb::tiledb_attr(
      name = "value",
      type = "ASCII",
      ncells = NA,
      nullable = FALSE,
      filter_list = .filter_zstd,
      ctx = ctx
    )
  )

  # schema constructor
  sch <- tiledb::tiledb_array_schema(
    domain = dom,
    attrs = attrs,
    cell_order = "COL_MAJOR",
    tile_order = "COL_MAJOR",
    capacity = 10000,
    sparse = TRUE,
    allows_dups = FALSE,
    coords_filter_list = .filter_zstd,
    offsets_filter_list = .filter_zstd,
    validity_filter_list = .filter_rle,
    ctx = ctx)

  sch

}

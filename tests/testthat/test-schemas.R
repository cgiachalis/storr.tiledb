
# helper to get array/schema filters
.schema_filters <- function(sch) {

  dom <- tiledb::domain(sch)
  dims <- tiledb::dimensions(dom)
  names(dims) <- sapply(dims, tiledb::name)
  cols <- c(dims, tiledb::attrs(sch))


  out <- vector("list", length(cols))
  names(out) <- names(cols)

  for (i in seq_along(cols)) {

    flist <- tiledb::filter_list(cols[[i]])
    n <- tiledb::nfilters(flist)

    out[i] <- lapply(0:c(n-1), function(.f) {
      flt <- flist[.f]
      flt_type <- tiledb::tiledb_filter_type(flt)

      lvl <- tryCatch( tiledb::tiledb_filter_get_option(flt, "COMPRESSION_LEVEL"),
      error = function(e) NULL)

      c(flt_type, lvl)
    })

  }

  sch_filters <- tiledb::filter_list(sch)
  out_sch <- vector("list", length(sch_filters))
  names(out_sch) <- names(sch_filters)

  for (i in seq_along(sch_filters)) {

    flist <- sch_filters[[i]]
    n <- tiledb::nfilters(flist)
    out_sch[i] <- lapply(0:c(n-1), function(.f) {
      flt <- flist[.f]
      flt_type <- tiledb::tiledb_filter_type(flt)

      lvl <- tryCatch( tiledb::tiledb_filter_get_option(flt, "COMPRESSION_LEVEL"),
                       error = function(e) NULL)

      c(flt_type, lvl)
    })

  }

  data.frame(c(out, out_sch))
}

test_that("CAS schemas data models", {

  ctx <- new_context()
  sch_keys <- schema_keys(ctx = ctx)
  sch_data <- schema_data(ctx = ctx)

  expect_true(tiledb::schema_check(sch_keys))
  expect_true(tiledb::schema_check(sch_data))

  expect_equal(tiledb::tiledb_schema_get_names(sch_keys),
               c("namespace", "key", "hash", "expires_at", "notes"))

  expect_equal(tiledb::tiledb_schema_get_types(sch_keys),
               c("ASCII", "ASCII", "ASCII", "DATETIME_MS", "UTF8"))

  expect_equal(tiledb::tiledb_schema_get_names(sch_data),
               c("hash", "value"))

  expect_equal(tiledb::tiledb_schema_get_types(sch_data),
               c("ASCII", "ASCII"))

})


test_that("CAS default schemas filters", {

  trg_keys <- data.frame(list(
    namespace = c("ZSTD", "-7"),
    key = c("ZSTD", "-7"),
    hash = c("ZSTD", "-7"),
    expires_at = c("ZSTD", "-7"),
    notes = c("ZSTD", "-7"),
    coords = c("ZSTD", "-7"),
    offsets = c("ZSTD", "-7"),
    validity = c("RLE", "-1")
  ))

  trg_data <- data.frame(list(hash = c("ZSTD", "-7"),
                              value = c("ZSTD", "-7"),
                              coords = c("ZSTD", "-7"),
                              offsets = c("ZSTD", "-7"),
                              validity = c("RLE", "-1")))


  ctx <- new_context()
  sch_keys <- schema_keys(ctx = ctx)
  sch_data <- schema_data(ctx = ctx)

  expect_equal(.schema_filters(sch_keys), trg_keys)
  expect_equal(.schema_filters(sch_data), trg_data)

})



test_that("CAS schemas compression level", {


  trg_keys <- data.frame(list(
    namespace = c("ZSTD", "0"),
    key = c("ZSTD", "0"),
    hash = c("ZSTD", "0"),
    expires_at = c("ZSTD", "0"),
    notes = c("ZSTD", "0"),
    coords = c("ZSTD", "0"),
    offsets = c("ZSTD", "0"),
    validity = c("RLE", "-1")
  ))

  trg_data <- data.frame(list(hash = c("ZSTD", "0"),
                              value = c("ZSTD", "0"),
                              coords = c("ZSTD", "0"),
                              offsets = c("ZSTD", "0"),
                              validity = c("RLE", "-1")))


  ctx <- new_context()
  sch_keys <- schema_keys(ctx = ctx, compression_level = 0)
  sch_data <- schema_data(ctx = ctx, compression_level = 0)

  expect_equal(.schema_filters(sch_keys), trg_keys)
  expect_equal(.schema_filters(sch_data), trg_data)

})


test_that("CAS schemas with no filters", {

  trg_keys <- data.frame(list(
    namespace = "NONE",
    key = "NONE",
    hash = "NONE",
    expires_at = "NONE",
    notes = "NONE",
    coords = "NONE",
    offsets = "NONE",
    validity = "NONE"
  ))

  trg_data <- data.frame(list(hash = "NONE",
                              value = "NONE",
                              coords = "NONE",
                              offsets = "NONE",
                              validity = "NONE"))


  ctx <- new_context()
  sch_keys <- schema_keys(ctx = ctx, compression_level = NULL)
  sch_data <- schema_data(ctx = ctx, compression_level = NULL)

  expect_equal(.schema_filters(sch_keys), trg_keys)
  expect_equal(.schema_filters(sch_data), trg_data)

})



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


test_that("SchemaBase initialization", {

  # Expected outputs
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

  trg_keys2 <- data.frame(list(
    namespace = c("ZSTD", "-7"),
    key = c("ZSTD", "-7"),
    hash = c("ZSTD", "-7"),
    expires_at = c("ZSTD", "-7"),
    notes = c("ZSTD", "-7"),
    coords = c("ZSTD", "-7"),
    offsets = c("ZSTD", "-7"),
    validity = c("RLE", "-1")
  ))

  trg_data2 <- data.frame(list(hash = c("ZSTD", "-7"),
                              value = c("ZSTD", "-7"),
                              coords = c("ZSTD", "-7"),
                              offsets = c("ZSTD", "-7"),
                              validity = c("RLE", "-1")))
  # ---

  # Without filters
  sch_data <- SchemaData$new(none_filter = TRUE)
  sch_keys <- SchemaKeys$new(none_filter = TRUE)

  # SchemaBase inherits correctly to SchemaKeys, SchemaData
  expect_r6_class(sch_data, "SchemaBase")
  expect_s3_class(sch_data, c("SchemaData", "SchemaBase"))

  expect_r6_class(sch_keys, "SchemaBase")
  expect_s3_class(sch_keys, c("Schemakeys", "SchemaBase"))


  expect_s4_class(sch_keys$schema(), "tiledb_array_schema")
  expect_s4_class(sch_data$schema(), "tiledb_array_schema")

  expect_equal(.schema_filters(sch_data$schema()), trg_data)
  expect_equal(.schema_filters(sch_keys$schema()), trg_keys)


  # With filters and custom context
  ctx <- R6.tiledb::new_context()
  sch_data <- SchemaData$new(ctx = ctx, none_filter = FALSE)
  sch_keys <- SchemaKeys$new(ctx = ctx, none_filter = FALSE)

  expect_r6_class(sch_data, "SchemaBase")
  expect_s3_class(sch_data, c("SchemaData", "SchemaBase"))

  expect_r6_class(sch_keys, "SchemaBase")
  expect_s3_class(sch_keys, c("Schemakeys", "SchemaBase"))

  expect_equal(.schema_filters(sch_data$schema()), trg_data2)
  expect_equal(.schema_filters(sch_keys$schema()), trg_keys2)

  # From uri path
  uri <- file.path(withr::local_tempdir(), "test-driver")

  driver_tiledb_create(uri, compression_level = NULL)

  uri_keys <- file_path(uri, "tbl_keys")
  uri_data <- file_path(uri, "tbl_data")

  sch_data <- SchemaData$new(uri = uri_data, none_filter = FALSE)
  sch_keys <- SchemaKeys$new(uri = uri_keys, none_filter = FALSE)

  expect_equal(.schema_filters(sch_data$schema()), trg_data)
  expect_equal(.schema_filters(sch_keys$schema()), trg_keys)

})


test_that("SchemaBase capacity getter and setter", {

  sch_data <- SchemaData$new()
  expect_equal(sch_data$capacity, 10000)

  expect_no_error(sch_data$capacity <- 100)
  expect_equal(sch_data$capacity, 100)

  expect_error(sch_data$capacity <- "invalid", "'capacity' should be a scalar numeric value")
  expect_error(sch_data$capacity <- c(1, 2), "'capacity' should be a scalar numeric value")

})


test_that("SchemaBase cell_order getter and setter", {

  sch_keys <- SchemaKeys$new()

  expect_no_error(original_order <- sch_keys$cell_order)
  expect_true(original_order %in% c("ROW_MAJOR", "COL_MAJOR"))

  new_order <- if (original_order == "ROW_MAJOR") "COL_MAJOR" else "ROW_MAJOR"

  expect_no_error(sch_keys$cell_order <- new_order)
  expect_equal(sch_keys$cell_order, new_order)

  expect_error(sch_keys$cell_order <- "INVALID_ORDER", "'cell_order' should be a either 'ROW_MAJOR' or 'COL_MAJOR'")

})

test_that("SchemaBase tile_order getter and setter", {

  sch_data <- SchemaData$new()

  expect_no_error(original_order <- sch_data$tile_order)
  expect_true(original_order %in% c("ROW_MAJOR", "COL_MAJOR"))

  new_order <- if (original_order == "ROW_MAJOR") "COL_MAJOR" else "ROW_MAJOR"
  expect_no_error(sch_data$tile_order <- new_order)
  expect_equal(sch_data$tile_order, new_order)
  expect_error(sch_data$tile_order <- "INVALID_ORDER", "'tile_order' should be a either 'ROW_MAJOR' or 'COL_MAJOR'")
})


test_that("SchemaBase coords_flist getter and setter", {

  ctx <- R6.tiledb::new_context()
  sch_data <- SchemaData$new(ctx = ctx, none_filter = FALSE)

  expect_s4_class(sch_data$coords_flist, "tiledb_filter_list")

  flt <- tiledb::tiledb_filter("RLE", ctx = ctx)
  flist <- tiledb::tiledb_filter_list(flt, ctx = ctx)

  expect_no_error(sch_data$coords_flist <- flist)

  expect_s4_class(res_flist <- sch_data$coords_flist, "tiledb_filter_list")
  expect_equal(tiledb::tiledb_filter_type(res_flist[0]), "RLE")

  # Set NULL or NA maps to "NONE" filter
  expect_no_error(sch_data$coords_flist <- NULL)
  expect_equal(tiledb::tiledb_filter_type(sch_data$coords_flist[0]), "NONE")

  # Set to RLE in order to test `<- NA`
  sch_data$coords_flist <- flist
  expect_equal(tiledb::tiledb_filter_type(sch_data$coords_flist[0]), "RLE")

  expect_no_error(sch_data$coords_flist <- NA)
  expect_equal(tiledb::tiledb_filter_type(sch_data$coords_flist[0]), "NONE")

  # Errors are raised
  expect_error(sch_data$coords_flist <- "invalid", "Not a filter list")
  expect_error(sch_data$coords_flist <- 123, "Not a filter list")

})

test_that("SchemaBase offsets_flist getter and setter", {

  ctx <- R6.tiledb::new_context()
  sch_data <- SchemaData$new(ctx = ctx, none_filter = FALSE)

  expect_s4_class(sch_data$offsets_flist, "tiledb_filter_list")

  flt <- tiledb::tiledb_filter("RLE", ctx = ctx)
  flist <- tiledb::tiledb_filter_list(flt, ctx = ctx)

  expect_no_error(sch_data$offsets_flist <- flist)

  expect_s4_class(res_flist <- sch_data$offsets_flist, "tiledb_filter_list")
  expect_equal(tiledb::tiledb_filter_type(res_flist[0]), "RLE")

  # Set NULL or NA maps to "NONE" filter
  expect_no_error(sch_data$offsets_flist <- NULL)
  expect_equal(tiledb::tiledb_filter_type(sch_data$offsets_flist[0]), "NONE")

  # Set to RLE in order to test `<- NA`
  sch_data$offsets_flist <- flist
  expect_equal(tiledb::tiledb_filter_type(sch_data$offsets_flist[0]), "RLE")

  expect_no_error(sch_data$offsets_flist <- NA)
  expect_equal(tiledb::tiledb_filter_type(sch_data$offsets_flist[0]), "NONE")

  # Errors are raised
  expect_error(sch_data$offsets_flist <- "invalid", "Not a filter list")
  expect_error(sch_data$offsets_flist <- 123, "Not a filter list")

})


test_that("SchemaBase validity_flist getter and setter", {

  ctx <- R6.tiledb::new_context()
  sch_data <- SchemaData$new(ctx = ctx, none_filter = FALSE)

  expect_s4_class(sch_data$validity_flist, "tiledb_filter_list")

  flt <- tiledb::tiledb_filter("RLE", ctx = ctx)
  flist <- tiledb::tiledb_filter_list(flt, ctx = ctx)

  expect_no_error(sch_data$validity_flist <- flist)

  expect_s4_class(res_flist <- sch_data$validity_flist, "tiledb_filter_list")
  expect_equal(tiledb::tiledb_filter_type(res_flist[0]), "RLE")

  # Set NULL or NA maps to "NONE" filter
  expect_no_error(sch_data$validity_flist <- NULL)
  expect_equal(tiledb::tiledb_filter_type(sch_data$validity_flist[0]), "NONE")

  # Set to RLE in order to test `<- NA`
  sch_data$validity_flist <- flist
  expect_equal(tiledb::tiledb_filter_type(sch_data$validity_flist[0]), "RLE")

  expect_no_error(sch_data$validity_flist <- NA)
  expect_equal(tiledb::tiledb_filter_type(sch_data$validity_flist[0]), "NONE")

  # Errors are raised
  expect_error(sch_data$validity_flist <- "invalid", "Not a filter list")
  expect_error(sch_data$validity_flist <- 123, "Not a filter list")

})

test_that("Schemakeys dimensions/attributes' getter and setter", {


  active_fields <- c("dim_namespace",
                     "dim_key",
                     "attr_hash",
                     "attr_expiry_at",
                     "attr_note")
  expect_identical(names(SchemaKeys$active), active_fields)

  ctx <- R6.tiledb::new_context()
  sch_keys <- SchemaKeys$new(ctx = ctx, none_filter = FALSE)

  dv <- sapply(active_fields, function(.a) {
    expect_s4_class(sch_keys[[.a]], "tiledb_filter_list")
    NULL
  })


  flt <- tiledb::tiledb_filter("RLE", ctx = ctx)
  flist <- tiledb::tiledb_filter_list(flt, ctx = ctx)


  dv <- sapply(active_fields, function(.a) {
    expect_no_error(sch_keys[[.a]] <- flist)
    expect_s4_class(res_flist <- sch_keys[[.a]], "tiledb_filter_list")
    expect_equal(tiledb::tiledb_filter_type(res_flist[0]), "RLE")

    NULL
  })


  # Set NULL or NA maps to "NONE" filter
  dv <- sapply(active_fields, function(.a) {
    expect_no_error(sch_keys[[.a]] <- NULL)
    expect_s4_class(res_flist <- sch_keys[[.a]], "tiledb_filter_list")
    expect_equal(tiledb::tiledb_filter_type(res_flist[0]), "NONE")

    NULL
  })


  # Set to RLE in order to test `<- NA`
  dv <- sapply(active_fields, function(.a) {
    # revert to RLE
    expect_no_error(sch_keys[[.a]] <- flist)
    expect_equal(tiledb::tiledb_filter_type(sch_keys[[.a]][0]), "RLE")

    expect_no_error(sch_keys[[.a]] <- NA)
    expect_s4_class(res_flist <- sch_keys[[.a]], "tiledb_filter_list")
    expect_equal(tiledb::tiledb_filter_type(res_flist[0]), "NONE")

    NULL
  })


  # Errors are raised
  dv <- sapply(active_fields, function(.a) {
    expect_error(sch_keys[[.a]] <- "invalid", "Not a filter list")
    expect_error(sch_keys[[.a]] <- 123, "Not a filter list")

    NULL
  })

})

test_that("SchemaData dimensions/attributes' getter and setter", {


  active_fields <- c("dim_hash", "attr_value")
  expect_identical(names(SchemaData$active), active_fields)

  ctx <- R6.tiledb::new_context()
  sch_data <- SchemaData$new(ctx = ctx, none_filter = FALSE)

  dv <- sapply(active_fields, function(.a) {
    expect_s4_class(sch_data[[.a]], "tiledb_filter_list")
    NULL
  })


  flt <- tiledb::tiledb_filter("RLE", ctx = ctx)
  flist <- tiledb::tiledb_filter_list(flt, ctx = ctx)


  dv <- sapply(active_fields, function(.a) {
    expect_no_error(sch_data[[.a]] <- flist)
    expect_s4_class(res_flist <- sch_data[[.a]], "tiledb_filter_list")
    expect_equal(tiledb::tiledb_filter_type(res_flist[0]), "RLE")

    NULL
  })


  # Set NULL or NA maps to "NONE" filter
  dv <- sapply(active_fields, function(.a) {
    expect_no_error(sch_data[[.a]] <- NULL)
    expect_s4_class(res_flist <- sch_data[[.a]], "tiledb_filter_list")
    expect_equal(tiledb::tiledb_filter_type(res_flist[0]), "NONE")

    NULL
  })


  # Set to RLE in order to test `<- NA`
  dv <- sapply(active_fields, function(.a) {
    # revert to RLE
    expect_no_error(sch_data[[.a]] <- flist)
    expect_equal(tiledb::tiledb_filter_type(sch_data[[.a]][0]), "RLE")

    expect_no_error(sch_data[[.a]] <- NA)
    expect_s4_class(res_flist <- sch_data[[.a]], "tiledb_filter_list")
    expect_equal(tiledb::tiledb_filter_type(res_flist[0]), "NONE")

    NULL
  })


  # Errors are raised
  dv <- sapply(active_fields, function(.a) {
    expect_error(sch_data[[.a]] <- "invalid", "Not a filter list")
    expect_error(sch_data[[.a]] <- 123, "Not a filter list")

    NULL
  })

})



# Schema updates when properties change; test it SchemaKeys

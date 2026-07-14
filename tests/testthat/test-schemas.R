
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
  ctx <- new_context()
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

  ctx <- new_context()
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

  ctx <- new_context()
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

  ctx <- new_context()
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
                     "attr_expires_at",
                     "attr_notes")
  expect_identical(names(SchemaKeys$active), active_fields)

  ctx <- new_context()
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

  ctx <- new_context()
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

test_that("Schemakeys' schema is updated when properties/filters change", {

  ctx <- new_context()
  sch <- SchemaKeys$new(ctx = ctx, none_filter = FALSE)

  ##  cell_order, tile_order, capacity

  # capacity
  original_value <- sch$capacity
  expect_equal(tiledb::capacity(sch$schema()), original_value )
  sch$capacity <- 100
  expect_true(!identical(original_value, sch$capacity))
  expect_equal(tiledb::capacity(sch$schema()), 100)

  # cell_order
  original_value <- sch$cell_order
  expect_equal(tiledb::cell_order(sch$schema()), original_value)
  new_value <-  if (original_value == "ROW_MAJOR") "COL_MAJOR" else "ROW_MAJOR"
  sch$cell_order <- new_value
  expect_true(!identical(original_value, sch$cell_order))
  expect_equal(tiledb::cell_order(sch$schema()), new_value)

  # tile_order
  original_value <- sch$tile_order
  expect_equal(tiledb::tile_order(sch$schema()), original_value)
  new_value <-  if (original_value == "ROW_MAJOR") "COL_MAJOR" else "ROW_MAJOR"
  sch$tile_order <- new_value
  expect_true(!identical(original_value, sch$tile_order))
  expect_equal(tiledb::tile_order(sch$schema()), new_value)

  ## offsets, coords, validity filters

  schema_flists <- tiledb::filter_list(sch$schema())

  # offsets
  original_value <- sch$offsets_flist
  expect_equal(tiledb::tiledb_filter_type(original_value[0]), "ZSTD")

  flt <- tiledb::tiledb_filter("NONE", ctx = ctx)
  new_flist <- tiledb::tiledb_filter_list(flt, ctx = ctx)
  sch$offsets_flist <- new_flist

  offsets <- tiledb::filter_list(sch$schema())$offsets
  expect_equal(tiledb::nfilters(offsets), 1)
  expect_equal(tiledb::tiledb_filter_type(offsets[0]), "NONE")

  # coords
  original_value <- sch$coords_flist
  expect_equal(tiledb::tiledb_filter_type(original_value[0]), "ZSTD")

  sch$coords_flist <- new_flist

  coords <- tiledb::filter_list(sch$schema())$coords
  expect_equal(tiledb::nfilters(coords), 1)
  expect_equal(tiledb::tiledb_filter_type(coords[0]), "NONE")

  # validity
  original_value <- sch$validity_flist
  expect_equal(tiledb::tiledb_filter_type(original_value[0]), "RLE")

  sch$validity_flist <- new_flist

  validity <- tiledb::filter_list(sch$schema())$validity
  expect_equal(tiledb::nfilters(validity), 1)
  expect_equal(tiledb::tiledb_filter_type(validity[0]), "NONE")

  ## Dimensions and attributes

  active_fields <- c("dim_namespace",
                     "dim_key",
                     "attr_hash",
                     "attr_expires_at",
                     "attr_notes")

  flt <- tiledb::tiledb_filter("RLE", ctx = ctx)
  flist <- tiledb::tiledb_filter_list(flt, ctx = ctx)


  dv <- sapply(active_fields, function(.a) {

    # original filter list
    original_flist <- sch[[.a]]
    expect_equal(tiledb::nfilters(original_flist), 1)
    expect_equal(tiledb::tiledb_filter_type(original_flist[0]), "ZSTD")

    # Set new filter list
    sch[[.a]] <- flist

    updated_schema <- sch$schema()

    if (.a == "dim_namespace" || .a == "dim_key") {

      dims <- tiledb::dimensions(updated_schema)

      if (tiledb::name(dims[[1]]) == "namespace" && .a == "dim_namespace") {

        new_flist <- tiledb::filter_list(dims[[1]])
        expect_equal(tiledb::nfilters(new_flist), 1)
        expect_equal(tiledb::tiledb_filter_type(new_flist[0]), "RLE")

      }else if (tiledb::name(dims[[2]]) == "key" && .a == "dim_key") {

        new_flist <- tiledb::filter_list(dims[[2]])
        expect_equal(tiledb::nfilters(new_flist), 1)
        expect_equal(tiledb::tiledb_filter_type(new_flist[0]), "RLE")

      } else {
        stop('Dims are not in order')
      }


    } else {

      attrs <- tiledb::attrs(updated_schema)

      if (.a == "attr_hash" ) {
        attr_val <- attrs$hash
      } else if (.a == "attr_expires_at") {
        attr_val <- attrs$expires_at
      } else if (.a == "attr_notes") {
        attr_val <- attrs$notes
      } else {
        stop('attribute not found')
      }

      new_flist <- tiledb::filter_list(attr_val)
      expect_equal(tiledb::nfilters(new_flist), 1)
      expect_equal(tiledb::tiledb_filter_type(new_flist[0]), "RLE")

    }

    NULL
  })


})


test_that("SchemaData's schema is updated when properties/filters change", {

  ctx <- new_context()
  sch <- SchemaData$new(ctx = ctx, none_filter = FALSE)

  ##  cell_order, tile_order, capacity

  # capacity
  original_value <- sch$capacity
  expect_equal(tiledb::capacity(sch$schema()), original_value )
  sch$capacity <- 100
  expect_true(!identical(original_value, sch$capacity))
  expect_equal(tiledb::capacity(sch$schema()), 100)

  # cell_order
  original_value <- sch$cell_order
  expect_equal(tiledb::cell_order(sch$schema()), original_value)
  new_value <-  if (original_value == "ROW_MAJOR") "COL_MAJOR" else "ROW_MAJOR"
  sch$cell_order <- new_value
  expect_true(!identical(original_value, sch$cell_order))
  expect_equal(tiledb::cell_order(sch$schema()), new_value)

  # tile_order
  original_value <- sch$tile_order
  expect_equal(tiledb::tile_order(sch$schema()), original_value)
  new_value <-  if (original_value == "ROW_MAJOR") "COL_MAJOR" else "ROW_MAJOR"
  sch$tile_order <- new_value
  expect_true(!identical(original_value, sch$tile_order))
  expect_equal(tiledb::tile_order(sch$schema()), new_value)

  ## offsets, coords, validity filters

  schema_flists <- tiledb::filter_list(sch$schema())

  # offsets
  original_value <- sch$offsets_flist
  expect_equal(tiledb::tiledb_filter_type(original_value[0]), "ZSTD")

  flt <- tiledb::tiledb_filter("NONE", ctx = ctx)
  new_flist <- tiledb::tiledb_filter_list(flt, ctx = ctx)
  sch$offsets_flist <- new_flist

  offsets <- tiledb::filter_list(sch$schema())$offsets
  expect_equal(tiledb::nfilters(offsets), 1)
  expect_equal(tiledb::tiledb_filter_type(offsets[0]), "NONE")

  # coords
  original_value <- sch$coords_flist
  expect_equal(tiledb::tiledb_filter_type(original_value[0]), "ZSTD")

  sch$coords_flist <- new_flist

  coords <- tiledb::filter_list(sch$schema())$coords
  expect_equal(tiledb::nfilters(coords), 1)
  expect_equal(tiledb::tiledb_filter_type(coords[0]), "NONE")

  # validity
  original_value <- sch$validity_flist
  expect_equal(tiledb::tiledb_filter_type(original_value[0]), "RLE")

  sch$validity_flist <- new_flist

  validity <- tiledb::filter_list(sch$schema())$validity
  expect_equal(tiledb::nfilters(validity), 1)
  expect_equal(tiledb::tiledb_filter_type(validity[0]), "NONE")

  ## Dimensions and attributes

  active_fields <- c("dim_hash",
                     "attr_value")

  flt <- tiledb::tiledb_filter("RLE", ctx = ctx)
  flist <- tiledb::tiledb_filter_list(flt, ctx = ctx)


  dv <- sapply(active_fields, function(.a) {

    # original filter list
    original_flist <- sch[[.a]]
    expect_equal(tiledb::nfilters(original_flist), 1)
    expect_equal(tiledb::tiledb_filter_type(original_flist[0]), "ZSTD")

    # Set new filter list
    sch[[.a]] <- flist

    updated_schema <- sch$schema()

    if (.a == "dim_hash") {

      dims <- tiledb::dimensions(updated_schema)

      new_flist <- tiledb::filter_list(dims[[1]])
      expect_equal(tiledb::nfilters(new_flist), 1)
      expect_equal(tiledb::tiledb_filter_type(new_flist[0]), "RLE")

    } else {

      attr_val <- tiledb::attrs(updated_schema)$value

      new_flist <- tiledb::filter_list(attr_val)
      expect_equal(tiledb::nfilters(new_flist), 1)
      expect_equal(tiledb::tiledb_filter_type(new_flist[0]), "RLE")

    }

    NULL
  })


})


test_that("TileDBDriverSchemas", {

  expect_equal(names(TileDBDriverSchemas$active), c("SchemaKeys", "SchemaData"))

  expect_no_error(sch <- TileDBDriverSchemas$new())
  expect_r6_class(sch, "TileDBDriverSchemas")


  # No filter
  expect_no_error(sch <- TileDBDriverSchemas$new(none_filter = TRUE))
  flist <- sch$SchemaKeys$coords_flist[0]
  expect_equal(tiledb::tiledb_filter_type(flist), "NONE")

  # From existing driver
  uri <- file.path(withr::local_tempdir(), "test-driver")
  ctx <- new_context()
  driver_tiledb_create(uri, compression_level = NULL, context = ctx)

  expect_no_error(sch <- TileDBDriverSchemas$new(uri, ctx = ctx))
  flist <- sch$SchemaKeys$coords_flist[0]
  expect_equal(tiledb::tiledb_filter_type(flist), "NONE")

  ##  Errors are raised ---

  uri <- file.path(withr::local_tempdir(), "test-driver")
  expect_error(TileDBDriverSchemas$new(uri))

  # Not TileDB Group found at the given `uri` path.
  uri <- file.path(withr::local_tempdir(), "test-driver")
  grp <- R6.tiledb::tdb_group_create(uri, ctx = ctx)

  # Not a 'storr' driver at the  given `uri`.
  expect_error(TileDBDriverSchemas$new(grp$uri, ctx = ctx))

  # Invalid ctx.
  expect_error(TileDBDriverSchemas$new(ctx = "invalid"))

  })


test_that("driver_schemas()", {

  expect_no_error(sch <- driver_schemas())
  expect_r6_class(sch, "TileDBDriverSchemas")

  # No filter
  expect_no_error(sch <- driver_schemas(none_filter = TRUE))
  flist <- sch$SchemaKeys$coords_flist[0]
  expect_equal(tiledb::tiledb_filter_type(flist), "NONE")

  # From existing driver
  uri <- file.path(withr::local_tempdir(), "test-driver")
  ctx <- new_context()
  driver_tiledb_create(uri, compression_level = NULL, context = ctx)

  expect_no_error(sch <- driver_schemas(uri, ctx = ctx))
  flist <- sch$SchemaKeys$coords_flist[0]
  expect_equal(tiledb::tiledb_filter_type(flist), "NONE")

  # Invalid ctx.
  expect_error(driver_schemas(ctx = "invalid"))

})

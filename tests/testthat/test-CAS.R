test_that("Test 'CAS' object", {

  uri <- file.path(withr::local_tempdir(), "test-cas")
  expect_no_error(cas <- CAS$new(uri))

  expect_s3_class(cas, "CAS")
  expect_false(cas$exists())
  expect_equal(cas$mode, "CLOSED")
  expect_error(cas$hash_algorithm, label = "CAS object does not exist")
  expect_error(cas$size, label = "CAS object does not exist")
  expect_error(cas$members_instantiated, label = "CAS object does not exist")

})


test_that("'CAS' basic methods", {

  # NOTE: The following methods:
  # 'query_keys', 'filter_keys', 'update_keys'
  #  are tested in "TileDBDriver" unit tests

  uri <- file.path(withr::local_tempdir(), "test-cas")
  cas <- CAS$new(uri)

  # Invalid hash algo
  expect_error(cas$create(algo = "nope"))

  # Create CAS
  expect_invisible(cas$create())
  expect_true(cas$exists())
  expect_equal(cas$mode, "WRITE")

  # Check members
  expect_equal(cas$count_members(), 2)

  mdf <- cas$get_members_df()[, 1:2]
  expected <- as.data.frame(list(
    name = c("tbl_keys", "tbl_data"),
    type = c("ARRAY", "ARRAY")
  ))

  expect_equal(mdf, expected)

  # Check active fields
  expect_error(cas$hash_algorithm <- "invalid")
  expect_equal(cas$hash_algorithm, "md5")

  # Check type
  expect_equal(cas$get_metadata("type"), "storr")

  # Set new hash algo
  expect_no_error(cas$hash_algorithm <- "blake3")
  expect_equal(cas$hash_algorithm, "blake3")

  expect_error(cas$size <- "immutable")
  expect_s3_class(cas$size, "vfs_size")

  expect_error(cas$members_instantiated <- "immutable")

  # Open with member instantiation
  cas$close()

  expect_false(cas$members_instantiated)

  # A bit defensive (but checking nevertheless)
  expect_null(cas$members$tbl_keys$object)
  expect_null(cas$members$tbl_data$object)

  expect_no_error(cas$open("READ", instantiate = TRUE))

  expect_true(cas$members_instantiated)

  # A bit defensive (but checking nevertheless)
  expect_true(!is.null(cas$members$tbl_keys$object))
  expect_true(!is.null(cas$members$tbl_data$object))

  # Destroy CAS
  expect_no_error(cas$destroy())
  expect_false(cas$exists())

  })

test_that("'CAS' with custom schemas", {

  uri <- file.path(withr::local_tempdir(), "test-cas")
  ctx <- new_context()
  cas <- CAS$new(uri,ctx = ctx)

  dr_custom <- driver_schemas(none_filter = TRUE, ctx = ctx)

  # Set up a ZSTD filter with high compression
  flt <- tiledb::tiledb_filter("ZSTD", ctx = ctx)
  flt <- tiledb::tiledb_filter_set_option(flt,"COMPRESSION_LEVEL", 22)
  fl_list <- tiledb::tiledb_filter_list(flt, ctx = ctx)

  dr_custom$SchemaData$attr_value <- fl_list

  # Invalid hash algo
  expect_no_error(cas$create(driver_schemas = dr_custom))

  # Check created driver
  dr <- driver_schemas(uri, ctx = ctx)

  trg_filters <- data.frame(
    list(
      hash = c("NONE", "NONE"),
      value = c("ZSTD", "22"),
      coords = c("NONE", "NONE"),
      offsets = c("NONE", "NONE"),
      validity = c("NONE", "NONE")
    )
  )
  res_filters <- .schema_filters(dr$SchemaData$schema())

  expect_equal(res_filters, trg_filters)

  # Errors are raised
  uri <- file.path(withr::local_tempdir(), "test-cas")
  ctx <- new_context()
  cas <- CAS$new(uri,ctx = ctx)
  # `custom_driver` should be <TileDBDriverSchemas> class.
  expect_error(cas$create(driver_schemas = "invalid"))


})

test_that("$open() checks type is 'storr'", {
  uri <- file.path(withr::local_tempdir(), "test-group")

  grpuri <- tiledb::tiledb_group_create(uri, ctx = R6.tiledb::new_context())

  cas <- CAS$new(uri)
  expect_error(cas$open(), label = "Not a 'TileDB Storr'")

})

test_that("ctx is unique and not cached", {

  # NB: Verify that passing a not cached context via new_context
  # (not via tiledb_cxt) is not cached accidentally
  #
  uri <- file.path(withr::local_tempdir(), "test-cas")

  cfg <- tiledb::tiledb_config()
  cfg["vfs.s3.region"] <- "eu-north-1"
  ctx <- R6.tiledb::new_context(cfg)

  cas <- CAS$new(uri, ctx)
  cas$create()
  cas_cfg <- tiledb::config(cas$ctx)
  expect_equal(unname(cas_cfg["vfs.s3.region"]), "eu-north-1")

  # get config from cached ctx in tiledb package environment
  pkg_cfg <- tiledb::config(tiledb::tiledb_get_context())
  expect_equal(unname(pkg_cfg["vfs.s3.region"]), "")

})


test_that("'TileDBStorr'", {

  uri <- file.path(withr::local_tempdir(), "test-storr")
  sto <- storr_tiledb(uri, init = TRUE)

  expect_s3_class(sto, "TileDBStorr")
  expect_s3_class(sto, "storr")

  # hash tables initialised correctly
  expect_true(is.hashtab(sto$envir))
  expect_equal(numhash(sto$envir), 0)

  expect_true(is.hashtab(sto$envir_metadata))
  expect_equal(numhash(sto$envir_metadata), 0)

  # Test for TileDB driver
  expect_error(TileDBStorr$new("not_valid", "objects"))

  dr <- driver_tiledb(uri)
  sto2 <- TileDBStorr$new(dr, default_namespace = "objects")
  expect_s3_class(sto2$driver, "TileDBDriver")

  # driver is modified in place inside TileDBStorr,
  # because we instantiate members if needed
  expect_true(dr$is_open())
  expect_true(sto2$driver$members_instantiated)
  expect_true(dr$members_instantiated)


  dr$reopen()
  expect_false(dr$members_instantiated)

  # case: driver is opened but members are not cached,
  # TileDBStorr will have to reopen and instantiate members
  sto3 <- TileDBStorr$new(dr, default_namespace = "objects")
  expect_s3_class(sto3$driver, "TileDBDriver")

  expect_true(dr$is_open())
  expect_true(sto3$driver$members_instantiated)
  expect_true(dr$members_instantiated)

})


test_that("driver_tiledb", {

  uri <- file.path(withr::local_tempdir(), "test-driver")

  expect_error(driver_tiledb(uri),
               "'storr' not found, please create one.",
               class = "error")

  expect_no_error(dr <- driver_tiledb(uri, init = TRUE, keep_open = FALSE,
                                   hash_algorithm = "sha1"))

  expect_s3_class(dr, "TileDBDriver")
  expect_false(dr$is_open())
  expect_equal(dr$hash_algorithm, "sha1")
  expect_false(dr$members_instantiated)

})


test_that("driver_tiledb_create", {

  uri <- file.path(withr::local_tempdir(), "test-driver")

  expect_true(driver_tiledb_create(uri, hash_algorithm = "sha1"))

  expect_equal(driver_tiledb(uri)$hash_algorithm, "sha1")

})


test_that("storr_tiledb", {

  uri <- file.path(withr::local_tempdir(), "test-driver")

  expect_error(storr_tiledb(uri),
               "'storr' not found, please create one.",
               class = "error")

  expect_no_error(st <- storr_tiledb(uri,
                                     init = TRUE,
                                     keep_open = FALSE,
                                     hash_algorithm = "sha1"))

  expect_s3_class(st, "TileDBStorr")
  expect_true(st$driver$is_open())
  expect_true( st$driver$members_instantiated)
  expect_equal( st$driver$hash_algorithm, "sha1")

  rm(st)
  uri <- file.path(withr::local_tempdir(), "test-driver")

  driver_tiledb_create(uri)
  expect_error(storr_tiledb(uri, init = TRUE))

  expect_s3_class(storr_tiledb(uri, init = FALSE), "TileDBStorr")

})

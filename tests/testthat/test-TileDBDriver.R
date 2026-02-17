test_that("'TileDBDriver'", {

  uri <- file.path(withr::local_tempdir(), "test-driver")
  expect_no_error(dr <- TileDBDriver$new(uri))

  expect_s3_class(dr, "TileDBDriver")
  expect_false(dr$exists())
  expect_equal(dr$mode, "CLOSED")
  expect_equal(dr$type(), "tiledb")

  # public fields
  expect_false(dr$binary)
  expect_equal(dr$traits, list(accept = "string",
                               throw_missing = TRUE))

  # public fields are locked
  expect_error(dr$binary <- TRUE)
  expect_error(dr$traits <- "boo")

})


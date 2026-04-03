test_that("'TileDBStorr'", {


  TileDBStorrMock <- R6::R6Class(
    cloneable = FALSE,
    "Mocked",
    inherit = TileDBStorr,
    public = list(
      getdriver = function() {
        private$DRIVER
      }
    ),
  )


  uri <- file.path(withr::local_tempdir(), "test-storr")
  dr <- driver_tiledb(uri, init = TRUE)
  sto <- TileDBStorrMock$new(dr, "ns1")

  expect_s3_class(sto, c("Mocked", "TileDBStorr", "R6"), exact = TRUE)

  # hash tables initialised correctly
  expect_true(is.hashtab(sto$envir))
  expect_equal(numhash(sto$envir), 0)

  expect_true(is.hashtab(sto$envir_metadata))
  expect_equal(numhash(sto$envir_metadata), 0)

  # Test for TileDB driver
  expect_error(TileDBStorrMock$new("not_valid", "objects"))

  dr <- driver_tiledb(uri)
  sto2 <- TileDBStorrMock$new(dr, default_namespace = "objects")
  cl <- c("TileDBDriver", "CAS", "TileDBGroup", "TileDBObject", "R6")
  expect_s3_class(sto2$getdriver(), cl, exact = TRUE)

  # driver is modified in place inside TileDBStorr,
  # because we instantiate members if needed
  expect_true(dr$is_open())
  expect_true(sto$getdriver()$members_instantiated)
  expect_true(dr$members_instantiated)


  dr$reopen()
  expect_false(dr$members_instantiated)

  # case: driver is opened but members are not cached,
  # TileDBStorr will have to reopen and instantiate members
  sto3 <- TileDBStorrMock$new(dr, "objects")
  expect_s3_class(sto3$getdriver(), cl, exact = TRUE)

  expect_true(dr$is_open())
  expect_true(sto3$getdriver()$members_instantiated)
  expect_true(dr$members_instantiated)

})

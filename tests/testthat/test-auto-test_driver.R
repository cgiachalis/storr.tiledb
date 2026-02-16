test_that("Test 'driver_tiledb()' (auto via storr's test_driver)", {


  oldsize <- tiledb::get_allocation_size_preference()
  tiledb::set_allocation_size_preference(0.5 * 1024 * 1024)
  on.exit(tiledb::set_allocation_size_preference(oldsize))


  uri <- file.path(withr::local_tempdir(), "test-driver")


  .driver_create <- function(dr = NULL, ...) {

    arg <- list(...)

    if (is.null(dr)) {

      driver_tiledb_create(uri, hash_algorithm = arg$hash_algorithm)
      driver_tiledb(uri)

    } else {

      if (!is.null(arg$hash_algorithm)) {
        if (!identical(dr$hash_algorithm, arg$hash_algorithm)) {
          stop(ConfigError("hash_algorithm", dr$hash_algorithm, arg$hash_algorithm))
        }

      }
      dr
    }
  }

  storr::test_driver(.driver_create)

})

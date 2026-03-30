
test_that("storr_tiledb", {

  uri <- file.path(withr::local_tempdir(), "test-driver")

  expect_error(storr_tiledb(uri),
               "'storr' not found, please create one.",
               class = "error")

  expect_no_error(st <- storr_tiledb(uri,
                                     init = TRUE,
                                     keep_open = FALSE,
                                     hash_algorithm = "sha1"))

  expect_s3_class(st, c("TileDBStorr", "storr", "R6"), exact = TRUE)
  expect_true(st$driver$is_open())
  expect_true(st$driver$members_instantiated)
  expect_equal(st$driver$hash_algorithm, "sha1")

  rm(st)
  uri <- file.path(withr::local_tempdir(), "test-driver")

  driver_tiledb_create(uri)
  expect_error(storr_tiledb(uri, init = TRUE))

  expect_s3_class(storr_tiledb(uri, init = FALSE), c("TileDBStorr", "storr", "R6"), exact = TRUE)

})

# NB: methods for key expiration management is tested with TileDBDriver;
# Here, we test again those we expose to storr.

test_that("keys_with_expiration", {

  tiledb::set_allocation_size_preference(0.5 * 1024 * 1024)
  uri <- file.path(withr::local_tempdir(), "test-driver")
  sto <- storr_tiledb(uri, init = TRUE)

  keys <- c("a", "b", "c", "d")
  t0 <- Sys.time()
  expires_at <- c(t0, t0, as.POSIXct("2250-05-28"), as.POSIXct(NA))
  sto$mset(keys, 1:4, namespace = c("ns1", "ns2", "ns3", "ns4"), expires_at = expires_at)

  # keys with expiration
  expect_no_error(arrw <- sto$keys_with_expiration(NULL, datetimes = TRUE))
  expect_s3_class(arrw, c("data.table"))

  expect_equal(dim(arrw), c(3, 3))
  expect_equal(arrw$key, c("a", "b", "c"))
  expect_equal(colnames(arrw), c("namespace", "key", "expires_at"))

  # Without 'expires_at' column
  expect_no_error(arrw <- sto$keys_with_expiration(NULL, datetimes = FALSE))
  expect_s3_class(arrw, c("data.table"))

  expect_equal(dim(arrw), c(3, 2))
  expect_equal(arrw$key, c("a", "b", "c"))
  expect_equal(colnames(arrw), c("namespace", "key"))

})

test_that("expired_keys and has_expired_keys", {

  tiledb::set_allocation_size_preference(0.5 * 1024 * 1024)
  uri <- file.path(withr::local_tempdir(), "test-driver")
  sto <- storr_tiledb(uri, init = TRUE)

  keys <- c("a", "b", "c", "d")
  t0 <- Sys.time()
  expires_at <- c(t0, t0, as.POSIXct("2250-05-28"), as.POSIXct(NA))
  sto$mset(keys, 1:4, namespace = c("ns1", "ns2", "ns3", "ns4"), expires_at = expires_at)


  expect_no_error(arrw <- sto$expired_keys(NULL, datetimes = TRUE))
  expect_s3_class(arrw, c("data.table"))

  expect_equal(dim(arrw), c(2, 3))

  expect_equal(arrw$key, c("a", "b"))
  expect_equal(colnames(arrw), c("namespace", "key", "expires_at"))

  # Without 'expires_at' column
  expect_no_error(arrw <- sto$expired_keys(NULL, datetimes = FALSE))
  expect_s3_class(arrw, c("data.table"))

  expect_equal(dim(arrw), c(2, 2))
  expect_equal(arrw$key, c("a", "b"))
  expect_equal(colnames(arrw), c("namespace", "key"))

  # Test for expired keys
  expect_true(sto$has_expired_keys(NULL))
  expect_true(sto$has_expired_keys("ns1"))
  expect_false(sto$has_expired_keys("ns3"))
  expect_false(sto$has_expired_keys("ns4"))

})

test_that("clear_expired_keys", {

  tiledb::set_allocation_size_preference(0.5 * 1024 * 1024)
  uri <- file.path(withr::local_tempdir(), "test-driver")
  sto <- storr_tiledb(uri, init = TRUE)

  keys <- c("a", "b", "c", "d")
  t0 <- Sys.time()
  expires_at <- c(t0, t0, as.POSIXct("2250-05-28"), as.POSIXct(NA))
  sto$mset(keys, 1:4, namespace = c("ns1", "ns2", "ns3", "ns4"), expires_at = expires_at)


  # Clear all expired keys
  expect_invisible(bool <- sto$clear_expired_keys(NULL))
  expect_true(bool)

  # Test for expired keys
  expect_false(sto$has_expired_keys(NULL))

  # ----------------------------------------------------------------------------
  # Lets redo it again..

  uri <- file.path(withr::local_tempdir(), "test-driver")
  sto <- storr_tiledb(uri, init = TRUE)

  keys <- c("a", "b", "c", "d")
  t0 <- Sys.time()
  expires_at <- c(t0, t0, as.POSIXct("2250-05-28"), as.POSIXct(NA))
  sto$mset(keys, 1:4, namespace = c("ns1", "ns2", "ns3", "ns4"), expires_at = expires_at)

  # GC without clearing all expired keys
  expect_invisible(unused <- sto$gc())
  expect_equal(unused, character(0))

  # GC and clear all expired keys
  expect_invisible(unused <- sto$gc(clear_expired = TRUE))
  expect_length(unused, 2)

  # Test for expired keys
  expect_false(sto$has_expired_keys(NULL))

})

test_that("cache global option", {

  tiledb::set_allocation_size_preference(0.5 * 1024 * 1024)
  uri <- file.path(withr::local_tempdir(), "test-driver")
  sto <- storr_tiledb(uri, init = TRUE)

  sto$set("a", 1)

  withr::with_options(list(storr.tiledb.cache = FALSE), {
    sto$set("b", 2)
    res <- sto$get(c("a"))
    res <- sto$mget(c("a", "b"))
  })


  expect_equal(numhash(sto$envir), 1)
  expect_equal(numhash(sto$envir_metadata), 1)


  withr::with_options(list(storr.tiledb.cache = TRUE), {
    sto$set("b", 2)
    res <- sto$get(c("a"))
    res <- sto$mget(c("a", "b"))
  })

  expect_equal(numhash(sto$envir), 2)
  expect_equal(numhash(sto$envir_metadata), 2)

})
